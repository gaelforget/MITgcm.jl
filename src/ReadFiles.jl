

"""
    scan_rundir(pth::String)

Scan a MITgcm run directory and standard output text file 
("output.txt" or "STDOUT.0000") and return a NamedTuple of
collected information (various formats)

Initially, the output looked like `(grid=gr,packages=pac,params_time=par1,params_grid=par2,completed=co)`
"""
function scan_rundir(pth::String)
    #1 standard output
    filout=joinpath(pth,"output.txt")
    !isfile(filout) ? filout=joinpath(pth,"STDOUT.0000") : nothing
    if isfile(filout)
        stdout=scan_stdout(filout)
    else
        stdout=missing
    end
    return stdout
end

"""
    scan_stdout(filout::String)

Scan a MITgcm standard output text file ("output.txt" or "STDOUT.0000") and return a NamedTuple of collected information (various formats).

- packages : report of packages being compiled and used
- params_time : initial time, model duation, output frequency, etc
- params_grid : type of grid (Curvilinear, Cartesian, ...) and array sizes
- params_files : type of output (use_mdsio, use_mnc) and array size (ioSize)
- completed : true / false depending on the end of standard output file (filout)
"""
function scan_stdout(filout::String)
    pth=dirname(filout)

    tmp=readlines(filout)
    #1.0 run completed
    co = tmp[end]=="PROGRAM MAIN: Execution ended Normally"

    ll = findall(occursin.("======================================================",tmp))
    #1.1 grid
    l0 = findall(occursin.("Computational Grid Specification",tmp))[1]
    l1 = minimum(ll[findall(ll.>l0+2)])
    gr = tmp[l0+2:l1]
    #1.2 packages
    l0 = findall(occursin.("PACKAGES_BOOT: On/Off package Summary",tmp))[1]
    l1 = findall(occursin.("PACKAGES_BOOT: End of package Summary",tmp))[1]
    pac = tmp[l0:l1]
    #1.3 parameters (time, grid)
    l0 = findall(occursin.("Time stepping paramters",tmp))[1]
    l1 = findall(occursin.("Gridding paramters",tmp))[1]
    l2 = findall(occursin.("End of Model config. summary",tmp))[1]

    par1=Dict()
    list1=["deltaTClock","nIter0","nTimeSteps","nEndIter",
            "pChkPtFreq","dumpFreq","monitorFreq"]
    for i in list1
        lx = findall(occursin.(i,tmp))
        lx = lx[findall((lx.>l0).*(lx.<l1))[1]]
        par1[Symbol(i)] = parse(Float64,tmp[lx+1][20:end])
    end
    par1=(; zip(Symbol.(keys(par1)), values(par1))...)

    par2=Dict()
    list1=["usingCartesianGrid","usingCylindricalGrid","usingSphericalPolarGrid","usingCurvilinearGrid"]
    for i in list1
        lx = findall(occursin.(i,tmp))
        lx = lx[findall((lx.>l1).*(lx.<l2))[1]]
        par2[Symbol(i)] = strip(tmp[lx+1][20:end])=="T"
    end

	b=["nPx","nPy","nSx","nSy","sNx","sNy","Nx","Ny","Nr"]
	for bb in b
		i1=findall(occursin.(bb,gr))[1]
		tmp=parse(Int,split(split(gr[i1][20:end],";")[1],"=")[2])
		par2[Symbol(bb)] = tmp
	end

    par2=(; zip(Symbol.(keys(par2)), values(par2))...)

    #1.4 monitors

    #2 output files

    #2.1 type (mdsio or mnc?) and overall size (ioSize) of output 
    tst_mdsio = !isempty(filter(x -> occursin("XC",x), readdir(pth)))
    pth_mnc=joinpath(pth,"mnc_test_0001")
    tst_mnc = isdir(pth_mnc)
    ioSize=Array{Any}(undef,1)
    if tst_mdsio
        tmp=read_mdsio(pth,"XC")
        ioSize[1]=size(tmp)
    end
    if tst_mnc
        tmp=MITgcmTools.read_mnc(pth_mnc,"grid","XC")
        ioSize[1]=size(tmp)
    end

    par3=(use_mdsio=tst_mdsio,use_mnc=tst_mnc,ioSize=ioSize[1])

    #2.1 pickups
    #2.2 diags
    #2.3 snapshots
    #2.4 tave
    #2.5 mnc

    return (packages=pac,params_time=par1,params_grid=par2,params_files=par3,completed=co)
end

"""
    read_nctiles(fileName,fldName,mygrid)

Read model output from NCTiles file and convert to MeshArray instance.
```
mygrid=GridSpec("LatLonCap")
fileName="nctiles_grid/GRID"
Depth=read_nctiles(fileName,"Depth",mygrid)
hFacC=read_nctiles(fileName,"hFacC",mygrid)
hFacC=read_nctiles(fileName,"hFacC",mygrid,I=(:,:,1))
```
"""
function read_nctiles(fileName::String,fldName::String,mygrid::gcmgrid;
    I::Union{Missing,Tuple{Colon,Colon,Vararg{Union{Colon,Integer}}}}=missing)

    if (mygrid.class!="LatLonCap")||(mygrid.ioSize!=[90 1170])
        error("non-llc90 cases have not yet been tested with read_nctiles")
    end

    pth0=dirname(fileName)

    nam=split(fileName,"/")[end]
    isempty(nam) ? nam=split(fileName,"/")[end-1] : nothing
    occursin(".nctiles",nam) ? nam=nam[1:end-8] : nothing

    isdir(fileName) ? pth1=fileName : pth1=pth0
    lst=readdir(pth1)
    lst=lst[findall(occursin.(nam,lst).*occursin.(".nc",lst))]

    fileIn=joinpath(pth1,lst[1])
    fileRoot=fileIn[1:end-8]
    ntile=MITgcmTools.ncgetatt(fileIn,"Global","ntile")

    x = MITgcmTools.ncread(fileIn,fldName)
    s = [size(x,i) for i in 1:ndims(x)]
    n=length(size(x))
    start=ones(Int,n)
    count=-ones(Int,n)

    ~ismissing(I) && length(I)!=n ? error("ncdims v I inconsistency") : nothing
    if ~ismissing(I)
        k=findall([!isa(I[i],Colon) for i=1:length(I)])
        j=[I[i] for i in k]
        start[k]=j
        count[k].=1
        s[k].=1
    end

    f=Array{Float64, n}[]
    m0=[0]
    for ff in 1:mygrid.nFaces
        (ni,nj)=Int.(mygrid.fSize[ff]./s[1:2])
        nn=ni*nj
        i0=(mod1.(1:nn,ni).-1)*s[1]
        j0=div.(0:nn-1,ni)*s[2]

        #f0=Array{Float64}(undef,mygrid.fSize[ff]...,s[3:end]...)
        f0=fill(NaN,mygrid.fSize[ff]...,s[3:end]...)
        n0=m0[1]
        for n in 1:nn
            fileIn=@sprintf("%s.%04d.nc",fileRoot,n+n0)
            if isfile(fileIn) #skip if no file / blank tile
                x = ncread(fileIn,fldName,start,count)
                i=collect(1:s[1]) .+ i0[n]
                j=collect(1:s[2]) .+ j0[n]
                f0[i,j,:,:]=x[:,:,:,:]
            end
            m0[1]+=1
        end
        #f0[findall(isnan.(f0))].=0.0
        push!(f,f0)
    end

    fld=MeshArray(mygrid,f)
    return fld
end

"""
    findtiles(ni::Int,nj::Int,mygrid::gcmgrid)
    findtiles(ni::Int,nj::Int,grid::String="LatLonCap",GridParentDir="../inputs/GRID_LLC90/")

Return a `MeshArray` map of tile indices, `mytiles["tileNo"]`, for tile
size `ni,nj` and extract grid variables accordingly.
"""
function findtiles(ni::Int,nj::Int,mygrid::gcmgrid)
    mytiles = OrderedDict()

    GridVariables=GridLoad(mygrid)

    mytiles["nFaces"]=mygrid.nFaces;
    mytiles["ioSize"]=mygrid.ioSize;

    XC=GridVariables.XC;
    YC=GridVariables.YC;
    XC11=similar(XC); YC11=similar(XC);
    XCNINJ=similar(XC); YCNINJ=similar(XC);
    iTile=similar(XC); jTile=similar(XC); tileNo=similar(XC);
    tileCount=0;
    for iF=1:XC11.grid.nFaces
        face_XC=XC.f[iF]; face_YC=YC.f[iF];
        for jj=Int.(1:size(face_XC,2)/nj);
            for ii=Int.(1:size(face_XC,1)/ni);
                tileCount=tileCount+1;
                tmp_i=(1:ni).+ni*(ii-1)
                tmp_j=(1:nj).+nj*(jj-1)
                tmp_XC=face_XC[tmp_i,tmp_j]
                tmp_YC=face_YC[tmp_i,tmp_j]
                XC11.f[iF][tmp_i,tmp_j].=tmp_XC[1,1]
                YC11.f[iF][tmp_i,tmp_j].=tmp_YC[1,1]
                XCNINJ.f[iF][tmp_i,tmp_j].=tmp_XC[end,end]
                YCNINJ.f[iF][tmp_i,tmp_j].=tmp_YC[end,end]
                iTile.f[iF][tmp_i,tmp_j]=collect(1:ni)*ones(Int,1,nj)
                jTile.f[iF][tmp_i,tmp_j]=ones(Int,ni,1)*collect(1:nj)'
                tileNo.f[iF][tmp_i,tmp_j]=tileCount*ones(Int,ni,nj)
            end
        end
    end

    mytiles["tileNo"] = tileNo;
    mytiles["XC"] = XC;
    mytiles["YC"] = YC;
    mytiles["XC11"] = XC11;
    mytiles["YC11"] = YC11;
    mytiles["XCNINJ"] = XCNINJ;
    mytiles["YCNINJ"] = YCNINJ;
    mytiles["iTile"] = iTile;
    mytiles["jTile"] = jTile;

    return mytiles

end

findtiles(ni::Int,nj::Int,GridName::String="LatLonCap",GridParentDir="../inputs/GRID_LLC90/") = findtiles(ni,nj,GridSpec(GridName,GridParentDir))

## read_bin function with full list of argument

"""
    read_bin(fil::String,kt::Union{Int,Missing},kk::Union{Int,Missing},prec::DataType,mygrid::gcmgrid)

Read model output from binary file and convert to MeshArray. Other methods:

```
read_bin(fil::String,prec::DataType,mygrid::gcmgrid)
read_bin(fil::String,mygrid::gcmgrid)
```
"""
function read_bin(fil::String,kt::Union{Int,Missing},kk::Union{Int,Missing},prec::DataType,mygrid::gcmgrid)

  if ~ismissing(kt);
    error("non-empty kt option not implemented yet");
  end;

  if ~ismissing(kk);
    error("non-empty kk option not implemented yet");
  end;

  (n1,n2)=mygrid.ioSize

  if prec==Float64;
    reclen=8;
  else;
    reclen=4;
  end;
  tmp1=stat(fil);
  n3=Int64(tmp1.size/n1/n2/reclen);

  fid = open(fil);
  fld = Array{prec,1}(undef,(n1*n2*n3));
  read!(fid,fld);
  fld = hton.(fld);
  close(fid)

  n3>1 ? s=(n1,n2,n3) : s=(n1,n2)
  v0=reshape(fld,s);

  convert2gcmfaces(v0,mygrid)

end

## read_bin with reduced list of argument

# read_bin(fil::String,prec::DataType,mygrid::gcmgrid)
function read_bin(fil::String,prec::DataType,mygrid::gcmgrid)
  read_bin(fil,missing,missing,prec,mygrid::gcmgrid)
end

# read_bin(fil::String,mygrid::gcmgrid)
function read_bin(fil::String,mygrid::gcmgrid)
  read_bin(fil,missing,missing,mygrid.ioPrec,mygrid::gcmgrid)
end

## read_bin with alternative arguments

# read_bin(fil::String,x::MeshArray)
function read_bin(fil::String,x::MeshArray)
  read_bin(fil,missing,missing,eltype(x),x.grid::gcmgrid)
end

# read_bin(tmp::Array,mygrid::gcmgrid)
function read_bin(tmp::Array,mygrid::gcmgrid)
  convert2gcmfaces(tmp,mygrid)
end

# read_bin(tmp::Array,x::MeshArray)
function read_bin(tmp::Array,x::MeshArray)
  convert2gcmfaces(tmp,x.grid)
end

"""
    read_meta(metafile)

Read a `MITgcm` metadata file, parse it, and return as a NamedTuple

```
p="./hs94.cs-32x32x5/run/"
meta=read_meta(p*"surfDiag.0000000020.002.001.meta")
pairs(meta)
meta.dimList
```
"""
function read_meta(metafile)

    metafile[end-4:end]==".data" ? fil=metafile[1:end-5]*".meta" : fil=metafile[:]
    fil[end-4:end]!==".meta" ? error("inconsistent file name") : nothing

    meta = read(fil,String)
    meta = split(meta,";\n")
    meta = meta[isempty.(meta).==false]
    meta = replace.(meta,Ref(",\n"=>";"))
    meta = replace.(meta,Ref("\n"=>""))
    meta = replace.(meta,Ref("}"=>"]"))
    meta = replace.(meta,Ref("{"=>"["))
    #meta = replace.(meta,Ref(" "=>""))
    meta = replace.(meta,Ref("'"=>"\""))
    meta = replace.(meta,Ref(";]"=>"]"))
    meta = replace.(meta,Ref(","=>" "))

    meta = split.(meta,"=")
    meta = [[replace(x[1]," "=>"") x[2]] for x in meta]

    metaDict = OrderedDict{String,Any}(m[1] => m[2] for m in meta)

    for k in keys(metaDict)
        val = eval(Meta.parse(metaDict[k]))
        if isa(val[1],String)
            val = replace.(val,Ref(" "=>""))
        end
        if length(val) == 1
            val = val[1]
        end
        metaDict[k] = val
    end
    metaDict["dataprec"] = titlecase(metaDict["dataprec"])

    meta = (; zip(Symbol.(keys(metaDict)),values(metaDict))...)
    return meta

end

"""
    read_namelist(fil)

Read a `MITgcm` namelist file, parse it, and return as a NamedTuple
- assumes a single variable definition per line

```
using MITgcmTools
testreport("advect_xy")
fil=joinpath(MITgcm_path[1],"verification","advect_xy","run","data")
namelist=read_namelist(fil)
```
"""
function read_namelist(fil)

    meta = read(fil,String)

    # extract parameters from meta. Assumes specific format 
    meta = split(meta,"\n")
    # get rid of empty lines
    meta = meta[findall((!isempty).(meta))]
    # get rid of comments
    meta = meta[findall(first.(meta).!=='#')]
    groups = meta[findall(occursin.('&',meta) .& (length.(strip.(meta)) .> 1))] # groups of params start with a & 
    
    # remove whitespace and & 
    trimmed_groups = []
    for g in groups
        g = lstrip(g)
        g = rstrip(g)
        g = replace(g, "&" => "")
        push!(trimmed_groups, g)
    end
    # instantiate params dictionary 
    # if groups isnt made right, params won't be created right 
	params = fill(OrderedDict(),length(groups))

    # for each param under each group, do the key/val split
	for i in 1:length(groups)
		ii=1+findall(occursin.(String(groups[i]),meta))[1]
		i1=ii
		tmp0=OrderedDict()
        k0=[:unknown]
        # THIS LINE IS LOOKING FOR A CLOSEING & or a single backslash  \
        while !occursin('&',meta[ii]) && !(occursin('/', meta[ii]) && length(strip(meta[ii]))==1)
			if occursin('=',meta[ii])
				tmp1=split(meta[ii],'=')
                k0[1]=Symbol(strip(tmp1[1]))
				tmp2=split(tmp1[2],',')
                if length(tmp2)==2
                    tmp0[k0[1]]=strip(tmp2[1])
                else
                    tmp0[k0[1]]=strip(tmp1[2])
                end
            else
                try
                    tmp0[k0[1]]=tmp0[k0[1]]*","*strip(meta[ii])
                catch
                    println("ignoring line -- unclear why ...", meta[ii])
                end
			end
			ii += 1
		end
        for ii in keys(tmp0)
            tmp0[ii]=parse_param(tmp0[ii])
        end
		params[i]=tmp0			
	end
		
    return MITgcm_namelist(Symbol.(trimmed_groups),params)
end

"""
    parse_param(p1)

Parse namelist parameter and return in corresponding type
"""
function parse_param(p1)
	p2=missing
	if p1==".TRUE."||p1==".true."
		p2=true
	elseif p1==".FALSE."||p1==".false."
		p2=false
	else
        # drop surrounding quotes IF its not a list 
        if first(p1)=='\''&&!occursin(',',p1)
			p2=p1[2:end-1]
        elseif occursin('.',p1) || occursin('e', p1) || occursin('E', p1)
			try
				p2=parse(Float64,p1)
			catch
				p2=p1
			end
		else
			try
				p2=parse(Int,p1)
			catch
				p2=p1
			end
		end
	end
    if isa(p2,AbstractString)
        if occursin(',',p2)
            p2=strip.(split(p2,','))
            p2=p2[findall( (!isempty).(p2) )]
			p2=[parse_param(p3) for p3 in p2]
        end
    end
	return p2
end

"""
    write_namelist(fil)

Save a `MITgcm` namelist file. In the example below, one is read from file, modified, and then saved to a new file using write_namelist.

```
using MITgcmTools
fil=joinpath(MITgcm_path[1],"verification","advect_xy","run","data")
nml=read_namelist(fil)
write_namelist(fil*"_new",namelist)
```

or 

```
nml=read(fil,MITgcm_namelist())
write(fil*"_new",nml)
```
"""
function write_namelist(fil,namelist)
    # TODO: if this function fails, writes an empty file
	fid = open(fil, "w")
	for jj in 1:length(namelist.groups)
        ii=namelist.groups[jj]
		tmpA=namelist.params[jj] 
		params=(; zip(keys(tmpA),values(tmpA))...)
			
        txt=fill("",length(params))
        for i in 1:length(params)
            x=params[i]
            y=missing
            isa(x,Bool)&&x==true ? y=".TRUE." : nothing
            isa(x,Bool)&&x==false ? y=".FALSE." : nothing
            # if x is an array, and it is filled with all AbstractStrings AND non of the elements contain *
            # this is so we don't put quotes around lists that contain *
            write_quotes = true
            if isa(x,Array)&&(eltype(x)<:AbstractString)
                for idx in 1:length(x)
                    if occursin('*', x[idx])
                        write_quotes = false
                    end
                end
            end
            if isa(x,Array)&&(eltype(x)<:AbstractString)&&write_quotes
                tmpy=[""]
                [tmpy[1]*="'"*x[ii]*"', \n " for ii in 1:length(x)]
                y=tmpy[1][1:end-4]
            elseif isa(x,Array)
                tmpy=[""]
                [tmpy[1]*=string(x[ii])*", \n " for ii in 1:length(x)]
                y=tmpy[1][1:end-4]
            end
            ismissing(y)&&isa(x,AbstractString)&&(!occursin('*',x)) ? y="'$x'" : nothing
            ismissing(y) ? y="$x" : nothing
            y[end]==',' ? y=y[1:end-1] : nothing
            txt[i]=y
        end
			
		txtparams=[" $(keys(params)[i]) = $(txt[i]),\n" for i in 1:length(params)]

		write(fid," &$(ii)\n")
		[write(fid,txtparams[i]) for i in 1:length(txtparams)]
		write(fid," &\n")
		write(fid," \n")
	end
	close(fid)
end

"""
    read_meta(pth::String,fil::String)

Read a `MITgcm` metadata files, parse them, and return as an array of NamedTuple

```
p="./hs94.cs-32x32x5/run/"
meta=read_meta(p,"surfDiag.0000000020")
pairs(meta[end])
[meta[i].dimList for i in 1:length(meta)]
```
"""
function read_meta(pth::String,fil::String)
    f=readdir(pth)
    kk=findall(occursin.(fil,f).*occursin.(".meta",f))
    [read_meta(pth*f[k]) for k in kk]
end

"""
    read_mdsio(fil::String)

Read a single `MITgcm` MDSIO-type file (".data" binary + ".meta" text pair), and return as an Array

```
p="./hs94.cs-32x32x5/run/"
x=read_mdsio(p*"surfDiag.0000000020.002.001.data")
y=read_mdsio(p*"pickup.ckptA.002.001.data")
z=read_mdsio(p*"T.0000000000.002.001.data")
```
"""
function read_mdsio(fil::String)
    m=read_meta(fil)
    T=eval(:($(Symbol(m.dataprec))))

    s=m.dimList[:,3]-m.dimList[:,2].+1
    isempty(findall(keys(m).==:nrecords)) ? n=1 : n=m.nrecords

    fid = open(fil)
    x = Array{T,1}(undef,(n*prod(s)))
    read!(fid,x)
    x = hton.(x)
    close(fid)

    n>1 ? s=Tuple([s;n]) : s=Tuple(s)
    x=reshape(x,s)

    return x
end

"""
    read_mdsio(pth::String,fil::String)

Read a set of `MITgcm` MDSIO-type files (".data" binary + ".meta" text pair), combine, and return as an Array.
Unlike `read_mdsio(fil::String)` where `fil` is one complete file name, this method will search within `pth` 
for files that start with `fil`.

```
p="./hs94.cs-32x32x5/run/"
x=read_mdsio(p,"surfDiag.0000000020")
y=read_mdsio(p,"pickup.ckptA")
z=read_mdsio(p,"T.0000000000")
```
"""
function read_mdsio(pth::String,fil::String)
    f=readdir(pth)
    kk=findall(occursin.(fil,f).*occursin.(".data",f))
    f=f[kk]
    kk=findall([ (f[i][1:length(fil)]==fil)*(f[i][1+length(fil)]=='.') for i in 1:length(f)])

    m=[read_meta(joinpath(pth,f[k])) for k in kk]
    T=eval(:($(Symbol(m[1].dataprec))))

    m[1].nrecords>1 ? s=Tuple([m[1].dimList[:,1];m[1].nrecords]) : s=Tuple(m[1].dimList[:,1])
    x = Array{T,length(s)}(undef,s)
    
    for k=1:length(m)
        ii=m[k].dimList[1,2]:m[k].dimList[1,3]
        jj=m[k].dimList[2,2]:m[k].dimList[2,3]
        x[ii,jj,:,:]=read_mdsio(joinpath(pth,f[kk[k]]))
    end

    return x
end

#for the gcmgrid interface in MeshArrays.jl
function read_mdsio(fil::String,x::MeshArray)
    bas=split(basename(fil),'.')[1]
    xx=read_mdsio(dirname(fil),string(bas))
    return x.grid.read(xx,x)
end

#for the gcmgrid interface in MeshArrays.jl
read_mdsio(xx::Array,x::MeshArray) = MeshArrays.read(xx::Array,x::MeshArray)

"""
    read_mnc(pth::String,fil::String,var::String)

Read variable `var` from a set of `MITgcm` MNC-type files (netcdf files), combine, and 
return as an Array. This method will search within `pth` for files that start with `fil`.
"""
function read_mnc(pth::String,fil::String,var::String)
    lst=readdir(pth)
    lst=lst[findall(occursin.(fil,lst).*occursin.(".nc",lst))]

    fil=joinpath(pth,lst[1])
    ncfile=MITgcmTools.NetCDF.open(fil)
    ncatts=ncfile.gatts

    v = MITgcmTools.NetCDF.open(fil, var)
    if haskey(v.atts,"coordinates")
        has_RC=occursin("RC",v.atts["coordinates"])
        has_iter=occursin("iter",v.atts["coordinates"])
    else
        has_RC=false
        has_iter=false
    end

    if has_RC*has_iter
        s=(ncatts["Nx"],ncatts["Ny"],Int64(v.dim[3].dimlen),Int64(v.dim[4].dimlen))
    elseif has_RC|has_iter
        s=(ncatts["Nx"],ncatts["Ny"],Int64(v.dim[3].dimlen))
    else
        s=(ncatts["Nx"],ncatts["Ny"])
    end

    T = Float64
    x = Array{T,length(s)}(undef,s)

    for f in lst
        fil=joinpath(pth,f)
        ncfile=MITgcmTools.NetCDF.open(fil)
        ncatts=ncfile.gatts
        b=(ncatts["bi"],ncatts["bj"])
        s=(ncatts["sNx"],ncatts["sNy"])
        ii=(b[1]-1)*s[1] .+ collect(1:s[1])
        jj=(b[2]-1)*s[2] .+ collect(1:s[2])
        v = MITgcmTools.NetCDF.open(fil, var)
        if has_RC*has_iter
            x[ii,jj,:,:]=v[:,:,:,:]
        elseif has_RC|has_iter
            x[ii,jj,:]=v[:,:,:]
        elseif has_RC|has_iter
            x[ii,jj]=v[:,:]
        end
    end

    x
end

"""
    GridLoad_mnc(γ::gcmgrid)

Load grid variabes (XC, YC, Depth) model run directory (`joinpath(rundir,"mnc_test_0001")`).   
"""
function GridLoad_mnc(γ::gcmgrid)
    pth=joinpath(γ.path,"mnc_test_0001")
    XC=read_mnc(pth,"grid","XC")
    YC=read_mnc(pth,"grid","YC")
    Depth=read_mnc(pth,"grid","Depth")
    tmp=MeshArray(γ,γ.ioPrec)
    (XC=γ.read(XC,tmp), YC=γ.read(YC,tmp), Depth=γ.read(Depth,tmp))
end

"""
    GridLoad_mnc(myexp::MITgcm_config)

Load grid variables (XC, YC, Depth) from model run directory (`joinpath(rundir,"mnc_test_0001")`).
"""
function GridLoad_mnc(myexp::MITgcm_config)
    if isdir(joinpath(myexp.folder,string(myexp.ID),"run"))
        rundir=joinpath(myexp.folder,string(myexp.ID),"run")
    else
        pth=joinpath(MITgcm_path[1],"verification")
        rundir=joinpath(pth,myexp.configuration,"run")
    end
    GridLoad_mnc(rundir)
end

"""
    GridLoad_mnc(rundir::String)

Load grid variables (XC, YC, Depth) from model run directory (`joinpath(rundir,"mnc_test_0001")`).
"""
function GridLoad_mnc(rundir::String)

    data_mnc=joinpath(rundir,"data.mnc")
    if isfile(data_mnc)
        nml=read_namelist(data_mnc)
        pth=joinpath(rundir,nml.params[1][:mnc_outdir_str]*"0001")
    else
        pth=joinpath(rundir,"mnc_test_0001")
    end

	tmp=MITgcmTools.read_mnc(pth,"grid","XC")
    exps_ioSize=size(tmp)
    elty=eltype(tmp)
    sc=MITgcmTools.scan_rundir(rundir)
    #
    if sc.params_grid.usingCurvilinearGrid&&(exps_ioSize==(192,32))
        γ=gcmgrid(rundir,"CubeSphere",6,fill((32,32),6),[32 32*6],elty, read, write)
        c=cube2compact
    elseif sc.params_grid.usingCurvilinearGrid&&(exps_ioSize==(384,16))
        γ=gcmgrid(rundir,"CubeSphere",6,fill((32,32),6),[32 32*6],elty, read, write)
        function c(tmp)
            tmp2=reshape(tmp,(32,12,16))
            tmp3=Array{eltype(tmp2)}(undef,32*6,32)
            for i in 1:6
                ii=collect((i-1)*32 .+(1:32))
                tmp3[ii,1:16]=tmp2[:,2*i-1,:]
                tmp3[ii,17:32]=tmp2[:,2*i,:]
            end
            cube2compact(tmp3)
        end
    else
        s1=exps_ioSize
        s2=[s1[1] s1[2]]
        γ=gcmgrid(rundir,"PeriodicDomain",1,fill(s1,1),s2,elty, read, write)
        c=(x->x)
    end
    #
    pth=joinpath(γ.path,"mnc_test_0001")
    XC=c(read_mnc(pth,"grid","XC"))
    YC=c(read_mnc(pth,"grid","YC"))
    Depth=c(read_mnc(pth,"grid","Depth"))
    tmp=MeshArray(γ,γ.ioPrec)
    (XC=γ.read(XC,tmp), YC=γ.read(YC,tmp), Depth=γ.read(Depth,tmp))
end

"""
    GridLoad_mdsio(myexp::MITgcm_config)

Load grid variables (XC, YC, Depth, etc) from model run directory (`rundir`).
"""
function GridLoad_mdsio(myexp::MITgcm_config)
    if isdir(joinpath(myexp.folder,string(myexp.ID),"run"))
        rundir=joinpath(myexp.folder,string(myexp.ID),"run")
    else
        pth=joinpath(MITgcm_path[1],"verification")
        rundir=joinpath(pth,myexp.configuration,"run")
    end
    GridLoad_mdsio(rundir)
end

"""
    GridLoad_mdsio(rundir::String)

Load grid variables (XC, YC, Depth, etc) from model run directory (`rundir`).
"""
function GridLoad_mdsio(rundir::String)
    tmp=read_mdsio(rundir,"XC")
    exps_ioSize=size(tmp)
    elty=eltype(tmp)
    sc=MITgcmTools.scan_rundir(rundir)
    #
    if sc.params_grid.usingCurvilinearGrid
        readcube(xx::Array,x::MeshArray) = read_mdsio(cube2compact(xx),x)
        readcube(fil::String,x::MeshArray) = read_mdsio(fil::String,x::MeshArray)
        writecube(x::MeshArray) = compact2cube(write(x))
        writecube(fil::String,x::MeshArray) = write(fil::String,x::MeshArray)    
        γ=gcmgrid(rundir,"CubeSphere",6,fill((32,32),6),[32 32*6],elty, readcube, writecube)
    else
        s1=exps_ioSize
        s2=[s1[1] s1[2]]
        γ=gcmgrid(rundir,"PeriodicDomain",1,fill(s1,1),s2,elty, read_mdsio, write)
    end
    Γ=GridLoad(γ;option="full")
end

"""
    read_available_diagnostics(fldname::String; filename="available_diagnostics.log")

Get the information for a particular variable `fldname` from the 
`available_diagnostics.log` text file generated by `MITgcm`.
"""
function read_available_diagnostics(fldname::String; filename="available_diagnostics.log")
    availdiags = readlines(filename)
    line = availdiags[findall(occursin.(@sprintf("%-8s",fldname),availdiags))[1]]

    line = split(line,'|')
    line = lstrip.(rstrip.(line))

    diagInfo = OrderedDict([
    "diagNum" => parse(Int,line[1]),
    "fldname" => line[2],
    "levs" => parse(Int,line[3]),
    "mate" => line[4],
    "code" => line[5],
    "units" => line[6],
    "title" => line[7]
    ])

end

"""
    read_flt(dirIn::String,prec::DataType)

Read displacements from MITgcm/pkg/flt output file into a DataFrame.
"""
function read_flt(dirIn::String,prec::DataType)

   #load the data into one array
   prec==Float64 ? reclen=8 : reclen=4
   n1=13

   filIn="float_trajectories"
   tmp1=readdir(dirIn)
   tmp1=filter(x -> occursin(filIn,x),tmp1)
   filList=filter(x -> occursin(".data",x),tmp1)
   #hack:
   #filList=filter(x -> occursin("002.002.data",x),tmp1)
   nf=length(filList)

   n2=Array{Int,1}(undef,nf)
   for ff=1:nf
      fil=dirIn*filList[ff]
      #println(fil)
      tmp=stat(fil)
      n2[ff]=Int64(tmp.size/n1/reclen)-1
   end

   arr = Array{prec,2}(undef,(n1+1,sum(n2)));
   ii=0;
   #@softscope for ff=1:nf
   for ff=1:nf
      fil=dirIn*filList[ff]
      fid = open(fil)
      tmp = Array{prec,2}(undef,(n1,n2[ff]+1))
      read!(fid,tmp)
      arr[1:n1,ii+1:ii+n2[ff]] = hton.(tmp[:,2:n2[ff]+1])
      arr[n1+1,ii+1:ii+n2[ff]] .= ff
      ii=ii+n2[ff]
   end

   #sort the whole dataset by time
   jj = sort!([1:ii;], by=i->arr[2,i]); arr=arr[:,jj];
   #arr = sort!(arr, dims=2, by=i->arr[2,i]);

   #nfloats=Int(maximum(arr[1,:]))
   #npoints=counts(Int.(arr[1,:]))

   #reformat data as a DataFrame
   df=DataFrame()
   df.ID=Int.(arr[1,:])
   df.time=Int.(arr[2,:])
   df.lon=arr[3,:]
   df.lat=arr[4,:]
   df.dep=arr[5,:]
   if true
      df.i=arr[6,:]
      df.j=arr[7,:]
      df.k=arr[8,:]
      df.etaN=arr[9,:]
      df.uVel=arr[10,:]
      df.vVel=arr[11,:]
      df.theta=arr[12,:]
      df.salt=arr[13,:]
      df.tile=Int.(arr[14,:])
   end

#   nfloats=maximum(df.ID);
#   nsteps=maximum(counts(df.ID));
#   println("# floats=$nfloats")
#   println("# steps=$nsteps")

   return df
end

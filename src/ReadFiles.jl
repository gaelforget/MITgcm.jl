
function read_mnc end
function read_nctiles end

"""
    scan_run_dir(pth::String)

Scan a MITgcm run directory and then, if found, the standard output text file ("output.txt" or "STDOUT.0000") via `scan_stdout`.
"""
function scan_run_dir(pth::String)
    #1 standard output
    filout=joinpath(pth,"output.txt")
    !isfile(filout) ? filout=joinpath(pth,"STDOUT.0000") : nothing
    if isfile(filout)
        tmp=readlines(filout)
        stdout=( isempty(tmp) ? missing : scan_stdout(filout) )
    else
        stdout=missing
    end
    return stdout
end

#alias with old name:
scan_rundir=scan_run_dir

"""
    scan_run_dir(config::MITgcm_config)

Scan a MITgcm run directory (joinpath(MC,"run")) and then, if found, the standard output text file ("output.txt" or "STDOUT.0000") via `scan_stdout`.
"""
scan_run_dir(config::MITgcm_config)=scan_run_dir(joinpath(config,"run"))

"""
    scan_stdout(filout::String)

Scan a MITgcm standard output text file ("output.txt" or "STDOUT.0000") and return a NamedTuple of information collected.

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
    pac = tmp[l0+1:l1-1]
    ll=findall( [!occursin("-------",ln) for ln in pac] )
    pac = pac[ll]
    ll=findall( [!occursin("autodiff",ln) for ln in pac] )
    pac = pac[ll]
    pac_keys = [split(ln)[1][5:end] for ln in pac]
    pac_values = [!occursin("not used",ln) for ln in pac]
    pac=(; zip(Symbol.(pac_keys), pac_values)...)
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
        try
            tmp=read_mnc(pth_mnc,"grid","XC")
            ioSize[1]=size(tmp)
        catch e
            @warn "failed to read mnc file (using NetCDF ?)"
            ioSize[1]=(0,0)
        end
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
    scan_build_dir(MC::MITgcm_config)

alias for `scan_build_dir(pathof(MC),MC.configuration)`
"""
scan_build_dir(MC::MITgcm_config) = scan_build_dir(pathof(MC),MC.configuration)

"""
    scan_build_dir(pth::String,config::String)

```
println.(f);
```
"""
function scan_build_dir(pth::String,config::String)
    genmake_log=joinpath(pth,"MITgcm","verification",config,"build","genmake.log")
    tmp=readlines(genmake_log)
    log = OrderedDict()

    ii=findall(occursin.("running",tmp))
    push!(log,("running"=>tmp[ii]))
    ii=findall(occursin.("-->",tmp))
    push!(log,("-->"=>tmp[ii]))
    ii=findall(occursin.("==>",tmp))
    push!(log,("==>"=>tmp[ii]))
    ii=findall(occursin.("warning",tmp))
    push!(log,("warning"=>tmp[ii]))
    ii=findall(occursin.("error",tmp))
    push!(log,("error"=>tmp[ii]))

    genmake_state=joinpath(pth,"MITgcm","verification",config,"build","genmake_state")
    tmp=readlines(genmake_state)
    state = OrderedDict()

    for st in tmp
        nam=split(st,"=")[1]
        val=st[length(nam)+1:end]
        push!(state,(nam=>val))
    end

    log,state
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

Read a `MITgcm` namelist file in native format, parse it, and return as a `NamedTuple`.

```
using MITgcm
testreport(MITgcm_config(configuration="advect_xy"))
fil=joinpath(MITgcm_path[1],"verification","advect_xy","run","data")
namelist=read_namelist(fil)
```
"""
function read_namelist(fil)

    meta = read(fil,String)
    meta = split(meta,"\n")
    meta = meta[findall((!isempty).(meta))]
    meta = meta[findall(first.(meta).!=='#')]
    meta = meta[findall( (!isempty).(lstrip.(meta)) )]
#    groups = meta[findall(occursin.('&',meta))]
    groups = meta[findall([ (l[1]=='&')||(l[1]=='/') for l in lstrip.(meta)])]
    groups = [Symbol(lstrip(lstrip(groups[1+2*(i-1)])[2:end])) for i in 1:Int(length(groups)/2)]
	params = fill(OrderedDict(),length(groups))

	for i in 1:length(groups)
		ii=1+findall(occursin.(String(groups[i]),meta))[1]
		i1=ii
		tmp0=OrderedDict()
        k0=[:unknown]
		while (!occursin('&',meta[ii]))&&(lstrip(meta[ii])[1]!=='/')
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
                    println("ignoring line -- unclear why ...")
                end
			end
			ii += 1
		end
        for ii in keys(tmp0)
            tmp0[ii]=parse_param(tmp0[ii])
        end
		params[i]=tmp0
	end

    return MITgcm_namelist(Symbol.(groups),params)
end

function list_namelist_files(input_path)
    tmpA=readdir(input_path)
    tmpA=tmpA[findall([length(tmpA[i])>3 for i in 1:length(tmpA)])]
    tmpA=tmpA[findall([tmpA[i][1:4]=="data"||tmpA[i]=="eedata"||
            tmpA[i]=="prepare_run" for i in 1:length(tmpA)])]
end

"""
    read_all_namelists(input_path)

Read all `MITgcm` namelist files in `input_path`, parse them, and return as a NamedTuple of NamedTuples.

```
using MITgcm; path0=default_path()
input_path=joinpath(path0,"verification","advect_xy","input")
params=read_all_namelists(input_path)
```
"""
function read_all_namelists(input_path)
    nmlfiles=list_namelist_files(input_path)

    params=OrderedDict()
    for fil in nmlfiles
        nml=read(joinpath(input_path,fil),MITgcm_namelist())
        ni=length(nml.groups); tmp1=OrderedDict()
        [push!(tmp1,(nml.groups[i] => nml.params[i])) for i in 1:ni]
        tmp2=""
        fil=="data" ? tmp2="main" : nothing
        fil=="eedata" ? tmp2="eedata" : nothing
        occursin("data.",fil) ? tmp2=fil[6:end] : nothing
        if !isempty(tmp2) 
            push!(params,(Symbol(tmp2) => tmp1))
        end
    end
    params
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
        if first(p1)=='\''&&!occursin(',',p1)
			p2=p1[2:end-1]
        elseif occursin('.',p1)
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

Save a `MITgcm` namelist file (native format). In the example below, one is read from file, modified, and then saved to a new file using write_namelist.

```
using MITgcm
fil=joinpath(MITgcm_path[1],"verification","advect_xy","run","data")
nml=read(fil,MITgcm_namelist())
write(fil*"_new",nml)
```

or `write_namelist(fil*"_new",namelist)`.
"""
function write_namelist(fil,namelist)
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
            if isa(x,Array)&&(eltype(x)<:AbstractString)
                tmpy=[""]
                try 
                    parse(Int,x[1][1])
                    [tmpy[1]*=x[ii]*", \n " for ii in 1:length(x)]
                catch
                    [tmpy[1]*="'"*x[ii]*"', \n " for ii in 1:length(x)]
                end
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
    write_all_namelists(params,output_path=tempname())

Write all `MITgcm` namelist to files in `output_path`, from corresponding `toml file`

```
using MITgcm
params=read_toml(:OCCA2)
write_all_namelists(params)
```
"""
function write_all_namelists(params,output_path=tempname())
    !isdir(output_path) ? mkdir(output_path) : nothing
    for k in keys(params)
        nml=params[k]
        if isa(nml,OrderedDict)
            nml=MITgcm_namelist(collect(keys(nml)),collect(values(nml)))
            fil="data"*(k!==:main ? "."*string(k) : "")
            k==:eedata ? fil="eedata" : nothing
            write_namelist(joinpath(output_path,fil),nml)
        end
    end
    output_path
end

"""
    read_toml(toml_file::String)

Read toml parameter file into an OrderedDict with Symbol keys, consistent with `tracked_parameters.toml`.

```
using MITgcm, TOML
pth=joinpath(dirname(pathof(MITgcm)),"..","examples","configurations")
toml_file=joinpath(pth,"tutorial_held_suarez_cs.toml")
params=read_toml(toml_file)
```

Writing parameters to file is straightforward. For example:

```
MC=MITgcm_config(configuration="tutorial_held_suarez_cs")
setup(MC)
open(tempname()*".toml", "w") do io
    TOML.print(io, MC.inputs)
end
```
"""
function read_toml(toml_file::String)
    PA=ClimateModels.TOML.parsefile(toml_file)

    meta = read(toml_file,String)
    meta = split(meta,"\n")
    meta = meta[findall((!isempty).(meta))]
    meta = meta[findall(first.(meta).!=='#')]
    meta=meta

    jj=findall([!isempty(l)&&(l[1]=='[') for l in meta])

	params = OrderedDict()
	groups = unique([split(l,".")[1][2:end] for l in meta[jj]])
    [params[Symbol(g)]=OrderedDict() for g in groups]

    gr=[:unknown]
    sg=[:unknown]
    for j in 1:length(meta)
        if in(j,jj) 
            #new bloc : start ordereddict(ordereddict)
            gr[1]=Symbol(split(meta[j],".")[1][2:end])
            sg[1]=Symbol(split(meta[j],'.')[2][1:end-1])
            params[gr[1]][sg[1]]=OrderedDict()
            #println(string(gr[1])*" -- "*string(sg[1]))
        elseif !isempty(meta[j])
            #add new line to ordereddict(ordereddict(ordereddict))
            if occursin('=',meta[j])
                tmp1=strip(split(meta[j],'=')[1])
                tmp1[1]=='"' ? key=tmp1[2:end-1] : key=tmp1
                params[gr[1]][sg[1]][Symbol(key)]=PA[string(gr[1])][string(sg[1])][key]
            end
        end
    end

    params
end

"""
    read_toml(config_name::Symbol)

Read toml parameter file specified by configuration name.

```
using MITgcm
params=read_toml(:OCCA2)
```
"""
function read_toml(config_name::Symbol)
    pth=joinpath(dirname(pathof(MITgcm)),"..","examples","configurations")
    fil0=joinpath(pth,string(config_name)*".toml")
    read_toml(fil0)
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
    read_mdsio(fil::String,rec::Integer)

Read a single variable / record from a single `MITgcm` MDSIO-type file, and return as an Array.

```
read_mdsio(fil,1)
```
"""
function read_mdsio(fil::String,rec::Integer)
    m=read_meta(fil)
    T=eval(:($(Symbol(m.dataprec))))
    s=m.dimList[:,3]-m.dimList[:,2].+1

    isempty(findall(keys(m).==:nrecords)) ? n=1 : n=m.nrecords
    rec>n ? error("rec too large") : nothing

    m.dataprec=="Float32" ? (L,T)=(4,Float32) : (L,T)=(8,Float64)

    recl=prod(s)*L
    buff=Array{T}(undef, s...)

    f=FortranFiles.FortranFile(fil,"r",access="direct",recl=recl,convert="big-endian")
    FortranFiles.read(f,rec=rec,buff)
    buff
end

"""
    read_mdsio(fil::String,nam::Symbol)

Read a single variable / record from a single `MITgcm` MDSIO-type file, and return as an Array.

```
read_mdsio(fil,:THETA)
```
"""
function read_mdsio(fil::String,nam::Symbol)
    m=read_meta(fil)
    jj=findall(Symbol.(m.fldList[:]).==nam)[1]
    read_mdsio(fil,jj)
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
    ff=glob(fil*"*",pth)
    ff=ff[findall(occursin.(".data",ff))]
    f=basename.(ff)
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

	tmp=read_mnc(pth,"grid","XC")
    exps_ioSize=size(tmp)
    elty=eltype(tmp)
    sc=MITgcm.scan_run_dir(rundir)
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
    sc=MITgcm.scan_run_dir(rundir)
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
      fil=joinpath(dirIn,filList[ff])
      tmp=stat(fil)
      n2[ff]=Int64(tmp.size/n1/reclen)-1
   end

   arr = Array{prec,2}(undef,(n1+1,sum(n2)));
   ii=0;
   #@softscope for ff=1:nf
   for ff=1:nf
      fil=joinpath(dirIn,filList[ff])
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

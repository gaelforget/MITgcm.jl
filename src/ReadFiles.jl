

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
        error("non-llc90 cases not implemented yet")
    end

    fileIn=@sprintf("%s.%04d.nc",fileName,1)
    x = ncread(fileIn,fldName)
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

    #initialize f
    if n==2
        f0=Array{Float64}(undef,90,0)
        f00=Array{Float64}(undef,0,90)
    elseif n==3
        f0=Array{Float64}(undef,90,0,s[3])
        f00=Array{Float64}(undef,0,90,s[3])
    elseif n==4
        f0=Array{Float64}(undef,90,0,s[3],s[4])
        f00=Array{Float64}(undef,0,90,s[3],s[4])
    end
    f=[f0,f0,f0,f00,f00]

    #fill in f
    for ff=1:13
        #read one tile
        fileIn=@sprintf("%s.%04d.nc",fileName,ff)
        x = ncread(fileIn,fldName,start,count)
        #combine tiles
        if ff<=3
            f[1]=cat(f[1],x;dims=2)
        elseif ff<=6
            f[2]=cat(f[2],x;dims=2)
        elseif ff==7
            f[3]=x
        elseif ff<=10
            f[4]=cat(f[4],x;dims=1)
        elseif ff<=13
            f[5]=cat(f[5],x;dims=1)
        end

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
    mytiles = Dict()

    GridVariables=GridLoad(mygrid)

    mytiles["nFaces"]=mygrid.nFaces;
    mytiles["ioSize"]=mygrid.ioSize;

    XC=GridVariables["XC"];
    YC=GridVariables["YC"];
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

    metaDict = Dict{String,Any}(m[1] => m[2] for m in meta)

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
    read_mdsio(datafile)

Read a `MITgcm` mdsio file (".data" binary + ".meta" text pair), and return as an Array

```
p="./hs94.cs-32x32x5/run/"
x=read_mdsio(p*"surfDiag.0000000020.002.001.data")
y=read_mdsio(p*"pickup.ckptA.002.001.data")
z=read_mdsio(p*"T.0000000000.002.001.data")
```
"""
function read_mdsio(fil)
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

Read a `MITgcm`'s MDSIO files (".data" binary + ".meta" text pair), combine, and return as an Array

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

    m=[read_meta(pth*f[k]) for k in kk]
    T=eval(:($(Symbol(m[1].dataprec))))

    m[1].nrecords>1 ? s=Tuple([m[1].dimList[:,1];m[1].nrecords]) : s=Tuple(m[1].dimList[:,1])
    x = Array{T,length(s)}(undef,s)
    
    for k=1:length(m)
        ii=m[k].dimList[1,2]:m[k].dimList[1,3]
        jj=m[k].dimList[2,2]:m[k].dimList[2,3]
        x[ii,jj,:,:]=read_mdsio(pth*f[kk[k]])
    end

    return x
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

    diagInfo = Dict([
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

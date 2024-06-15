module MITgcmNetCDFExt

    using MITgcm, NetCDF
    using MITgcm.Printf, MITgcm.MeshArrays, MITgcm.ClimateModels, MITgcm.Glob
    import MITgcm: read_nctiles, read_mnc

"""
    read_nctiles(fileName,fldName,mygrid; I, eccoVersion4Release4=false, verbose=false)

Read model output from NCTiles file and convert to MeshArray instance. Setting the keyword
argument `eccoVersion4Release4=true` allows `read_nctiles` to read in ECCOv4r4 data which
has a different file naming convention to previous versions.
```
mygrid=GridSpec("LatLonCap")
fileName="nctiles_grid/GRID"
Depth=read_nctiles(fileName,"Depth",mygrid)
hFacC=read_nctiles(fileName,"hFacC",mygrid)
hFacC=read_nctiles(fileName,"hFacC",mygrid,I=(:,:,1))
```
"""
function read_nctiles(fileName::String,fldName::String,mygrid::gcmgrid;
    I::Union{Missing,Tuple{Colon,Colon,Vararg{Union{Colon,Integer}}}}=missing,
    eccoVersion4Release4=false,verbose=false)

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
    fileRoot= eccoVersion4Release4 ? fileIn[1:end-11] : fileIn[1:end-8]
    ntile=ncgetatt(fileIn,"Global","ntile")

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

    nr=50 # number of depth levels, this should be accessed from data in case nr = 1
    numFiles = length(glob("*.nc", pth0))
    f=eccoVersion4Release4==false ? Array{Float64, n}[] : numFiles > 1 ?
                                                          MeshArray(mygrid,mygrid.ioPrec,nr,numFiles) :
                                                          MeshArray(mygrid,mygrid.ioPrec,nr)
    if eccoVersion4Release4
        fill!(f, NaN)
        tiles=Tiles(mygrid,90,90)
        year=pth0[findlast('/', pth0)+1:end]
        months=vcat("0" .* string.(1:9), string.(10:12))
        fileCounter = 0 # for indexing the time in the MeshArray
        for month in months
            fileIn = @sprintf("%s_%s_%s.nc", fileRoot, year, month)
            if isfile(fileIn) #skip if no file
                fileCounter += 1
                verbose ? @info("Reading file $(fileIn)") : nothing
                for l in 1:13, k in 1:50
                    x = ncread(fileIn,fldName,start,count)
                    face = tiles[l].face
                    i=collect(tiles[l].i)
                    j=collect(tiles[l].j)
                    f[face, k, fileCounter][i, j] = x[:,:,l,k,1]
                end
            end
        end
    else
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
                    verbose ? @info("Reading file $(fileIn)") : nothing
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
    end

    fld=eccoVersion4Release4==false ? MeshArray(mygrid,f) : f
    return fld
end

"""
    read_mnc(pth::String,fil::String,var::String)

Read variable `var` from a set of `MITgcm` MNC-type files (netcdf files), combine, and
return as an Array. This method will search within `pth` for files that start with `fil`.
"""
function read_mnc(pth::String,fil::String,var::String)
    lst=readdir(pth)
    lst=lst[findall(occursin.(fil,lst).*occursin.(".nc",lst))]

    fil=joinpath(pth,lst[1])
    ncfile=ClimateModels.NetCDF.open(fil)
    ncatts=ncfile.gatts

    v = ClimateModels.NetCDF.open(fil, var)
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
        ncfile=ClimateModels.NetCDF.open(fil)
        ncatts=ncfile.gatts
        b=(ncatts["bi"],ncatts["bj"])
        s=(ncatts["sNx"],ncatts["sNy"])
        ii=(b[1]-1)*s[1] .+ collect(1:s[1])
        jj=(b[2]-1)*s[2] .+ collect(1:s[2])
        v = ClimateModels.NetCDF.open(fil, var)
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

end


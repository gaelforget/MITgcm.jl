
##  qwckplot function

"""
    qwckplot(fld::MeshArray)

Call qwckplot(fld::MeshArray) with date as title. Example:

```
!isdir("GRID_LLC90") ? error("missing files") : nothing
GridVariables=GridLoad(GridSpec("LLC90"))
qwckplot(GridVariables["Depth"])
```
"""
function qwckplot(fld::MeshArray)
    tmp1=Dates.now()
    tmp1=Dates.format(tmp1, "yyyy-mm-dd HH:MM:SS")
    qwckplot(fld,"Plotted at time "*tmp1)
end

"""
    qwckplot(fld::MeshArray,ttl::String)

Plot input using convert2array and heatmap + add title
"""
function qwckplot(fld::MeshArray,ttl::String)
    arr=convert2array(fld)
    arr=permutedims(arr,[2 1])
    #This uses Plots.jl:
    p=heatmap(arr,title=ttl)
end

"""
    qwckplot(fld::MeshArray,clims::NTuple{2, Number})

Plot input using convert2array and heatmap w. chosen clims
"""
function qwckplot(fld::MeshArray,clims::NTuple{2, Number})
    arr=convert2array(fld)
    arr=permutedims(arr,[2 1])
    #This uses Plots.jl:
    p=heatmap(arr,clims=clims)
end

"""
    MatrixInterp(in::Array{T,N},MTRX,siz) where {T,N}

Interpolate `in` using `MTRX` to grid of size `siz`.
"""
function MatrixInterp(in::Array{T,N},MTRX::SparseMatrixCSC,siz) where {T,N}
    #input
    l=size(in,1)*size(in,2);
    m=size(in,3);
    tmp1=reshape(in,l,m)
    tmp0=Float64.(.!(isnan.(tmp1)))
    tmp1[isnan.(tmp1)].=0.
    siz=siz[1],siz[2],m
    #matrix product
    tmp0=MTRX*tmp0
    tmp1=MTRX*tmp1
    tmp1=tmp1./tmp0
    #this may be redundant:
    tmp1[tmp0 .== 0.] .= NaN
    #output
    out=reshape(tmp1,siz)
    m==1 ? out=dropdims(out,dims=3) : nothing
    return out
end

"""
    prep_MTRX()

Repackage interpolation matrix, mask, etc to `.jld` file.
"""
function prep_MTRX()
    GCMGridSpec()
    GCMGridLoad()
    msk2d=mask(view(MeshArrays.hFacC,:,:,1),NaN,0)
    msk3d=mask(MeshArrays.hFacC,NaN,0)
    msk2d=convert2gcmfaces(msk2d)
    msk3d=convert2gcmfaces(msk3d)

    dirIn=""
    MTRX,lon,lat=read_SPM(dirIn)
    lon=vec(lon[:,1])
    lat=vec(lat[1,:])

    fid = open("GRID_LLC90/RC.data")
    dep=Array{Float64,1}(undef,50)
    read!(fid,dep)
    close(fid)
    dep = -hton.(dep)

    siz2d=(length(lon),length(lat))
    siz3d=(length(lon),length(lat),50)

    save(dirIn*"MTRX.jld", "MTRX", MTRX, "lon", lon, "lat", lat, "dep", dep,
    "msk2d", msk2d, "msk3d", msk3d, "siz2d", siz2d, "siz3d", siz3d)
end

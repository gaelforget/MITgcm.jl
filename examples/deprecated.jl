

##

using MAT

"""
    read_SPM(pth::String,fil::String="interp_precomputed.mat")

Reads pre-computed interpolation (sparse matrix) from
`pth*"interp_precomputed.mat"`.
"""
function read_SPM(pth::String,fil::String="interp_precomputed.mat")
    #vars = matread(pth*"interp_precomputed.mat")
    file = matopen(pth*fil)
    interp=read(file, "interp")
    lon=read(file, "lon")
    lat=read(file, "lat")
    SPM=interp["SPM"]
    #println(keys(interp))
    close(file)
    return SPM,lon,lat
end

##

using JLD

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



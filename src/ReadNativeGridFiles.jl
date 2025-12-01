module ReadNativeGridFiles

using MeshArrays, FortranFiles

"""
    GridLoad_native(path,files,γ)

Load grid variables from native grid files.

```
path=MeshArrays.Dataset("GRID_LLC90")
files=["tile001.mitgrid","tile002.mitgrid","tile003.mitgrid","tile004.mitgrid","tile005.mitgrid"]
ioSize=[90 1170]
facesSize=[(90, 270), (90, 270), (90, 90), (270, 90), (270, 90)]

γ=gcmgrid(path,"LatLonCap",5,facesSize, ioSize, Float64, read, write)

Γ=GridLoad_native(path,files,γ)
```

or using another grid

```
path="llc_1080"
files=[ "llc_001_1080_3240.bin","llc_002_1080_3240.bin",
        "llc_003_1080_1080.bin","llc_004_3240_1080.bin","llc_005_3240_1080.bin"]
ioSize=[1080 14040]
facesSize=[(1080, 3240), (1080, 3240), (1080, 1080), (3240, 1080), (3240, 1080)]
```

and for plotting 

```
using GLMakie

#col=log10.(Γ.RAC); rng=(4,8)

#using MAT
#Depth=get_bathy(path,γ)
#col=write(Depth); rng=(-5000,5000)

XC=write(Γ.XC); YC=write(Γ.YC)
ii=findall((XC.>-80).&(XC.<-10).&(YC.>-10).&(YC.<60))
#ii=findall((XC.>-80).&(XC.<-60).&(YC.>15).&(YC.<35))

#ii=1:prod(γ.ioSize)

scatter(XC[ii],YC[ii],color=col[ii],colorrange=rng,markersize=0.1, markerspace = :data)
```
"""
function GridLoad_native(path,files,γ)
    list_n=("XC","YC","DXF","DYF","RAC","XG","YG","DXV","DYU","RAZ",
            "DXC","DYC","RAW","RAS","DXG","DYG")
    Γ=Dict()
    [Γ[ii]=MeshArray(γ) for ii in list_n]

    #fill in each Γ[ii][ff]
    for ff in 1:length(files)
        fil=joinpath(path,files[ff])

        siz=γ.fSize[ff].+1
        recl=prod(siz)*8
        buffer2d=Array{Float64}(undef, siz...)
        
        f=FortranFiles.FortranFile(fil,"r",access="direct",recl=recl,convert="big-endian")
        for jj in 1:length(list_n)
            FortranFiles.read(f,rec=jj,buffer2d)
            Γ[list_n[jj]][ff].=buffer2d[1:end-1,1:end-1]
        end
    end

    AngleCS,AngleSN=GridLoadNative_Angle(path,files,γ)
    Γ["AngleCS"]=AngleCS
    Γ["AngleSN"]=AngleSN

    return MeshArrays.Dict_to_NamedTuple(Γ)
end

function GridLoadNative_Angle(path,files,γ)
    list_n=("XC","YC","DXF","DYF","RAC","XG","YG","DXV","DYU","RAZ",
            "DXC","DYC","RAW","RAS","DXG","DYG")

    AngleCS=MeshArray(γ)
    AngleSN=MeshArray(γ)
    jj(x)=findall(list_n.==x)[1]

    for ff in 1:length(files)
        fil=joinpath(path,files[ff])

        siz=γ.fSize[ff].+1
        recl=prod(siz)*8
        yG=Array{Float64}(undef, siz...)
        dyG=Array{Float64}(undef, siz...)
        dxG=Array{Float64}(undef, siz...)
        
        f=FortranFiles.FortranFile(fil,"r",access="direct",recl=recl,convert="big-endian")
        FortranFiles.read(f,rec=jj("YG"),yG)
        FortranFiles.read(f,rec=jj("DYG"),dyG)
        FortranFiles.read(f,rec=jj("DXG"),dxG)

        uPseudo= - deg2rad.( yG[:,1:end-1] - yG[:,2:end] ) ./ dyG[:,1:end-1]
        vPseudo= + deg2rad.( yG[1:end-1,:] - yG[2:end,:] ) ./ dxG[1:end-1,:]
        uC=0.5*(uPseudo[1:end-1,:]+uPseudo[2:end,:])
        vC=0.5*(vPseudo[:,1:end-1]+vPseudo[:,2:end])
        uNorm = sqrt.(uC.*uC+vC.*vC)

        AngleCS[ff].=uC./uNorm
        AngleSN[ff].=-vC./uNorm
    end

    return AngleCS,AngleSN
end

#using MAT
function get_bathy(path,γ)
    fil=joinpath(path,"bathy.mat")
    fid=matopen(fil)
    bathy=read(fid,"bathy")
    Depth=MeshArray(γ)
    Depth.f.=[b["vals"] for b in bathy][:]
    return Depth
end

end #module ReadNativeGridFiles


using MITgcmTools, MeshArrays, Plots

p="./hs94.cs-32x32x5/run_long2/"
pp=tempdir()*"/"

readcube(xx::Array,x::MeshArray) = read(cube2compact(xx),x)
readcube(fil::String,x::MeshArray) = read(fil::String,x::MeshArray)
writecube(x::MeshArray) = compact2cube(write(x))
writecube(fil::String,x::MeshArray) = write(fil::String,x::MeshArray)

γ=gcmgrid(p,"CubeSphere",6,fill((32, 32),6), [192 32], Float64, readcube, writecube)
Γ = GridLoad(γ)

lon=[i for i=-179.5:1.0:179.5, j=-89.5:1.0:89.5]
lat=[j for i=-179.5:1.0:179.5, j=-89.5:1.0:89.5]
(f,i,j,w,j_f,j_x,j_y)=InterpolationFactors(Γ,vec(lon),vec(lat))

##

ff=readdir(p); fil="T.0000"
ff=ff[findall(occursin.(fil,ff).*occursin.(".data",ff))]
nt=length(ff)

function myplot(fil)
    T=read(p*fil,MeshArray(γ,Float64))
    TT=Interpolate(T,f,i,j,w)
    contourf(vec(lon[:,1]),vec(lat[1,:]),TT,clims=(260.,320.))
end

##

#dt=Int(ceil(nt/100))
dt=1
anim = @animate for i ∈ 1:dt:nt
    myplot(ff[i])
end

gif(anim,pp*"hs94.cs.10days.gif", fps = 8)


using MITgcmTools, MeshArrays, Plots

p="./hs94.cs-32x32x5/run/"
pp=tempdir()*"/"

(γ,Γ)=GridOfOnes("CubeSphere",6,32)
#f2d(x)=γ.read(permutedims(Float64.(x),[2,1]),MeshArray(γ))
#f3d(x)=γ.read(permutedims(Float64.(x),[2,1,3]),MeshArray(γ))

list_n=("XC","XG","YC","YG","RAC","RAW","RAS","RAZ","DXC","DXG","DYC","DYG","Depth");
for n in list_n
#    tmp=f2d(read_mdsio(p,n))
#    [Γ[n][i].=tmp[i] for i in eachindex(tmp)]
    tmp=read_mdsio(p,n)
    [Γ[n][i].=tmp[(1:32).+(i-1)*32,:] for i in 1:6]
end

#XC=f2d(read_mdsio(p,"XC"))
#YC=f2d(read_mdsio(p,"YC"))
#[Γ["XC"][i].=XC[i] for i in eachindex(XC)];
#[Γ["YC"][i].=YC[i] for i in eachindex(YC)];

lon=[i for i=-179.5:1.0:179.5, j=-89.5:1.0:89.5]
lat=[j for i=-179.5:1.0:179.5, j=-89.5:1.0:89.5]
(f,i,j,w,j_f,j_x,j_y)=InterpolationFactors(Γ,vec(lon),vec(lat))

##

ff=readdir(p); fil="T.0000"
ff=ff[findall(occursin.(fil,ff).*occursin.(".data",ff))]

T=MeshArray(γ)

function myplot(fil)
    tmp=read_mdsio(p,fil)[:,:,3]
    [T[i].=tmp[(1:32).+(i-1)*32,:] for i in 1:6]
    TT=Interpolate(T,f,i,j,w)
    contourf(vec(lon[:,1]),vec(lat[1,:]),TT,clims=(260.,320.))
end

#myplot("T.0000000000")

nt=length(ff)
#nt=300
#dt=Int(ceil(nt/100))
dt=1
anim = @animate for i ∈ 1:dt:nt
    myplot(ff[i])
end

gif(anim,pp*"hs94.cs.10days.gif", fps = 8)

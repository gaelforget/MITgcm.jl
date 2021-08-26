
using MITgcmTools, MeshArrays
exps=verification_experiments()
rep=joinpath(MITgcm_path[1],"verification")

sc=Vector{Any}(nothing, length(exps))
for i in 1:length(exps)
    myexp=exps[i].configuration; rundir=joinpath(rep,myexp,"run")
    sc[i]=MITgcmTools.scan_rundir(rundir)
end

exps_CurvilinearGrid=findall([sc[i].params_grid.usingCurvilinearGrid for i in 1:length(exps)])
exps_SphericalPolarGrid=findall([sc[i].params_grid.usingSphericalPolarGrid for i in 1:length(exps)])
exps_CartesianGrid=findall([sc[i].params_grid.usingCartesianGrid for i in 1:length(exps)])
exps_CylindricalGrid=findall([sc[i].params_grid.usingCylindricalGrid for i in 1:length(exps)])

#GridLoad(γ)
#γ=gcmgrid(rundir,"CubeSphere",6,fill((32,32),6),[32 32*6],Float64, read_mdsio, write)
#γ=gcmgrid(rundir,"PeriodicDomain",1,fill((128,23),1),[128 23],Float32, read_mdsio, write)

#? read_mdsio(XC) > size > gcmgrid

tst_XC=Vector{Any}(nothing, length(exps))
tst_mnc=Vector{Any}(nothing, length(exps))
vec_ioSize=Vector{Any}(nothing, length(exps))

for i in 1:length(exps)
    myexp=exps[i].configuration; rundir=joinpath(rep,myexp,"run")
    tst_XC[i] = !isempty(filter(x -> occursin("XC",x), readdir(rundir)))
    tst_mnc[i] = isdir(joinpath(rundir,"mnc_test_0001"))
    if tst_XC[i] 
        myexp=exps[i].configuration; rundir=joinpath(rep,myexp,"run")
        tmp=read_mdsio(rundir,"XC")
        vec_ioSize[i]=size(tmp)
    end
end

function exp_GridLoad(iexp)
    println(iexp)
    #
    myexp=exps[iexp].configuration; rundir=joinpath(rep,myexp,"run")
    tmp=read_mdsio(rundir,"XC")
    exps_ioSize=size(tmp)
    elty=eltype(tmp)
    #
    if sc[iexp].params_grid.usingCurvilinearGrid
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
    Γ=GridLoad(γ)
end

#for i in findall(tst_XC)
#    show(exps[i])
#    exp_GridLoad(i)    
#end


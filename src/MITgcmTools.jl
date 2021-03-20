module MITgcmTools

using Dates, DataFrames, NetCDF, Printf, MeshArrays, SparseArrays, Pkg.Artifacts

include("ReadFiles.jl")
include("FormatConversions.jl")
include("PhysicalOceanography.jl")

export MITgcm_path, MITgcm_clean, MITgcm_build, MITgcm_compile, MITgcm_link, MITgcm_run
export verification_experiments, testreport, read_namelist, write_namelist
export read_mdsio, read_meta, read_available_diagnostics
export read_bin, read_flt, read_nctiles, findtiles, parse_param
export cube2compact, compact2cube, convert2array, convert2gcmfaces
export SeaWaterDensity, MixedLayerDepth

p=dirname(pathof(MITgcmTools))
artifact_toml = joinpath(p, "../Artifacts.toml")
MITgcm_hash = artifact_hash("MITgcm", artifact_toml)
MITgcm_path = joinpath(artifact_path(MITgcm_hash)*"/","MITgcm-checkpoint67s/")

export MITgcm_namelist, MITgcm_config

"""
    MITgcm_namelist

```
using MITgcmTools
fil=joinpath(MITgcm_path,"verification","advect_xy","run","data")
nml=read_namelist(fil)
MITgcm_namelist(nml.groups,nml.params)
MITgcm_namelist(groups=nml.groups,params=nml.params)
MITgcm_namelist(groups=nml.groups)
```
"""
Base.@kwdef struct MITgcm_namelist
    groups :: Array{Symbol,1} = Array{Symbol,1}(undef, 0)
    params :: Array{Dict{Symbol,Any},1} = Array{Dict{Symbol,Any},1}(undef, 0)
end

import Base:read,write
read(fil::AbstractString,nml::MITgcm_namelist) = read_namelist(fil)
write(fil::AbstractString,nml::MITgcm_namelist) = write_namelist(fil,nml)

"""
    MITgcm_config

```
using MITgcmTools
exps=verification_experiments()
MITgcm_config(exps[end]...)
```
"""
Base.@kwdef struct MITgcm_config
    name :: String = ""
    build_options :: Array{String,1} = Array{String,1}(undef, 0)
    runtime_options :: Array{String,1} = Array{String,1}(undef, 0)
end

"""
    testreport(nam::String,ext="")

Run the testreport script for one model config `nam` (or "all"),
with additional options (optional) speficied in `ext`

```
using MITgcmTools
testreport("front_relax")
```
"""
function testreport(nm::String,ext="")
    pth=pwd()
    cd(tempdir())
    println(pwd())
    if nm!=="all"
        lst=[nm]
    else
        exps=verification_experiments()
        lst=[exps[i].name for i in 1:length(exps)]
    end
    for nm in lst
        c=`$(MITgcm_path)/verification/testreport -t $(MITgcm_path)/verification/$(nm) $ext`
        isempty(ext) ? c=`$(MITgcm_path)/verification/testreport -t $(MITgcm_path)/verification/$(nm)` : nothing
        run(c)
    end
    cd(pth)
    return true
end

"""
    MITgcm_clean(nam::String)
"""
MITgcm_clean(nam::String) = testreport(nam,"-clean")

"""
    MITgcm_build(nam::String)
"""
MITgcm_build(nam::String) = testreport(nam,"-norun")

"""
    MITgcm_compile(nam::String)
"""
function MITgcm_compile(nam::String)
    pth=pwd()
    cd("$(MITgcm_path)/verification/$(nam)/build")
    try
        run(`make`)
    catch e
        println("model compilation may have failed")
    end
    cd(pth)
    return true
end

"""
    MITgcm_link(nam::String)
"""
MITgcm_link(nam::String) = testreport(nam,"-runonly")

"""
    MITgcm_run(nam::String)
"""
function MITgcm_run(nam::String)
    pth=pwd()
    cd("$(MITgcm_path)/verification/$(nam)/run")
    try
        run(pipeline(`./mitgcmuv`,"output.txt"))
    catch e
        println("model run may have failed")
    end
    cd(pth)
    return true
end

"""
    verification_experiments()

```
exps=verification_experiments()
```
"""
function verification_experiments()
    pth=joinpath(MITgcm_path,"verification")
    lst=readdir(pth)
    tmp=[isfile(joinpath(pth,i,"code","packages.conf")) for i in lst]
    tmp2=[isfile(joinpath(pth,i,"code","SIZE.h")) for i in lst]
    lst=lst[findall(tmp.|tmp2)]

    pkg_build=fill(String[],size(lst))
    pkg_run=fill(String[],size(lst))
    for i in 1:length(lst)
        fil=joinpath(pth,lst[i],"code","packages.conf")
        if isfile(fil)
            tmp1=read(fil,String)
            tmp1=split(tmp1,"\n")
            tmp1=tmp1[findall((!isempty).(tmp1))]
            pkg_build[i]=tmp1[findall(first.(tmp1).!=='#')]
        end

        fil=joinpath(pth,lst[i],"input","data.pkg")
        tmp1=read(fil,String)
        tmp1=split(tmp1,"\n")
        tmp1=tmp1[findall((!isempty).(tmp1))]
        tmp1=tmp1[findall(first.(tmp1).!=='#')]
        pkg_run[i]=tmp1[findall([!occursin("&",i) for i in tmp1])]
    end

    [MITgcm_config(lst[i],pkg_build[i],pkg_run[i]) for i in 1:length(lst)]
end

#more:
#
#using Plots; include("recipes_plots.jl"); export qwckplot

#deprecated:
#
#include("deprecated.jl"); export prep_MTRX, read_SPM

end # module

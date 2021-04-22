module MITgcmTools

using Dates, Printf, SparseArrays, Pkg.Artifacts, UUIDs, Suppressor
using OrderedCollections, DataFrames, NetCDF, MeshArrays, ClimateModels

include("Types.jl")
include("ReadFiles.jl")
include("ModelSteps.jl")
include("FormatConversions.jl")
include("PhysicalOceanography.jl")

export MITgcm_path, MITgcm_config, MITgcm_namelist, MITgcm_launch
export testreport, clean, build, compile, setup
#export launch, pause, stop, clock, monitor, train, help
export verification_experiments, read_namelist, write_namelist
export read_mdsio, read_meta, read_available_diagnostics
export read_bin, read_flt, read_nctiles, findtiles, parse_param
export cube2compact, compact2cube, convert2array, convert2gcmfaces
export SeaWaterDensity, MixedLayerDepth

p=dirname(pathof(MITgcmTools))
artifact_toml = joinpath(p, "../Artifacts.toml")
MITgcm_hash = artifact_hash("MITgcm", artifact_toml)
MITgcm_path = joinpath(artifact_path(MITgcm_hash)*"/","MITgcm-checkpoint67s/")

"""
    verification_experiments()

Get list of all `most-standard` configurations of `MITgcm` and return as an Array of `MITgcm_config`

```
using MITgcmTools
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

    exps=fill(MITgcm_config(),length(lst))
    for i in 1:length(lst)
        ID = UUIDs.uuid4()
        exps[i]=MITgcm_config(configuration=lst[i],ID=ID)
    end

    return exps
end

#more:
#
#using Plots; include("recipes_plots.jl"); export qwckplot

#deprecated:
#
#include("deprecated.jl"); export prep_MTRX, read_SPM

end # module

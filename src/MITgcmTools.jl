module MITgcmTools

using Dates, DataFrames, NetCDF, Printf, MeshArrays, SparseArrays, Pkg.Artifacts

include("ReadFiles.jl")
include("FormatConversions.jl")
include("PhysicalOceanography.jl")

export MITgcm_path, testreport
export read_mdsio, read_meta, read_available_diagnostics
export read_bin, read_flt, read_nctiles, findtiles
export cube2compact, compact2cube, convert2array, convert2gcmfaces
export SeaWaterDensity, MixedLayerDepth

p=dirname(pathof(MITgcmTools))
artifact_toml = joinpath(p, "../Artifacts.toml")
MITgcm_hash = artifact_hash("MITgcm", artifact_toml)
MITgcm_path = joinpath(artifact_path(MITgcm_hash)*"/","MITgcm-checkpoint67s/")

"""
    testreport(nam::String)

```
tmp=testreport("front_relax");
```
"""
function testreport(nm::String)
    c=`$(MITgcm_path)/verification/testreport -t $(MITgcm_path)/verification/$(nm)`
    run(c)
end

#more:
#
#using Plots; include("recipes_plots.jl"); export qwckplot

#deprecated:
#
#include("deprecated.jl"); export prep_MTRX, read_SPM

end # module

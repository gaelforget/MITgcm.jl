module MITgcmTools

using Dates, Printf, SparseArrays, Artifacts, LazyArtifacts, UUIDs, Suppressor
using OrderedCollections, DataFrames, NetCDF, MeshArrays, ClimateModels

include("Types.jl")
include("ReadFiles.jl")
include("ModelSteps.jl")
include("FormatConversions.jl")
include("PhysicalOceanography.jl")
include("verification_experiments.jl")

export MITgcm_path, MITgcm_download
export PICKUP_hs94_path, PICKUP_hs94_download
export MITgcm_config, MITgcm_namelist, MITgcm_launch
export testreport, build, compile, setup, clean
#export pause, stop, clock, monitor, train, help
export verification_experiments, read_namelist, write_namelist
export read_mdsio, read_meta, read_available_diagnostics, read_namelist
export scan_rundir, scan_stdout
export read_bin, read_flt, read_mnc, read_nctiles, findtiles
export GridLoad_mnc, GridLoad_mdsio
export cube2compact, compact2cube, convert2array, convert2gcmfaces
export SeaWaterDensity, MixedLayerDepth

p=dirname(pathof(MITgcmTools))
artifact_toml = joinpath(p, "../Artifacts.toml")
MITgcm_hash = artifact_hash("MITgcm", artifact_toml)

"""
    MITgcm_path

Path to a MITgcm folder. `MITgcm_path[1]` should generally be used. `MITgcm_path[2]` is mostly 
meant to facilitate comparisons between e.g. MITgcm releases when needed.
"""
# MITgcm_path = [ joinpath(artifact_path(MITgcm_hash)*"/","MITgcm_test"),
#                 joinpath(artifact_path(MITgcm_hash)*"/","MITgcm_test")]
# TODO: change back
MITgcm_path = ["/Users/birdy/Documents/eaps_research/darwin3"]

PICKUP_hs94_path = artifact_path(artifact_hash("PICKUP_hs94", artifact_toml))

MITgcm_download() = artifact"MITgcm"
PICKUP_hs94_download() = artifact"PICKUP_hs94"

#more:
#
#using Plots; include("recipes_plots.jl"); export qwckplot

#deprecated:
#
#include("deprecated.jl"); export prep_MTRX, read_SPM

end # module

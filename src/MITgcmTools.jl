module MITgcmTools

using Dates, Printf, SparseArrays, Pkg.Artifacts, UUIDs, Suppressor
using OrderedCollections, DataFrames, NetCDF, MeshArrays, ClimateModels

include("Types.jl")
include("ReadFiles.jl")
include("ModelSteps.jl")
include("FormatConversions.jl")
include("PhysicalOceanography.jl")
include("verification_experiments.jl")

export MITgcm_path, MITgcm_download
export MITgcm_config, MITgcm_namelist, MITgcm_launch
export testreport, build, compile, setup, clean
#export pause, stop, clock, monitor, train, help
export verification_experiments, read_namelist, write_namelist
export read_mdsio, read_meta, read_available_diagnostics, scan_rundir
export read_bin, read_flt, read_mnc, read_nctiles, findtiles, parse_param
export GridLoad_mnc, GridLoad_mdsio
export cube2compact, compact2cube, convert2array, convert2gcmfaces
export SeaWaterDensity, MixedLayerDepth

p=dirname(pathof(MITgcmTools))
artifact_toml = joinpath(p, "../Artifacts.toml")
MITgcm_hash = artifact_hash("MITgcm", artifact_toml)
MITgcm_path = [ joinpath(artifact_path(MITgcm_hash)*"/","MITgcm-checkpoint68a/"),
                joinpath(artifact_path(MITgcm_hash)*"/","MITgcm-checkpoint68a/")]
MITgcm_download() = artifact"MITgcm"

#more:
#
#using Plots; include("recipes_plots.jl"); export qwckplot

#deprecated:
#
#include("deprecated.jl"); export prep_MTRX, read_SPM

end # module

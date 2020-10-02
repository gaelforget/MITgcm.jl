module MITgcmTools

using Dates, DataFrames, NetCDF, Printf, MeshArrays, SparseArrays, StatsBase

include("ReadFiles.jl")
include("FormatConversions.jl")
include("PhysicalOceanography.jl")

export MatrixInterp, convert2array, convert2gcmfaces
export read_bin, read_flt, read_nctiles, findtiles
export read_meta, read_available_diagnostics
export SeaWaterDensity, MixedLayerDepth

#more:
#
#using Plots; include("recipes_plots.jl"); export qwckplot

#deprecated:
#
#include("deprecated.jl"); export prep_MTRX, read_SPM

end # module

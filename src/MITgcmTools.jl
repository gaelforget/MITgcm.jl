module MITgcmTools

using Dates, NetCDF, Printf, MeshArrays, SparseArrays

include("ReadFiles.jl")
include("FormatConversions.jl")
include("PhysicalOceanography.jl")

export MatrixInterp, convert2array, convert2gcmfaces
export read_bin, read_flt, read_nctiles, findtiles
export MetaFileRead, parsemeta, readAvailDiagnosticsLog
export SeaWaterDensity, MixedLayerDepth

#more:
#
#using Plots; include("BasicPlots.jl"); export qwckplot

#deprecated:
#
#include("deprecated.jl"); export prep_MTRX, read_SPM

end # module

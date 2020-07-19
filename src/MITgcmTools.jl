module MITgcmTools

using Plots, Dates, NetCDF, Printf, MeshArrays, SparseArrays

include("BasicPlots.jl")
include("FormatConversions.jl")
include("ReadFiles.jl")
include("PhysicalOceanography.jl")

export qwckplot, convert2array, convert2gcmfaces, read_bin
export read_nctiles, findtiles, MetaFileRead
export MatrixInterp
export parsemeta, readAvailDiagnosticsLog
export SeaWaterDensity, MixedLayerDepth

#deprecated:
#include("deprecated.jl")
#export prep_MTRX, read_SPM

end # module

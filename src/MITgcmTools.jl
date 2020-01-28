module MITgcmTools

using Plots, Dates, NetCDF, Printf, MeshArrays, SparseArrays, MAT, JLD

include("BasicPlots.jl")
include("FormatConversions.jl")
include("ReadFiles.jl")

export qwckplot, convert2array, convert2gcmfaces, read_bin
export read_nctiles, findtiles, MetaFileRead
export prep_MTRX, MatrixInterp, read_SPM

end # module

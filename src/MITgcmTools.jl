module MITgcmTools

using Plots, Dates, NetCDF, Printf, MeshArrays

include("BasicPlots.jl")
include("FormatConversions.jl")
include("ReadNCTiles.jl")

export qwckplot, convert2array, convert2gcmfaces, read_bin
export read_nctiles, findtiles

end # module

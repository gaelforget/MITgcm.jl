module MITgcmTools

using Plots, Dates, NetCDF, Printf, MeshArrays

include("BasicPlots.jl")
include("FormatConversions.jl")
include("ReadNCTiles.jl")

export qwckplot, convert2array, convert2gcmfaces, read_bin, read_nctiles

end # module

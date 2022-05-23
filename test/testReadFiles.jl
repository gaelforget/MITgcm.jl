# Tests for MITgcmTools/ReadFiles.jl 
# specifically for read_namelist 


using MITgcmTools
using Test
using OrderedCollections

# read in file thats in /input/
folder  = "/Users/birdy/Documents/eaps_research/darwin3/verification/darwin-single-box/input"
file = "eedata"
fil = joinpath(folder, file)

# write some motherfuckin test cases
nml = read_namelist(fil)

# mock nml
# nml = MITgcm_namelist(groups=Symbol.(["EEPARMS"]), params=fill(OrderedDict(), length(1)))

println(nml)

@test Symbol.("EEPARMS") in nml.groups
@test Symbol.("nTx") in collect(keys(nml.params[1]))
@test Symbol.("nTy") in collect(keys(nml.params[1]))
@test nml.params[1][Symbol.("nTx")] == 1
@test nml.params[1][Symbol.("nTy")] == 1





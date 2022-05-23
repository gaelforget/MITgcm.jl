# Tests for MITgcmTools/ReadFiles.jl 
# specifically for read_namelist 


using MITgcmTools
using Test
using OrderedCollections

# read in file thats in /input/
folder  = "/Users/birdy/Documents/eaps_research/darwin3/verification/darwin-single-box/input"



# test eedata
file = "eedata"
fil = joinpath(folder, file)
nml = read_namelist(fil)
@test Symbol.("EEPARMS") in nml.groups
@test Symbol.("nTx") in collect(keys(nml.params[1]))
@test Symbol.("nTy") in collect(keys(nml.params[1]))
@test nml.params[1][Symbol.("nTx")] == 1
@test nml.params[1][Symbol.("nTy")] == 1

# test data.darwin
file = "data.darwin"
fil = joinpath(folder, file)
nml = read_namelist(fil)
@test Symbol.("DARWIN_FORCING_PARAMS") in nml.groups
@test Symbol.("DARWIN_PARAMS") in nml.groups
@test Symbol.("DARWIN_CDOM_PARAMS") in nml.groups
@test Symbol.("DARWIN_RADTRANS_PARAMS") in nml.groups
@test Symbol.("DARWIN_RANDOM_PARAMS") in nml.groups
@test Symbol.("DARWIN_TRAIT_PARAMS") in nml.groups


# @test Symbol.("nTx") in collect(keys(nml.params[1]))
# @test Symbol.("nTy") in collect(keys(nml.params[1]))
# @test nml.params[1][Symbol.("nTx")] == 1
# @test nml.params[1][Symbol.("nTy")] == 1





# mock nml
# nml = MITgcm_namelist(groups=Symbol.(["EEPARMS"]), params=fill(OrderedDict(), length(1)))





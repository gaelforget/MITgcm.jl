# Tests for MITgcmTools/ReadFiles.jl 
# specifically for read_namelist 


using MITgcmTools
using Test
using OrderedCollections

# read in file thats in /input/
folder  = "/Users/birdy/Documents/eaps_research/darwin3/verification/darwin-single-box/input"



# read_namelist test eedata
file = "eedata"
fil = joinpath(folder, file)
nml = read_namelist(fil)
@test Symbol.("EEPARMS") in nml.groups
@test Symbol.("nTx") in collect(keys(nml.params[1]))
@test Symbol.("nTy") in collect(keys(nml.params[1]))
@test nml.params[1][Symbol.("nTx")] == 1
@test nml.params[1][Symbol.("nTy")] == 1

# read_namelist test data.darwin
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


# test write_namelist 
file = "data.gmredi"
fil = joinpath(folder, file)
nml = read_namelist(fil)
out_file = joinpath(pwd(),"test_read_namelist_scratch")
write_namelist(out_file, nml)
res = read(out_file, String)
println("write_namelist result: ")
println(res)

@test 'D' in res

# mock nml
# nml = MITgcm_namelist(groups=Symbol.(["EEPARMS"]), params=fill(OrderedDict(), length(1)))





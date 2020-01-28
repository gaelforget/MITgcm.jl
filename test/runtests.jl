using MITgcmTools, MeshArrays
using Test

@testset "MITgcmTools.jl" begin
    GridVariables=GridOfOnes("CubeSphere",30,30)
    @test isa(convert2gcmfaces(GridVariables["XC"]),Array)
    @test isa(convert2array(GridVariables["XC"]),Array)
end

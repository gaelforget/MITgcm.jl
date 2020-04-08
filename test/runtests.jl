using MITgcmTools, MeshArrays
using Test

@testset "MITgcmTools.jl" begin

    #format conversions
    (γ,Γ)=GridOfOnes("CubeSphere",30,30)
    @test isa(convert2gcmfaces(Γ["XC"]),Array)
    @test isa(convert2array(Γ["XC"]),Array)

    #physical oceanography
    (ρP,ρI,ρR) = SeaWaterDensity(3.,35.5,3000.)
    @test isapprox(ρI,1041.83267, rtol=1e-6)

    D=collect(0.0:1.0:500.0); tmp=(1.0.-tanh.(5*(-1 .+ 2/D[end]*D)));
    T=2.0 .+ 8.0*tmp; S=34.0 .+ 0.5*tmp;
    (ρP,ρI,ρR) = SeaWaterDensity(T,S,D);
    mld=MixedLayerDepth(T,S,D,"BM");
    @test isapprox(mld,134.0)


end

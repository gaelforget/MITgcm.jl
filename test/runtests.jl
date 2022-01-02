using MITgcmTools, ClimateModels, MeshArrays, OceanStateEstimation
using Test

MITgcm_download()

@testset "MITgcmTools.jl" begin

    #format conversions
    (γ,Γ)=MeshArrays.GridOfOnes("CubeSphere",30,30)
    @test isa(convert2gcmfaces(Γ.XC),Array)
    @test isa(convert2array(Γ.XC),Array)

    #physical oceanography
    (ρP,ρI,ρR) = SeaWaterDensity(3.,35.5,3000.)
    @test isapprox(ρI,1041.83267, rtol=1e-6)

    D=collect(0.0:1.0:500.0); tmp=(1.0.-tanh.(5*(-1 .+ 2/D[end]*D)));
    T=2.0 .+ 8.0*tmp; S=34.0 .+ 0.5*tmp;
    (ρP,ρI,ρR) = SeaWaterDensity(T,S,D);
    mld=MixedLayerDepth(T,S,D,"BM");
    @test isapprox(mld,134.0)

    #running and reading in verification experiments
    exps=verification_experiments()
    @test isa(exps,Array)

    MC=MITgcm_config(configuration="advect_xy")
    setup(MC)

    fil=joinpath(MC.folder,string(MC.ID),"run","data")
    nml=read(fil,MITgcm_namelist())
    write(fil*"_new",nml)
	
    MITgcmTools.parse_param("1.0")
    MITgcmTools.parse_param(".TRUE.")
    MITgcmTools.parse_param(".false.")
    MITgcmTools.parse_param("10")

    @test isa(nml,MITgcm_namelist)
    @test nml.groups[1]==:PARM01
    @test nml.params[1][:implicitFreeSurface]

    #

    myexp="advect_cs"
    iexp=findall([exps[i].configuration==myexp for i in 1:length(exps)])[1]
    MC=exps[iexp]

    @test clean(MC)=="no task left in pipeline"
    @test build(MC)
    @test build(MC,"--allow-skip")
    @test compile(MC)
    @test setup(MC)

    push!(MC.status,("setup" => "ended"))
    
    launch(MC)
    pth=joinpath(MC.folder,string(MC.ID),"run")
    tmp=read_mdsio(pth,"XC.001.001")
    @test isa(tmp,Array)
    tmp=read_mdsio(pth,"XC")
    @test isa(tmp,Array)

    scan_rundir(pth)
    Γ=GridLoad_mdsio(MC)
    @test isa(Γ,NamedTuple)

    myexp="MLAdjust"
    iexp=findall([exps[i].configuration==myexp for i in 1:length(exps)])[1]
    MC=exps[iexp]
    setup(MC)
    build(MC,"--allow-skip")
    launch(MC)
    if isdir(joinpath(MC.folder,string(MC.ID),"run","mnc_test_0001"))
        Γ=GridLoad_mnc(MC)
    else
        Γ=GridLoad_mdsio(MC)
    end
    @test isa(Γ,NamedTuple)

    #read / write functions

    fil=joinpath(pth,"available_diagnostics.log")
    read_available_diagnostics("ETAN";filename=fil)

    readcube(xx::Array,x::MeshArray) = read(cube2compact(xx),x)
    function readcube(fil::String,x::MeshArray) 
        p=dirname(fil)*"/"
        b=basename(fil)[1:end-5]
        xx=read_mdsio(p,b)
        read(cube2compact(xx),x)
    end
    writecube(x::MeshArray) = compact2cube(write(x))
    writecube(fil::String,x::MeshArray) = write(fil::String,x::MeshArray)
    
    γ=gcmgrid(pth,"CubeSphere",6,fill((32, 32),6), [192 32], Float64, readcube, writecube)
    Γ = GridLoad(γ)
    tmp1=writecube(Γ.XC)
    tmp2=readcube(tmp1,Γ.XC)

    @test isa(tmp2,MeshArray)

    ##

    γ=GridSpec("LatLonCap",MeshArrays.GRID_LLC90)
    findtiles(30,30,γ)
    findtiles(30,30,"LatLonCap",MeshArrays.GRID_LLC90)
    read_meta(MeshArrays.GRID_LLC90,"XC.meta")

    fil=joinpath(MeshArrays.GRID_LLC90,"XC.data")
    tmp1=read_bin(fil,γ)
    tmp2=convert2gcmfaces(tmp1)
    tmp3=convert2array(tmp1)
    tmp4=convert2array(tmp3,γ)
    read_bin(fil,γ.ioPrec,γ)
    read_bin(tmp2,tmp1)
    read_bin(tmp2,γ)

    function get_ecco_variable_if_needed(v::String)
        p=dirname(pathof(OceanStateEstimation))
        lst=joinpath(p,"../examples/nctiles_climatology.csv")
        pth=ECCOclim_path
        !isdir(pth*v) ? get_from_dataverse(lst,v,pth) : nothing
    end
    
    get_ecco_variable_if_needed("ETAN")
    tmp=read_nctiles(joinpath(ECCOclim_path,"ETAN/ETAN"),"ETAN",γ,I=(:,:,1))

    @test isa(tmp,MeshArray)

    ##

    MC=MITgcm_config(configuration="flt_example")
    tmp=testreport(MC)
    pth=MITgcm_path[1]*"verification/flt_example/run/"
    tmp=read_flt(pth,Float32)
    
    @test isa(tmp[1,1],Number)

end

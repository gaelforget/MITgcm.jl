using MITgcm, Test

import NetCDF
using MITgcm.MeshArrays
using MITgcm.ClimateModels.Suppressor
using MITgcm.ClimateModels.DataFrames
using MITgcm.ClimateModels.CSV

@testset "ECCO4" begin

    MC=MITgcm_config(inputs=read_toml(:OCCA2))
    push!(MC.inputs[:setup][:main],(:input_folder => tempname()))
    @suppress setup(MC)

    try
        ECCO4_inputs.list0
        list1=ECCO4_inputs.get_list()
        nam1="documentation"
        @suppress ECCO4_inputs.get_files(list1,nam1,joinpath(MC,"run"),filenames=("README.pdf",))
        fil=joinpath(MC,"run","README.pdf")
    catch 
        @warn "could not download from dataverse"
    end

    ref_file=joinpath(MC,"MITgcm","mysetups","ECCOv4","test","testreport_baseline2.csv")
    ref=CSV.read(ref_file,DataFrame)
    report=deepcopy(ref); report.value.+=rand(length(ref.value))
    @suppress ECCO4_testreport.compare(report,ref)
    @test isa(report,DataFrame)

    ECCO4_inputs.download_input_folder(MC, dry_run=true)
    MeshArrays.GRID_LLC90_download()
    ispath(MeshArrays.GRID_LLC90) ? nothing : @warn "missing GRID_LLC90"
    ECCO4_testreport.compute(joinpath(MC,"run"),dry_run=true)

    ECCO4_testreport.list_diags_files(joinpath(MC,"run"))
    ECCO4_testreport.list_diags_files_alt(joinpath(MC,"run"))
        
    try
        Dataverse=MITgcm.ECCO4_inputs.Dataverse
        DOI="doi:10.7910/DVN/ODM2IQ"
        files=Dataverse.file_list(DOI)
        Dataverse.file_download(DOI,files.filename[2])
        fil0=joinpath(tempdir(),files.filename[2])
        fc=ECCO4_testreport.parse_fc(fil0)
        @test isa(fc,NamedTuple)
    catch 
        @warn "could not download from dataverse"
    end

end

@testset "MITgcm.jl" begin

    MITgcm_tests=MITgcm.system_check()
    @test MITgcm_tests["run complete"]

    path0=MITgcm.default_path()
    @test ispath(path0)

    fil=MITgcm.create_script()
    @test isfile(fil)

    #format conversions
    (γ,Γ)=Grids_simple.GridOfOnes("CubeSphere",30,30)
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

    MC=verification_experiments("advect_xy")
    MC=MITgcm_config(configuration="advect_xy")
    setup(MC)

    fil=joinpath(MC.folder,string(MC.ID),"run","data")
    nml=read(fil,MITgcm_namelist())
    write(fil*"_new",nml)
	
    MITgcm.parse_param("1.0")
    MITgcm.parse_param(".TRUE.")
    MITgcm.parse_param(".false.")
    MITgcm.parse_param("10")

    @test isa(nml,MITgcm_namelist)
    @test nml.groups[1]==:PARM01
    @test nml.params[1][:implicitFreeSurface]

    #

    MC=MITgcm_config(configuration="advect_cs")

    @test setup(MC)
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

    MC=MITgcm_config(configuration="MLAdjust")
    setup(MC)
    build(MC,"--allow-skip")
    launch(MC)

    PA=read_toml(joinpath(MC,"log","tracked_parameters.toml"))
    @test PA[:main][:PARM03][:nTimeSteps]==12

    if isdir(joinpath(MC.folder,string(MC.ID),"run","mnc_test_0001"))
        Γ=GridLoad_mnc(MC)
        GridLoad_mnc(Γ.XC.grid)
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

    files=["tile00$i.mitgrid" for i in 1:5]
    Γ=GridLoad_native(MeshArrays.GRID_LLC90,files,γ)
    @test isa(Γ.AngleCS,MeshArray)

    fil=joinpath(MeshArrays.GRID_LLC90,"XC.data")
    tmp1=read_bin(fil,γ)
    tmp2=convert2gcmfaces(tmp1)
    tmp3=convert2array(tmp1)
    tmp4=convert2array(tmp3,γ)
    read_bin(fil,γ.ioPrec,γ)
    read_bin(tmp2,tmp1)
    read_bin(tmp2,γ)
        
    MITgcmScratchSpaces.download_nctiles_sample()
    tmp=read_nctiles(joinpath(MITgcmScratchSpaces.path,"ETAN"),"ETAN",γ,I=(:,:,1))

    @test isa(tmp,MeshArray)

    ##

    path1=joinpath(MITgcm.getdata("mitgcmsmallverif"),"MITgcm","verification")
    f1=joinpath(path1,"flt_example","results","output.with_flt.txt")
    f2=joinpath(path1,"flt_example","results","output.txt")
    isfile(f2) ? nothing : symlink(f1,f2)

    p=MITgcm.getdata("mitgcmsmallverif")
    f=MITgcm.datadeps.add_darwin_arm64_gfortran(p)
    @test ispath(f)

    MC=MITgcm_config(configuration="flt_example")
    testreport(MC)
    pth=joinpath(MC,"MITgcm","verification","flt_example","run")
    tmp=read_flt(pth,Float32)
    
    @test isa(tmp[1,1],Number)

    ##

    dir_out=HS94_pickup_download()
    @test isdir(dir_out)

end

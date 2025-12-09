using MITgcm, Test

import NetCDF
using MITgcm.MeshArrays
using MITgcm.ClimateModels.Suppressor
using MITgcm.ClimateModels.DataFrames
using MITgcm.ClimateModels.CSV

println("Sys.islinux=$(Sys.islinux())")
println("Sys.isapple=$(Sys.isapple())")
println("Sys.iswindows=$(Sys.iswindows())")
println("Sys.ARCH=$(Sys.ARCH)")

path_LLC90=MeshArrays.Dataset("GRID_LLC90")
ispath(path_LLC90) ? nothing : @warn "missing GRID_LLC90"

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

@testset "MITgcm system check" begin
    MITgcm.getdata("mitgcmsmallverif")
    p=MITgcm.getdata("mitgcmsmall")

    f=MITgcm.datadeps.add_darwin_arm64_gfortran(p)
    @test ispath(f)

    path0=MITgcm.default_path()
    @test ispath(path0)

    MITgcm.set_environment_variables_to_default()
    SC=system_check()
    show(SC)
    @test SC.complete
end

@testset "MITgcm various" begin
    dir_out=HS94_pickup_download()
    @test isdir(dir_out)

    fil=MITgcm.create_script()
    @test isfile(fil)

    #format conversions
    (γ,Γ)=MeshArrays.Grids_simple.GridOfOnes("CubeSphere",30,30)
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
end

@testset "verification" begin
    list_main,list_adj,list_inp,list_out=scan_verification()
    @test in("advect_xy",list_main)

    ii=findall(list_main.=="advect_xy")[1]
    list_success,list_fail=verification_loop([ii])
    @test in("advect_xy",list_success)

    exps=verification_experiments()
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
end

@testset "interface" begin
    MC=MITgcm_config(configuration="advect_cs")

    @test setup(MC)
    @test clean(MC)=="no task left in pipeline"
    @test build(MC)
    @test build(MC,"--allow-skip")
    @test compile(MC)
    @test setup(MC)

    push!(MC.status,("setup" => "ended"))
    
    launch(MC)
    monitor(MC)
    sc=scan_run_dir(MC)
    @test sc.completed

    path_cs=joinpath(MC,"run")

    fil=joinpath(path_cs,"available_diagnostics.log")
    diag=read_available_diagnostics("ETAN";filename=fil)
    @test diag["units"]=="m"

    ## CS I/O

    tmp=read_mdsio(path_cs,"XC.001.001")
    @test isa(tmp,Array)
    tmp=read_mdsio(path_cs,"XC")
    @test isa(tmp,Array)

    scan_run_dir(path_cs)
    Γ=GridLoad_mdsio(MC)
    @test isa(Γ,NamedTuple)

    γ=gcmgrid(path_cs,"CubeSphere",6,fill((32, 32),6), [192 32], Float64, MITgcm.readcube, MITgcm.writecube)
    Γ = GridLoad(γ)
    tmp1=MITgcm.writecube(Γ.XC)
    tmp2=MITgcm.readcube(tmp1,Γ.XC)
    @test isa(tmp2,MeshArray)

    G=GridLoad_mdsio(path_cs)
    @test isa(G.XC,MeshArray)
end

@testset "more I/O functions" begin
    ## LLC90

    γ=GridSpec(ID=:LLC90)
    findtiles(30,30,γ)
    read_meta(path_LLC90,"XC.meta")

    files=["tile00$i.mitgrid" for i in 1:5]
    Γ=GridLoad_native(path_LLC90,files,γ)
    @test isa(Γ.AngleCS,MeshArray)

    fil=joinpath(path_LLC90,"XC.data")
    tmp1=read_bin(fil,γ)
    tmp2=convert2gcmfaces(tmp1)
    tmp3=convert2array(tmp1)
    tmp4=convert2array(tmp3,γ)
    read_bin(fil,γ.ioPrec,γ)
    read_bin(tmp2,tmp1)
    read_bin(tmp2,γ)
        
    ## nctiles

    try
        MITgcmScratchSpaces.download_nctiles_sample()
        tmp=read_nctiles(joinpath(MITgcmScratchSpaces.path,"ETAN"),"ETAN",γ,I=(:,:,1))
        @test isa(tmp,MeshArray)
    catch 
        @warn "could not download from dataverse"
    end

end

@testset "mnc I/O functions" begin
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
end

@testset "pkg/flt" begin
    inputs=Dict(:input_folder=>"input.with_flt")
    MC=MITgcm_config(configuration="exp4",inputs=inputs);
    run(MC)
    tmp=read_flt(joinpath(MC,"run"),Float32)
    testreport(MC)
    @test isa(tmp[1,1],Number)
end

@testset "Darwin3" begin
    MC=MITgcm_config(model="darwin3",configuration="31+16+3_RT_1D")
    mkdir(pathof(MC))
    setup_darwin3!(MC)
    @test in("31+16+3_RT_1D",readdir(MC,"MITgcm","mysetups"))
end

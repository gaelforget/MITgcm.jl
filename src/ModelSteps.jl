
import ClimateModels: compile, build, setup, clean, git, launch

"""
    clean(config::MITgcm_config)

Cancel any remaining task (config.channel) and clean up the run directory (via rm).

(part of the climate model interface as specialized for `MITgcm`)
"""
function clean(config::MITgcm_config)
    #cancel any remaining task
    while !isempty(config.channel)
        take!(config.channel)
    end
    #clean up run directory
    pp=joinpath(config.folder,string(config.ID),"run")
    isdir(pp) ? rm(pp,recursive=true) : nothing
    #
    return "no task left in pipeline"
end

"""
    build(config::MITgcm_config)

Build the model using `genmake2`, `make depend`, and `make`. The first two link all 
code files, headers, etc  in the `build/` folder before compiling the model.

Note : this is skipped if `config.inputs[:setup][:main][:exe]` is specified.
"""
function build(config::MITgcm_config)
    do_build=(config.inputs[:setup][:build][:rebuild])||(!ispath(config.inputs[:setup][:build][:exe]))
    if do_build
        try
            pth=pwd()
        catch e
            cd()
        end
        pth=pwd()
        cd(config.inputs[:setup][:build][:path])
        opt=config.inputs[:setup][:build][:options]
        opt=Cmd(convert(Vector{String}, split(opt)))
        tst=haskey(config.inputs[:setup][:build],:rootdir)
        tst ? rootdir=config.inputs[:setup][:build][:rootdir] : rootdir=joinpath(config,"MITgcm")
        try
            withenv("MITGCM_ROOTDIR"=>rootdir) do
                genmake2=joinpath(MITgcm_path[1],"tools","genmake2")
                @suppress run(`$(genmake2) $(opt)`)
                @suppress run(`make clean`)
                @suppress run(`make depend`)
                @suppress run(`make -j 4`)
            end
        catch e
            println("model compilation may have failed")
        end
        cd(pth)
        return true
    end
end

"""
    build(config::MITgcm_config,options::String)

Build the model using `genmake2`, `make depend`, and `make` unless otherwise
specified via `options`. The `genmake2` and `make depend` commands link all 
code files, headers, etc  in the `build/` folder before `make` compiles the model.

(part of the climate model interface as specialized for `MITgcm`)
"""
function build(config::MITgcm_config,options::String)
    if options=="--allow-skip"
        exe=config.inputs[:setup][:build][:exe]
        tst=!isfile(joinpath(config.inputs[:setup][:build][:path],exe))
        tst ? build(config) : nothing
    else
        build(config)
    end
    return true
end

"""
    compile(config::MITgcm_config)

Compile the model using `make` in `build/` that has already been setup.

(part of the climate model interface as specialized for `MITgcm`)
"""
function compile(config::MITgcm_config)
    try
        pth=pwd()
    catch e
        cd()
    end
    pth=pwd()
    cd(config.inputs[:setup][:build][:path])
    try
        @suppress run(`make -j 4`)
    catch e
        println("model compilation may have failed")
    end
    cd(pth)
    return true
end

build_options_pleiades="-mods=../code -optfile=../../../tools/"*
  "build_options/linux_amd64_ifort+mpi_ice_nas -mpi"

linux_arm64_gfortran="-mods=../code -optfile=../../../tools/"*
  "build_options/linux_arm64_gfortran"

build_options_default=["-mods=../code", linux_arm64_gfortran,build_options_pleiades]

"""
    setup(config::MITgcm_config)

Create a `run/` folder and link everything there as needed to be ready to run model as 
normally done for most-standard MITgcm configurations (incl. `prepare_run` and `mitgcmuv`).
Call `ClimateModels.git_log_init(config)` to setup git tracker and 
`put!(config.channel,MITgcm_launch)` to be executed via `launch(config)` later.

(part of the climate model interface as specialized for `MITgcm`)
"""
function setup(config::MITgcm_config)
    !isdir(joinpath(config.folder)) ? mkdir(joinpath(config.folder)) : nothing
    !isdir(joinpath(config.folder,string(config.ID))) ? mkdir(joinpath(config.folder,string(config.ID))) : nothing

    pth_run=joinpath(config.folder,string(config.ID),"run")
    !isdir(pth_run) ? mkdir(pth_run) : nothing

    pth_log=joinpath(config.folder,string(config.ID),"log")
    !isdir(pth_log) ? ClimateModels.git_log_init(config) : nothing

    if !isfile(joinpath(pth_run,"data"))&&isfile(joinpath(pth_log,"tracked_parameters","data"))
        p=joinpath(pth_log,"tracked_parameters")
        f=readdir(p)
        [symlink(joinpath(p,f[i]),joinpath(pth_run,f[i])) for i in 1:length(f)]
    end

    if !isempty(config.inputs)
        nam=config.inputs[:setup][:main][:name]
        if nam=="ECCO4"||nam=="OCCA2"
            setup_ECCO4!(config)
        elseif config.inputs[:setup][:main][:category]=="verification"
            setup_verification!(config)    
        else
            error("unknown model configuration")
        end
    elseif config.model=="darwin3"
        setup_darwin3!(config)
    elseif !haskey(config.inputs,:setup)||(config.inputs[:setup][:main][:category]=="verification")
        setup_verification!(config)    
    else
        error("unknown model configuration")
    end

    pth_tra=joinpath(pth_log,"tracked_parameters")
    !isdir(pth_tra) ? mkdir(pth_tra) : nothing
    write_all_namelists(config.inputs,pth_tra)

    if config.model=="darwin3"
        p3=joinpath(MITgcm.getdata("darwin3oneD"),"input_"*config.configuration,"input_1D_BATS")
        ispath(p3) ? cp(p3,joinpath(pth_run,"input_1D_BATS")) : nothing
        p3=joinpath(MITgcm.getdata("darwin3oneD"),"input_"*config.configuration,"OPTICS_COEFF2")
        ispath(p3) ? cp(p3,joinpath(pth_run,"OPTICS_COEFF2")) : nothing        
    end

    #replace namelists with editeable versions in pth_tra
    pth_mv=joinpath(config.folder,string(config.ID),"original_parameters")
    if !isdir(pth_mv)
        mkdir(pth_mv)
        nmlfiles=list_namelist_files(pth_tra)
        for fil in nmlfiles
            fr=joinpath(pth_run,fil)
            fm=joinpath(pth_mv,fil)
            fl=joinpath(pth_tra,fil)
            isfile(fr) ? mv(fr,fm) : nothing
            symlink(fl,fr)
        end
    end

    ClimateModels.git_log_prm(config)

    exe=config.inputs[:setup][:build][:exe]
    if !islink(joinpath(pth_run,exe))
        f=joinpath(config.inputs[:setup][:build][:path],exe)
        symlink(f,joinpath(pth_run,exe)) 
    end

    #add model run to scheduled tasks
    put!(config.channel,MITgcm_launch)

    return true
end

"""
    launch(config::MITgcm_config)

Go to `run/` folder and effectively call `mitgcmuv > output.txt`

(part of the climate model interface as specialized for `MITgcm`)
"""
launch(config::MITgcm_config)=MITgcm_launch(config)

function MITgcm_launch(config::MITgcm_config)
    try
        pth=pwd()
    catch e
        cd()
    end
    pth=pwd()
    cd(joinpath(config.folder,string(config.ID),"run"))
    tmp=["STOP NORMAL END"]
    try
        if haskey(config.inputs[:setup][:main],:command)
            s=config.inputs[:setup][:main][:command]
            c=Cmd(convert(Vector{String}, split(s)))
            @suppress run(pipeline(c))
        else
            exe=config.inputs[:setup][:build][:exe]
            try 
                @suppress run(pipeline(`./$(exe)`,"output.txt"))
            catch
                @suppress run(pipeline(`$(exe)`,"output.txt"))
            end
        end
    catch e
        tmp[1]="model run may have failed"
    end
    cd(pth)
    return tmp[1]
end

to_DF(x)=DataFrame((name=[keys(x)...],value=[values(x)...]))

"""
    function monitor(config::MITgcm_config)

Call `scan_rundir` and show to REPL. 
"""
monitor(config::MITgcm_config) = begin
    rundir=joinpath(config,"run")
    sc=MITgcm.scan_rundir(rundir)
    lst=[:packages, :params_grid, :params_files, :params_time, :completed]
    show(config)
    for nam in lst
        println("")
        println(nam)
        show(to_DF(sc[nam]))
        println("")
    end
end

##

"""
    testreport(config::MITgcm_config,ext="")

Run the testreport script for one model `config`,
with additional options (optional) speficied in `ext`

```
using MITgcm
testreport(MITgcm_config(configuration="front_relax"),"-norun")
#testreport(MITgcm_config(configuration="all"),"-norun")
```
"""
function testreport(config::MITgcm_config,ext="")
    nm=config.configuration
    try
        pth=pwd()
    catch e
        cd()
    end
    pth=pwd()
    tmpdir=tempname(); mkdir(tmpdir); cd(tmpdir)
    println(pwd())
    if nm!=="all"
        lst=[nm]
    else
        exps=verification_experiments()
        lst=[exps[i].configuration for i in 1:length(exps)]
    end

    cp(MITgcm_path[1],"MITgcm")
    mv(joinpath("MITgcm","verification"),joinpath("MITgcm","verification.tmp") )
    cp(MITgcm_path[2],joinpath("MITgcm","verification"))
    cd(joinpath("MITgcm","verification"))

    for nm in lst
        c=`./testreport -t $(nm) $ext`
        isempty(ext) ? c=`./testreport -t $(nm)` : nothing
        @suppress run(c)
    end
    cd(pth)
    return tmpdir
end


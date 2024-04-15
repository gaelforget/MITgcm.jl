
module MITgcmScratchSpaces

using Downloads, Scratch

# This will be filled in inside `__init__()`
path = ""

# Downloads a resource, stores it within path
function download_dataset(url,path)
    fname = joinpath(path, basename(url))
    if !isfile(fname)
        Downloads.download(url, fname)
    end
    return fname
end

function __init__()
    global path = @get_scratch!("src")
end

end


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
    cd(tempdir())
    println(pwd())
    if nm!=="all"
        lst=[nm]
    else
        exps=verification_experiments()
        lst=[exps[i].configuration for i in 1:length(exps)]
    end
    for nm in lst
        c=`$(MITgcm_path[1])/verification/testreport -t $(MITgcm_path[1])/verification/$(nm) $ext`
        isempty(ext) ? c=`$(MITgcm_path[1])/verification/testreport -t $(MITgcm_path[1])/verification/$(nm)` : nothing
        @suppress run(c)
    end
    cd(pth)
    return true
end

import ClimateModels: compile, build, setup, clean, git

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
code files, headers, etc  in the `build/` folder before compiling the model

(part of the climate model interface as specialized for `MITgcm`)
"""
function build(config::MITgcm_config)
    try
        pth=pwd()
    catch e
        cd()
    end
    pth=pwd()
    cd(config.inputs[:setup][:build][:path])
    opt=config.inputs[:setup][:build][:options]
    try
        @suppress run(`../../../tools/genmake2 $(opt)`) #$ext
        @suppress run(`make clean`)
        @suppress run(`make depend`)
        @suppress run(`make -j 4`)
    catch e
        println("model compilation may have failed")
    end
    cd(pth)
    return true
end

"""
    build(config::MITgcm_config,options::String)

Build the model using `genmake2`, `make depend`, and `make` unless otherwise
specified via `options`. The `genmake2` and `make depend` commands link all 
code files, headers, etc  in the `build/` folder before `make` compiles the model.

(part of the climate model interface as specialized for `MITgcm`)
"""
function build(config::MITgcm_config,options::String)
    exe=config.inputs[:setup][:build][:exe]
    if options=="--allow-skip"
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

    if !haskey(config.inputs,:setup)||(config.inputs[:setup][:main][:category]=="verification")
        setup_verification!(config)
    elseif !isempty(config.inputs)
        nam=config.inputs[:setup][:main][:name]
        if nam=="ECCO4"||nam=="OCCA2"
            setup_ECCO4!(config)
        else
            error("unknown model configuration")
        end
    else
        error("unknown model configuration")
    end

    pth_tra=joinpath(pth_log,"tracked_parameters")
    !isdir(pth_tra) ? mkdir(pth_tra) : nothing
    write_all_namelists(config.inputs,pth_tra)

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
    setup_ECCO4!(config::MITgcm_config)

Setup method for ECCO4 and OCCA2 solutions.

```
fil=joinpath("examples","configurations","OCCA2.toml")
MC=MITgcm_config(inputs=read_toml(fil))
setup(MC)
build(MC)
MITgcm_launch(MC)
```
"""
function setup_ECCO4!(config::MITgcm_config)
    if !haskey(config.inputs[:setup],:build)
        println("get MITgcm checkoint, link code folder, ... ")
        u0="https://github.com/MITgcm/MITgcm"; p0=joinpath(config,"MITgcm")
        run(`$(git()) clone --depth 1 --branch checkpoint68o $(u0) $(p0)`)
        u0="https://github.com/gaelforget/ECCOv4"; p0=joinpath(config,"ECCOv4")
        run(`$(git()) clone $(u0) $(p0)`)
        p1=joinpath(config,"MITgcm","mysetups")
        p2=joinpath(p1,"ECCOv4")
        mkdir(p1); mv(p0,p2)
        p3=joinpath(p2,"build")
        P=OrderedDict(:path=>p3,:options=>"-mods=../code -mpi",:exe=>"mitgcmuv")
        push!(config.inputs[:setup],(:build => P))
    end
    return true
end

"""
    MITgcm_launch(config::MITgcm_config)

Go to `run/` folder and effectively call `mitgcmuv > output.txt`

(part of the climate model interface as specialized for `MITgcm`)
"""
function MITgcm_launch(config::MITgcm_config)
    try
        pth=pwd()
    catch e
        cd()
    end
    pth=pwd()
    cd(joinpath(config.folder,string(config.ID),"run"))
    tmp=["STOP NORMAL END"]
    exe=config.inputs[:setup][:build][:exe]
    try
        @suppress run(pipeline(`./$(exe)`,"output.txt"))
    catch e
        tmp[1]="model run may have failed"
    end
    cd(pth)
    return tmp[1]
end

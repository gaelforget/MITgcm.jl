"""
    testreport(nam::String,ext="")

Run the testreport script for one model config `nam` (or "all"),
with additional options (optional) speficied in `ext`

```
using MITgcmTools
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
        c=`$(MITgcm_path)/verification/testreport -t $(MITgcm_path)/verification/$(nm) $ext`
        isempty(ext) ? c=`$(MITgcm_path)/verification/testreport -t $(MITgcm_path)/verification/$(nm)` : nothing
        @suppress run(c)
    end
    cd(pth)
    return true
end

import ClimateModels: compile, build, clean, setup

"""
    clean(config::MITgcm_config)

Cancel any remaining task (config.channel), clean up the build 
directory (via testreport), and clean the run directory (via rm).

(part of the climate model interface as specialized for `MITgcm`)
"""
function clean(config::MITgcm_config)
    #cancel any remaining task
    while !isempty(config.channel)
        take!(config.channel)
    end
    #clean up build directory
    testreport(config,"-clean")
    #clean up run directory
    pp=joinpath(config.folder,string(config.ID),"run")
    isdir(pp) ? rm(pp,recursive=true) : nothing
    #
    return "no task left in pipeline"
end

"""
    build(config::MITgcm_config)

Build the model using `genmake2`, `make depend`, and `make`. These link all 
code files, headers, etc  in the `build/` folder before compiling the model

(part of the climate model interface as specialized for `MITgcm`)
"""
#build(config::MITgcm_config) = testreport(config,"-j 4")
function build(config::MITgcm_config)
    nam=config.configuration
    try
        pth=pwd()
    catch e
        cd()
    end
    pth=pwd()
    cd("$(MITgcm_path)/verification/$(nam)/build")
    try
        @suppress run(`../../../tools/genmake2 -mods=../code`) #$ext
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
    compile(config::MITgcm_config)

Compile the model using `make` in `build/` that has already been setup.

(part of the climate model interface as specialized for `MITgcm`)
"""
function compile(config::MITgcm_config)
    nam=config.configuration
    try
        pth=pwd()
    catch e
        cd()
    end
    pth=pwd()
    cd("$(MITgcm_path)/verification/$(nam)/build")
    try
        @suppress run(`make`)
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
Call `init_git_log(config)` to setup git tracker and `put!(config.channel,MITgcm_launch)` 
to be executed via `launch(config)` later.

(part of the climate model interface as specialized for `MITgcm`)
"""
function setup(config::MITgcm_config)
    !isdir(joinpath(config.folder)) ? mkdir(joinpath(config.folder)) : nothing
    !isdir(joinpath(config.folder,string(config.ID))) ? mkdir(joinpath(config.folder,string(config.ID))) : nothing
    pp=joinpath(config.folder,string(config.ID),"run")
    !isdir(pp) ? mkdir(pp) : nothing
    if !isfile(joinpath(pp,"data"))
        p="$(MITgcm_path)/verification/$(config.configuration)/input"
        f=readdir(p)
        [symlink(joinpath(p,f[i]),joinpath(pp,f[i])) for i in 1:length(f)]
    end
    #replace relative paths with absolutes then exe prepare_run
    if isfile(joinpath(pp,"prepare_run"))
		try
			pth=pwd()
		catch e
			cd()
		end
        pth=pwd()
        cd(pp)
        #
        fil="prepare_run"
        meta = read(fil,String)
        meta = split(meta,"\n")
        ii=findall(occursin.("../../",meta))
        for i in ii
            meta[i]=replace(meta[i],"../../" => "$(MITgcm_path)/verification/")
        end
        ii=findall(occursin.("../",meta))
        for i in ii
            meta[i]=replace(meta[i],"../" => "$(MITgcm_path)/verification/$(config.configuration)")
        end
        #rm old file from run dir
        rm(fil)
        #write new file in run dir
        txt=["$(meta[i])\n" for i in 1:length(meta)]
        fid = open(fil, "w")
		[write(fid,txt[i]) for i in 1:length(txt)]
    	close(fid)
        #execute prepare_run
        chmod(fil,0o777)
        @suppress run(`./$(fil)`)
        #
        cd(pth)
    end

    if !islink(joinpath(pp,"mitgcmuv"))
        f="$(MITgcm_path)/verification/$(config.configuration)/build/mitgcmuv"
        symlink(f,joinpath(pp,"mitgcmuv")) 
    end

    logdir=joinpath(config.folder,string(config.ID),"log")
    !isdir(logdir) ? init_git_log(config) : nothing

    #Replace namelists with editeable versions in log/
    #
    #- read from run folder, rewrite to log/parameter_files
    #- mv all namelists to ../original_parameter_files
    #- link from log/parameter_files to here (run/)
    #(- add to git with message = original params)

    pth=joinpath(config.folder,string(config.ID),"run")
    function list_namelist_files(pth)
            tmpA=readdir(pth)
            tmpA=tmpA[findall([length(tmpA[i])>3 for i in 1:length(tmpA)])]
            tmpA=tmpA[findall([tmpA[i][1:4]=="data" for i in 1:length(tmpA)])]
    end
    nmlfiles=list_namelist_files(pth)

    pth_log=joinpath(config.folder,string(config.ID),"log","tracked_parameters")
    !isdir(pth_log) ? mkdir(pth_log) : nothing
    for fil in nmlfiles
        nml=read(joinpath(pth,fil),MITgcm_namelist())
        write(joinpath(pth_log,fil),nml)
    end

    pth_mv=joinpath(config.folder,string(config.ID),"original_parameters")
    !isdir(pth_mv) ? mkdir(pth_mv) : nothing
    for fil in nmlfiles
        mv(joinpath(pth,fil),joinpath(pth_mv,fil))
        symlink(joinpath(pth_log,fil),joinpath(pth,fil))
    end

    #add model run to scheduled tasks
    put!(config.channel,MITgcm_launch)

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
    try
        @suppress run(pipeline(`./mitgcmuv`,"output.txt"))
    catch e
        tmp[1]="model run may have failed"
    end
    cd(pth)
    return tmp[1]
end

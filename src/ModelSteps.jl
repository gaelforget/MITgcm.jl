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
        run(c)
    end
    cd(pth)
    return true
end

import ClimateModels: compile, build, clean, setup

"""
    clean(config::MITgcm_config)

Cancel any remaining task (config.channel), clean up the build 
directory (via testreport), and clean the run directory (via rm).
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

Part of the climate model interface -- specialized for `MITgcm`
"""
build(config::MITgcm_config) = testreport(config,"-norun")

"""
    compile(config::MITgcm_config)

Part of the climate model interface -- specialized for `MITgcm`
"""
function compile(config::MITgcm_config)
    nam=config.configuration
    pth=pwd()
    cd("$(MITgcm_path)/verification/$(nam)/build")
    try
        run(`make`)
    catch e
        println("model compilation may have failed")
    end
    cd(pth)
    return true
end

"""
    setup(config::MITgcm_config)

Part of the climate model interface -- specialized for `MITgcm`
"""
#setup(config::MITgcm_config) = testreport(config,"-q")
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
        run(`./$(fil)`)
        #
        cd(pth)
    end

    if !islink(joinpath(pp,"mitgcmuv"))
        f="$(MITgcm_path)/verification/$(config.configuration)/build/mitgcmuv"
        symlink(f,joinpath(pp,"mitgcmuv")) 
    end

    init_git_log(config)

    put!(config.channel,MITgcm_launch)

    return true
end

"""
    MITgcm_launch(config::MITgcm_config)

Part of the climate model interface -- specialized for `MITgcm`
"""
function MITgcm_launch(config::MITgcm_config)
    pth=pwd()
    cd(joinpath(config.folder,string(config.ID),"run"))
    tmp=["STOP NORMAL END"]
    try
        run(pipeline(`./mitgcmuv`,"output.txt"))
    catch e
        tmp[1]="model run may have failed"
    end
    cd(pth)
    return tmp[1]
end

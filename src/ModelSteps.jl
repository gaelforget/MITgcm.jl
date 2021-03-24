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
"""
clean(config::MITgcm_config) = testreport(config,"-clean")

"""
    build(config::MITgcm_config)
"""
build(config::MITgcm_config) = testreport(config,"-norun")

"""
    compile(config::MITgcm_config)
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
"""
#setup(config::MITgcm_config) = testreport(config,"-q")
function setup(config::MITgcm_config)
    !isdir(joinpath(config.folder)) ? mkdir(joinpath(config.folder)) : nothing
    !isdir(joinpath(config.folder,"run")) ? mkdir(joinpath(config.folder,"run")) : nothing
    if !isfile(joinpath(config.folder,"run","data"))
        p="$(MITgcm_path)/verification/$(config.configuration)/input"
        f=readdir(p)
        [symlink(joinpath(p,f[i]),joinpath(config.folder,"run",f[i])) for i in 1:length(f)]
    end
    #replace relative paths with absolutes then exe prepare_run
    if isfile(joinpath(config.folder,"run","prepare_run"))
        pth=pwd()
        cd(joinpath(config.folder,"run"))
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

    if !isfile(joinpath(config.folder,"run","mitgcmuv"))
        f="$(MITgcm_path)/verification/$(config.configuration)/build/mitgcmuv"
        symlink(f,joinpath(config.folder,"run","mitgcmuv")) 
    end

    put!(config.channel,MITgcm_launch)

    return true
end

"""
    MITgcm_launch(config::MITgcm_config)
"""
function MITgcm_launch(config::MITgcm_config)
    pth=pwd()
    cd(joinpath(config.folder,"run"))
    tmp=["STOP NORMAL END"]
    try
        run(pipeline(`./mitgcmuv`,"output.txt"))
    catch e
        tmp[1]="model run may have failed"
    end
    cd(pth)
    return tmp[1]
end

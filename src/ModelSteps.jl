"""
    testreport(nam::String,ext="")

Run the testreport script for one model config `nam` (or "all"),
with additional options (optional) speficied in `ext`

```
using MITgcmTools
testreport("front_relax")
```
"""
function testreport(config::MITgcm_config,ext="")
    nm=config.name
    pth=pwd()
    cd(tempdir())
    println(pwd())
    if nm!=="all"
        lst=[nm]
    else
        exps=verification_experiments()
        lst=[exps[i].name for i in 1:length(exps)]
    end
    for nm in lst
        c=`$(MITgcm_path)/verification/testreport -t $(MITgcm_path)/verification/$(nm) $ext`
        isempty(ext) ? c=`$(MITgcm_path)/verification/testreport -t $(MITgcm_path)/verification/$(nm)` : nothing
        run(c)
    end
    cd(pth)
    return true
end

"""
    MITgcm_clean(config::MITgcm_config)
"""
MITgcm_clean(config::MITgcm_config) = testreport(config.name,"-clean")

"""
    MITgcm_build(config::MITgcm_config)
"""
MITgcm_build(config::MITgcm_config) = testreport(config.name,"-norun")

"""
    MITgcm_compile(config::MITgcm_config)
"""
function MITgcm_compile(config::MITgcm_config)
    nam=config.name
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
    MITgcm_link(config::MITgcm_config)
"""
MITgcm_link(config::MITgcm_config) = testreport(config.name,"-runonly")

"""
    MITgcm_run(config::MITgcm_config)
"""
function MITgcm_run(config::MITgcm_config)
    nam=config.name
    pth=pwd()
    cd("$(MITgcm_path)/verification/$(nam)/run")
    try
        run(pipeline(`./mitgcmuv`,"output.txt"))
    catch e
        println("model run may have failed")
    end
    cd(pth)
    return true
end

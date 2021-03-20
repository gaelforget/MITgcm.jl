"""
    testreport(nam::String,ext="")

Run the testreport script for one model config `nam` (or "all"),
with additional options (optional) speficied in `ext`

```
using MITgcmTools
testreport("front_relax")
```
"""
function testreport(nm::String,ext="")
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
    MITgcm_clean(nam::String)
"""
MITgcm_clean(nam::String) = testreport(nam,"-clean")

"""
    MITgcm_build(nam::String)
"""
MITgcm_build(nam::String) = testreport(nam,"-norun")

"""
    MITgcm_compile(nam::String)
"""
function MITgcm_compile(nam::String)
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
    MITgcm_link(nam::String)
"""
MITgcm_link(nam::String) = testreport(nam,"-runonly")

"""
    MITgcm_run(nam::String)
"""
function MITgcm_run(nam::String)
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

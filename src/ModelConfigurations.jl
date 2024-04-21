

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
verification_experiments()

Get list of all `most-standard` configurations of `MITgcm` and return as an Array of `MITgcm_config`

```
exps=verification_experiments()
```
"""
function verification_experiments()
MITgcm_download()
pth=joinpath(MITgcm_path[1],"verification")
lst=readdir(pth)
tmp=[isfile(joinpath(pth,i,"code","packages.conf")) for i in lst]
tmp2=[isfile(joinpath(pth,i,"code","SIZE.h")) for i in lst]
lst=lst[findall(tmp.|tmp2)]

pkg_build=fill(String[],size(lst))
pkg_run=fill(String[],size(lst))
for i in 1:length(lst)
    fil=joinpath(pth,lst[i],"code","packages.conf")
    if isfile(fil)
        tmp1=read(fil,String)
        tmp1=split(tmp1,"\n")
        tmp1=tmp1[findall((!isempty).(tmp1))]
        pkg_build[i]=tmp1[findall(first.(tmp1).!=='#')]
    end

    fil=joinpath(pth,lst[i],"input","data.pkg")
    tmp1=read(fil,String)
    tmp1=split(tmp1,"\n")
    tmp1=tmp1[findall((!isempty).(tmp1))]
    tmp1=tmp1[findall(first.(tmp1).!=='#')]
    pkg_run[i]=tmp1[findall([!occursin("&",i) for i in tmp1])]
end

exps=fill(MITgcm_config(),length(lst))
for i in 1:length(lst)
    ID = UUIDs.uuid4()
    exps[i]=MITgcm_config(configuration=lst[i],ID=ID)
end

return exps
end

"""
    verification_experiments(nam::String)

Get one configurations of `MITgcm` and return as a `MITgcm_config`

```
advect_xy=verification_experiments("advect_xy")
```
"""
function verification_experiments(nam::String)
    exps=verification_experiments()
    iexp=findall([exps[i].configuration==nam for i in 1:length(exps)])[1]
    exps[iexp]
end

"""
    setup_verification!(config::MITgcm_config)

Setup method for verification experiments.
"""
function setup_verification!(config::MITgcm_config)
    pth_run=joinpath(config.folder,string(config.ID),"run")

    MITgcm_download()
    p="$(MITgcm_path[1])/verification/$(config.configuration)/input"
    tmpA=readdir(p)
    f=tmpA[findall([!isfile(joinpath(pth_run,tmpA[i])) for i in 1:length(tmpA)])]
    [symlink(joinpath(p,f[i]),joinpath(pth_run,f[i])) for i in 1:length(f)]

    #replace relative paths with absolutes then exe prepare_run
    if isfile(joinpath(pth_run,"prepare_run"))
        try
            pth=pwd()
        catch e
            cd()
        end
        pth=pwd()
        cd(pth_run)
        #
        fil="prepare_run"
        meta = read(fil,String)
        meta = split(meta,"\n")
        ii=findall(occursin.("../../",meta))
        for i in ii
            meta[i]=replace(meta[i],"../../" => "$(MITgcm_path[1])/verification/")
        end
        ii=findall(occursin.("../",meta))
        for i in ii
            meta[i]=replace(meta[i],"../" => "$(MITgcm_path[1])/verification/$(config.configuration)")
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

    params=read_all_namelists(pth_run)

    P=OrderedDict()
    P[:main]=OrderedDict(
        :category=>"verification",
        :name=>config.configuration,
        :version=>"main")
    P[:build]=OrderedDict(
        :path=>"$(MITgcm_path[1])/verification/$(config.configuration)/build",
        :options=>"-mods=../code",
        :exe=>"mitgcmuv",
        )
    push!(params,(:setup => P))

    push!(config.inputs,params...)

    return true
end


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

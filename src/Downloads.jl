module datadeps

using DataDeps, Dataverse, Glob
import DataDeps: @datadep_str
using Dataverse.downloads.Downloads

"""
    unpackDV(filepath)

Like DataDeps's `:unpack` but using `Dataverse.untargz` and remove the `.tar.gz` file.
"""    
function unpackDV(filepath; do_warn=false)
    tmp_path=Dataverse.untargz(filepath)
    tmp_path2=joinpath(tmp_path,basename(filepath)[1:end-7])
    tmp_path=(ispath(tmp_path2) ? tmp_path2 : tmp_path)
    if isdir(tmp_path)
        [mv(p,joinpath(dirname(filepath),basename(p))) for p in glob("*",tmp_path)]
        [println(joinpath(dirname(filepath),basename(p))) for p in glob("*",tmp_path)]
        rm(filepath)
    else
        rm(filepath)
        mv(tmp_path,joinpath(dirname(filepath),basename(tmp_path)))
    end
    do_warn ? println("done with unpackDV for "*filepath) : nothing
end

"""
    __init__source_code()

Register data dependency with DataDep.
"""
function __init__source_code()
    register(DataDep("mitgcmsmall","MITgcm source code (only)",
        ["https://zenodo.org/records/11515564/files/MITgcm-checkpoint68y-small.tar.gz"],
        ["ad81c755be47ed2c5fdb089e650837cff790cc6d5f1cb0c3e08339ac89cdfd7d"],      
        post_fetch_method=unpackDV))
    register(DataDep("mitgcmsmallverif","MITgcm verification experiments (subset)",
        ["https://zenodo.org/records/11515564/files/MITgcm-checkpoint68y-verif.tar.gz"],
        ["ae325ea4761373155fd9416a9f5edf8f96de7fe007712085a3ff1d115aa8cdd6"],
        post_fetch_method=unpackDV))
    register(DataDep("hs94pickup","Held and Suarez simulation pickup",
        ["https://zenodo.org/record/5422009/files/pickup_hs94.cs-32x32x5.tar.gz"],
        ["766fda741f896e1fcf42fd75c8ab1a1c4090485225fe04a303c715f33953db5e"],
        post_fetch_method=unpackDV))
    register(DataDep("darwin3oneD","Darwin3 1D examples",
        ["https://zenodo.org/records/12575686/files/Darwin3_1D_examples.tar.gz"],
        ["6bd72ec12d53474d45f44940a021005b78a0dc40a89e969acc1379bb4def0931"],
        post_fetch_method=unpackDV))
    register(DataDep("darwin3code","Darwin3 code base",
        ["https://github.com/darwinproject/darwin3/archive/refs/tags/darwin_ckpt68y.tar.gz"],
        ["9d5f9319a37ae74a4da5c3e69c6778fd7c895bcaafab5ce36245357ecd2e637f"],
        post_fetch_method=unpackDV))
end

"""
    getdata(nam::String)

Add data to the scratch space folder. Known options for `nam` include 
"mitgcmsmall", "mitgcmsmallverif", "hs94pickup", "darwin3code", "darwin3oneD"
"""
function getdata(nam::String)
    withenv("DATADEPS_ALWAYS_ACCEPT"=>true) do
        if nam=="mitgcmsmall"
            p=datadep"mitgcmsmall"
        elseif nam=="mitgcmsmallverif"
            datadep"mitgcmsmallverif"
        elseif nam=="hs94pickup"
            datadep"hs94pickup"
        elseif nam=="darwin3code"   
            datadep"darwin3code"
        elseif nam=="darwin3oneD"
            datadep"darwin3oneD"
        else
            println("unknown dataset")
        end
    end
end

function add_darwin_arm64_gfortran(p)
    pth=joinpath(p,"MITgcm","tools","build_options")
    fil=joinpath(pth,"darwin_arm64_gfortran")
    url="https://raw.githubusercontent.com/MITgcm/MITgcm/refs/heads/master/tools/build_options/darwin_arm64_gfortran"
    ispath(pth)&&!isfile(fil) ? Downloads.download(url,fil) : nothing
    fil
end

end #module datadeps

##

module MITgcmScratchSpaces

using Dataverse, Scratch
using Dataverse.downloads.Downloads

# This will be filled in inside `__init__()`
path = ""

# Downloads a resource, stores it within path
function download_dataset(url,path)
    fname = joinpath(path, basename(url))
    if !isfile(fname)
        !isdir(path) ? mkdir(path) : nothing
        Downloads.download(url, fname)
    end
    return fname
end

# 
function download_nctiles_sample()
    lst=Dataverse.file_list("doi:10.7910/DVN/3HPRZI")    
    v="ETAN"; fil=joinpath(path,v*".0001.nc")
    if !isfile(fil)
        lst1=findall([v==n[1:end-8] for n in lst.filename])
        [Dataverse.file_download(lst,v,path) for v in lst.filename[lst1]]
    end
end

function __init__()
    global path = @get_scratch!("src")
end

end #module MITgcmScratchSpaces

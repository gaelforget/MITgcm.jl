"""
    MITgcm_namelist(groups,params)

Data structure representing a MITgcm _namelist_ file, such as `data.pkg`, which contains 

```
    groups :: Array{Symbol,1} = Array{Symbol,1}(undef, 0)
    params :: Array{OrderedDict{Symbol,Any},1} = Array{OrderedDict{Symbol,Any},1}(undef, 0)
```

with model parameters (`params`) being organized in `groups` as done in the files.

```
using MITgcmTools
fil=joinpath(MITgcm_path[1],"verification","advect_xy","run","data")
nml=read_namelist(fil)
MITgcm_namelist(nml.groups,nml.params)
MITgcm_namelist(groups=nml.groups,params=nml.params)
MITgcm_namelist(groups=nml.groups)
```
"""
Base.@kwdef struct MITgcm_namelist
    groups :: Array{Symbol,1} = Array{Symbol,1}(undef, 0)
    params :: Array{OrderedDict{Symbol,Any},1} = Array{OrderedDict{Symbol,Any},1}(undef, 0)
end

import Base:read,write
read(fil::AbstractString,nml::MITgcm_namelist) = read_namelist(fil)
write(fil::AbstractString,nml::MITgcm_namelist) = write_namelist(fil,nml)

"""
    MITgcm_config()

Concrete type of `AbstractModelConfig` for `MITgcm` (as part of the `ClimateModels.jl` interface for `MITgcm`)
which contains

```
    model :: String = "MITgcm"
    configuration :: String = ""
    options :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
    inputs :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
    outputs :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
    status :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
    channel :: Channel{Any} = Channel{Any}(10) 
    folder :: String = tempdir()
    ID :: UUID = UUIDs.uuid4()
```

and can be constructed using keywords as follows

```
unknown_config=MITgcm_config(configuration="unknown")
```
"""
Base.@kwdef struct MITgcm_config <: AbstractModelConfig
    model :: String = "MITgcm"
    configuration :: String = ""
    options :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
    inputs :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
    outputs :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
    status :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
    channel :: Channel{Any} = Channel{Any}(10) 
    folder :: String = tempdir()
    #ID :: UUID = UUIDs.uuid4()
    ID :: String = ""
end

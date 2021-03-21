"""
    MITgcm_namelist

```
using MITgcmTools
fil=joinpath(MITgcm_path,"verification","advect_xy","run","data")
nml=read_namelist(fil)
MITgcm_namelist(nml.groups,nml.params)
MITgcm_namelist(groups=nml.groups,params=nml.params)
MITgcm_namelist(groups=nml.groups)
```
"""
Base.@kwdef struct MITgcm_namelist
    groups :: Array{Symbol,1} = Array{Symbol,1}(undef, 0)
    params :: Array{Dict{Symbol,Any},1} = Array{Dict{Symbol,Any},1}(undef, 0)
end

import Base:read,write
read(fil::AbstractString,nml::MITgcm_namelist) = read_namelist(fil)
write(fil::AbstractString,nml::MITgcm_namelist) = write_namelist(fil,nml)

"""
    MITgcm_config

```
using MITgcmTools
exps=verification_experiments()
MITgcm_config(exps[end]...)
```
"""
Base.@kwdef struct MITgcm_config <: AbstractModelConfig
    Model_name :: String = "MITgcm"
    Config_name :: String = ""
    status :: Array{String,1} = Array{String,1}(undef, 0)
    build_options :: Array{String,1} = Array{String,1}(undef, 0)
    runtime_options :: Array{String,1} = Array{String,1}(undef, 0)
end

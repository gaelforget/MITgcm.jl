"""
    MITgcm_namelist(groups,params)

Data structure representing a MITgcm _namelist_ file, such as `data.pkg`, which contains 

```
    groups :: Array{Symbol,1} = Array{Symbol,1}(undef, 0)
    params :: Array{OrderedDict{Symbol,Any},1} = Array{OrderedDict{Symbol,Any},1}(undef, 0)
```

with model parameters (`params`) being organized in `groups` as done in the files.

```
using MITgcm
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
    inputs :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
    outputs :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
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
    inputs :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
    outputs :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
    status :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
    channel :: Channel{Any} = Channel{Any}(10) 
    folder :: String = tempdir()
    ID :: UUID = UUIDs.uuid4()
end


"""
    MITgcm_system_check()

Output of `MITgcm.system_check`.

```
  download :: Bool = false
  complete :: Bool = false
  genmake_log :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
  genmake_state :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
  name :: String = "advect_xy"
  folder :: String = ""
  path_MITgcm :: String = ""
  path_verification :: String = ""
  NETCDF_ROOT :: String = ""
  MPI_INC_DIR :: String = ""
  mpi :: Bool = false
```
"""
Base.@kwdef struct MITgcm_system_check
  download :: Bool = false
  complete :: Bool = false
  genmake_log :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
  genmake_state :: OrderedDict{Any,Any} = OrderedDict{Any,Any}()
  name :: String = "advect_xy"
  folder :: String = ""
  path_MITgcm :: String = ""
  path_verification :: String = ""
  NETCDF_ROOT :: String = ""
  MPI_INC_DIR :: String = ""
  mpi :: Bool = false
  adj :: Bool = false
end

function Base.show(io::IO, z::MITgcm_system_check)
    zn=fieldnames(typeof(z))
    printstyled(io, " $(typeof(z)) \n",color=:normal)
    for nam in (:name,:download,:complete,:mpi)
        in(nam,zn) ? printstyled(io, "  $(nam) = ",color=:normal) : nothing
        val=getfield(z,nam)
        in(nam,zn) ? printstyled(io, "$(val)\n",color=:magenta) : nothing
    end
    for nam in (:folder,:path_MITgcm,:path_verification,:NETCDF_ROOT,:MPI_INC_DIR)
        in(nam,zn) ? printstyled(io, "  $(nam) = ",color=:normal) : nothing
        val=getfield(z,nam)
        in(nam,zn) ? printstyled(io, "$(val)\n",color=:yellow) : nothing
    end
    for nam in (:genmake_log,:genmake_state)
        in(nam,zn) ? printstyled(io, "  $(nam) = ",color=:normal) : nothing
        val=length(keys(getfield(z,nam)))
        in(nam,zn) ? printstyled(io, "OrderedDict with $(val) keys\n",color=:blue) : nothing
    end

    return
end  
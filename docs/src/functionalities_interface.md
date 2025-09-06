# Interface to MITgcm

The [ClimateModels.jl](https://github.com/gaelforget/ClimateModels.jl/#readme)'s interface is implemented in `MITgcm.jl`. 

This allows you to easily create model simulations using `MITgcm` conveniently from `Julia`.


```@example 1
using MITgcm
MITgcm_tests=MITgcm.system_check()
```

```@example 1
MC=MITgcm_config(configuration="advect_xy")
setup(MC)
#exe=joinpath(MITgcm_path[2],MC.configuration,"build","mitgcmuv") #hide
#MC.inputs[:setup][:build][:exe]=exe #hide
build(MC)
launch(MC)
```

```@example 1
sc=scan_rundir(MC)
keys(sc)
```

## Functionalities

### Main

!!! note
    The following three methods implement the [ClimateModels.jl](https://github.com/gaelforget/ClimateModels.jl/#readme)'s interface for [MITgcm](https://github.com/MITgcm/MITgcm#readme).

```@docs
setup
build
launch
```

### Tools

```@docs
system_check
set_environment_variables_to_default
default_path
MITgcm.getdata
create_script
parse_param
```

!!! note
    The following methods are imported from [ClimateModels.jl](https://github.com/gaelforget/ClimateModels.jl/#readme)and customized for [MITgcm](https://github.com/MITgcm/MITgcm#readme).

```@docs
compile
clean
```│

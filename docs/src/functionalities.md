# Manual

`MITgcmTools.jl` provides a suite of tools for running [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest), analyzing its output, and/or modifying its inputs.

## Read MITgcm Files

- [Parameter Files](@ref) (text)
- [MDSIO Files](@ref) (binary)
- [MNC Files](@ref) (netcdf)
- [Other Files](@ref)

## MITgcm Configurations

In `MITgcmTools.jl`, a model configuration is generally formulated as a `MITgcm_config` struct. It can thus easily later be run either in batch mode or interactively using the [ClimateModels.jl](https://github.com/gaelforget/ClimateModels.jl) interface. 

`verification_experiments` provides a list of standard MITgcm configurations -- those found in the `verification/` subfolder of `MITgcm_path[1]` which is also where MITgcm would compiled.

Please refer to [Detail On Configurations](@ref) for more on this.

The `setup` function can be used to prepare a temporary folder for running a `MITgcm_config` (see [ClimateModels Interface](@ref)). Interactive notebooks are found in the [Examples](@ref) section (and the `examples/` subfolder). 


## Compile & Run MITgcm

The recommended method for running the model is via the simple [ClimateModels Interface](@ref). Additional information about the `ClimateModels.jl` interface is also available at [docs@ClimateModels.jl](https://gaelforget.github.io/ClimateModels.jl/dev/)

The `MITgcm_launch` function can be used to run a `MITgcm` configuration after setting up the `MITgcm_config`. Using this interface facilitates operations like compiling and setting up a temporary folder to run the model. Key functions, incl. the climate model interface, are documented further down in the docs. 

The `verification_experiments` function provides a list of the most-standard MITgcm configurations that can all be run either in batch mode or interactively. `MITgcm_path[1]` points to where MITgcm code is installed and would be compiled. Interactive / reactive notebooks are found in the [Examples](@ref) section (e.g. `examples/MITgcm_configurations.jl` and `examples/MITgcm_run.jl`). 
 
## More Functionalities

- [Format conversions](@ref)
- [Formulae etc](@ref)
- [API Reference](@ref)


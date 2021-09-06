# Manual

`MITgcmTools.jl` provides a suite of tools for analyzing [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest) results, compiling the model, modifying inputs, running the model, etc.

## Read MITgcm Files

- [Parameter Files](@ref) (text)
- [MDSIO Files](@ref) (binary)
- [MNC Files](@ref) (netcdf)
- [Other Files](@ref)

## MITgcm Configurations

In `MITgcmTools.jl`, a model configuration is generally formulated as a `MITgcm_config` struct. It can thus easily later be run either in batch mode or interactively using the [ClimateModels.jl](https://github.com/gaelforget/ClimateModels.jl) interface. 

`verification_experiments` provides a list of standard MITgcm configurations found in the `verification/` subfolder of `MITgcm_path[1]`, which is also where MITgcm would compiled. The `setup` function can be used to prepare a temporary folder for running a `MITgcm_config`. 

Please refer to the [Detail On Configurations](@ref) and [ClimateModels Interface](@ref) sections for more on this. Interactive notebooks are found in the [Examples](@ref) section (and the `examples/` subfolder). 

## Compile & Run MITgcm

The [ClimateModels Interface](@ref) provides a convenient framework to build and run MITgcm. Additional information about the `ClimateModels.jl` interface is also available in the [ClimateModels.jl docs](https://gaelforget.github.io/ClimateModels.jl/dev/)

The `MITgcm.build` and `MITgcm.launch` functions can be used to build and run a `MITgcm` configuration after setting up the `MITgcm_config`. `MITgcm_path[1]` points to where MITgcm code is installed and will be compiled. `MITgcm_config` provides the run directory.
 
Interactive notebooks are found in the [Examples](@ref) section (and the `examples/` subfolder). 
 
## More Functionalities

- [Format conversions](@ref)
- [Formulae etc](@ref)
- [API Reference](@ref)


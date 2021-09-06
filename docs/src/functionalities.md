# Manual

`MITgcmTools.jl` provides a suite of tools for running [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest), analyzing its output, and/or modifying its inputs.

## Read MITgcm Files

```@docs
read_mdsio
read_meta
read_mnc
GridLoad_mdsio
GridLoad_mnc
```

```@docs
read_available_diagnostics
read_bin
read_flt
read_nctiles
```

```@docs
MITgcm_namelist
read_namelist
write_namelist
```

## MITgcm Configurations

The `verification_experiments()` function provides a list of the most-standard MITgcm configurations that can all be run either in batch mode or interactively using `MITgcmTools.jl`. 

The `MITgcm_path` variable points to where MITgcm is compiled. Interactive / reactive notebooks are found in the `examples/` folder (e.g. `MITgcm_run.jl`  seen just below). 

```@docs
MITgcm_path
verification_experiments
scan_rundir
```

## Compile & Run MITgcm

The recommended, simple, method to run the model is via the climate model interface (see the [ClimateModels.jl interface](@ref) section and  [docs@ClimateModels.jl](https://gaelforget.github.io/ClimateModels.jl/dev/) for detail). The `MITgcm_launch` function can be used to run a `MITgcm` configuration after setting up the `MITgcm_config`. Using this interface facilitates operations like compiling and setting up a temporary folder to run the model. Key functions, incl. the climate model interface, are documented further down in the docs. 

The `verification_experiments` function provides a list of the most-standard MITgcm configurations that can all be run either in batch mode or interactively. The `MITgcm_path` variable points to where MITgcm is compiled. Interactive / reactive notebooks are found in the `examples/` folder (e.g. `run_MITgcm.jl`  seen just below). 

## ClimateModels interface

```@docs
MITgcm_config
build
compile 
setup
MITgcm_launch
clean
```

## More Functionalities

See [Format conversions](@ref), [Formulae etc](@ref), [API Reference](@ref)


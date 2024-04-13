# Overview

## Installation Instructions

You can install the latest version of `MITgcm.jl` using the built-in [package manager](https://pkgdocs.julialang.org/). 

```
using Pkg
Pkg.add("MITgcm")
```

## Main Functionalities

`MITgcm.jl` provides a suite of tools for analyzing [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest) results, compiling the model, modifying its inputs, running simulations, and more -- all from within `julia`. Functionalities are also documented via the [Examples](@ref) section.

## MITgcm File Formats

A common use case for `MITgcm.jl` is to use and analyze model output from a previous `MITgcm` run. As an example, the [notebook from JuliaCon2021](https://juliaocean.github.io/MarineEcosystemsJuliaCon2021.jl/dev/MITgcm_tutorial_global_oce_biogeo.html) ([MITgcm\_tutorial\_global\_oce\_biogeo.jl](https://juliaocean.github.io/MarineEcosystemsJuliaCon2021.jl/dev/MITgcm_tutorial_global_oce_biogeo.jl)) reads and visualize results from the standard `MITgcm` configuration called [tutorial\_global\_oce_biogeo](https://mitgcm.readthedocs.io/en/latest/examples/global_oce_biogeo/global_oce_biogeo.html).

!!! note 
    This notebook builds and runs `tutorial_global_oce_biogeo` from within `Julia`. Alternatively, the [MITgcm documentation](https://mitgcm.readthedocs.io/en/latest/getting_started/getting_started.html) explains how to build and run tutorials at the command line in `linux`. 

[`scan_rundir`](@ref) / [`scan_stdout`](@ref) provides a summary information about what's in the model _run/_ folder. With this information, we are ready to read model output. The various files that MITgcm can generate are covered below.

- [Standard Output](@ref) (text)
- [Input Files](@ref) (text)
- [MDS Files](@ref) (binary output)
- [MNC Files](@ref) (netcdf output)
- [Grid Files](@ref) (binary or netcdf)
- [Other Files](@ref)

Grid variables are often needed for analysis. The grid output can be read from file using either [`GridLoad_mdsio`](@ref) or [`GridLoad_mnc`](@ref). This will return `Γ.XC`, `Γ.YC`, etc formated using [MeshArrays.jl](https://github.com/JuliaClimate/MeshArrays.jl). See also [`GridLoad_native`](@ref).

!!! note 
    [MITgcm\_scan\_output.jl](https://github.com/gaelforget/MITgcm.jl/blob/master/examples/MITgcm_scan_output.jl) does this in bulk for all configurations in `MITgcm/verification` and further displays each grid ([this page](https://gaelforget.github.io/MITgcm.jl/dev/examples/MITgcm_scan_output.html)).

## Model Configurations

The [`verification_experiments`](@ref) function lists standard model configurations found in the `joinpath(MITgcm_path[1],"verification")` folder. 

Each model configuration has a subfolder where MITgcm can be compiled and run. 

In `MITgcm.jl`, a model configuration is represented as a [`MITgcm_config`](@ref). This data structure allows you take advantage of the [ClimateModels.jl](https://github.com/gaelforget/ClimateModels.jl) interface for example.

The [`setup`](@ref) function can thus be used to prepare a temporary run directory for the `MITgcm_config`; then [`build`](@ref) to compile the model, and [`MITgcm_launch`](@ref) to run it.

!!! note
    For more on these aspects, see [Examples](@ref), [More On Configurations](@ref), and [ClimateModels Interface](@ref).

## More Functionalities

Interactive notebooks can be found in the [Examples](@ref) section (and the `examples/` subfolder). They demonstrate functionalities like plotting with [Makie.jl](https://makie.juliaplots.org/stable/) and particle tracking with  [IndividualDisplacements.jl](https://github.com/JuliaClimate/IndividualDisplacements.jl).

And more documentation is also found here:

- [API Reference](@ref)
- [Format Conversions](@ref)
- [Formulae etc](@ref)


## API Reference

```@index
```


# Overview

`MITgcm.jl` provides supports the analysis of [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest) results in `Julia`. It can also compile a model configuration, edit model parameters, and run model simulations from within `julia`. 

Functionalities are documented in the coming sections, and in the [Examples](@ref).

## Installation Instructions

You can install the latest version of `MITgcm.jl` using the built-in [package manager](https://pkgdocs.julialang.org/). 

```
using Pkg
Pkg.add("MITgcm")
```

!!! tip
	The [ECCO-Docker](https://github.com/gaelforget/ECCO-Docker#readme) _image_ has `MITgcm.jl` pre-installed, as well as `gfortran`, `MPI`, and `NetCDF` allowing to run any `MITgcm` configuration. The [ECCO-Binder](https://mybinder.org/v2/gh/gaelforget/ECCO-Docker/HEAD) _instance_ (free, but small) is available to try functionalities in the cloud.

## MITgcm File Formats

A common use case for `MITgcm.jl` is to use and analyze model output from a previous `MITgcm` run. As an example, the [notebook from JuliaCon2021](https://juliaocean.github.io/MarineEcosystemsJuliaCon2021.jl/dev/MITgcm_tutorial_global_oce_biogeo.html) ([MITgcm\_tutorial\_global\_oce\_biogeo.jl](https://juliaocean.github.io/MarineEcosystemsJuliaCon2021.jl/dev/MITgcm_tutorial_global_oce_biogeo.jl)) reads and visualize results from the standard `MITgcm` configuration called [tutorial\_global\_oce_biogeo](https://mitgcm.readthedocs.io/en/latest/examples/global_oce_biogeo/global_oce_biogeo.html).

!!! note 
    This notebook builds and runs `tutorial_global_oce_biogeo` from within `Julia`. Alternatively, the [MITgcm documentation](https://mitgcm.readthedocs.io/en/latest/getting_started/getting_started.html) explains how to build and run tutorials at the command line in `linux`. 

A typical `MITgcm` run stores model output within the _run/_ folder, including the standard _STDOUT_ text files. [`scan_rundir`](@ref) / [`scan_stdout`](@ref) provides a summary information about what's in the _run/_ folder. With this metadata, we are ready to read model output. The various file formats that `MITgcm` can generate are covered below.

- [Standard Output](@ref) (text)
- [Input Files](@ref) (text)
- [MDS Files](@ref) (binary output)
- [MNC Files](@ref) (netcdf output)
- [Grid Files](@ref) (binary or netcdf)
- [Other Files](@ref)

Grid variables are often needed for analysis. The grid output can be read from file using either [`GridLoad_mdsio`](@ref) or [`GridLoad_mnc`](@ref). This will return `Γ.XC`, `Γ.YC`, etc formated using [MeshArrays.jl](https://github.com/JuliaClimate/MeshArrays.jl). See also [`GridLoad_native`](@ref).

!!! note 
    The [MITgcm\_scan\_output.jl](https://github.com/gaelforget/MITgcm.jl/blob/master/examples/MITgcm_scan_output.jl) notebook does this in bulk for all configurations in `MITgcm/verification` and displays the gridded model domain for each model configuration ([this page](https://gaelforget.github.io/MITgcm.jl/dev/examples/MITgcm_scan_output.html)).

## MITgcm Configurations

In `MITgcm.jl`, a model configuration is represented as a [`MITgcm_config`](@ref). This data structure allows you take advantage of the [ClimateModels.jl](https://github.com/gaelforget/ClimateModels.jl) interface for example. [`setup`](@ref) can prepare a temporary directory for the `MITgcm_config` to run in. Then [`build`](@ref) can compile the model, and [`MITgcm_launch`](@ref) run it.

The [`verification_experiments`](@ref) function provides a list of standard model configurations. Each one has a subfolder in `joinpath(MITgcm_path[1],"verification")` where MITgcm can be compiled as `mitgcmuv`. 

!!! note
    For more on these aspects, see [Examples](@ref), [Model Configurations](@ref), and [ClimateModels Interface](@ref).

Interactive notebooks can be found in the [Examples](@ref) section (and the `examples/` subfolder). They demonstrate functionalities like plotting with [Makie.jl](https://makie.juliaplots.org/stable/) and particle tracking with  [IndividualDisplacements.jl](https://github.com/JuliaClimate/IndividualDisplacements.jl).

## API Reference

```@index
```


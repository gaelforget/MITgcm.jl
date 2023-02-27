# Manual

`MITgcmTools.jl` provides a suite of tools for analyzing [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest) results, compiling the model, modifying its inputs, running simulations, etc all from within `julia`. Much of the functionalities are documented via the [Examples](@ref) section, as highlighted hereafter.

## Read / Write MITgcm Files

Let's assume that user has run an `MITgcm` configuration in standard fashion, and wants to examine the output in `Julia`. This is one of the common use case for `MITgcmTools.jl`.

The [tutorial\_global\_oce_biogeo](https://mitgcm.readthedocs.io/en/latest/examples/global_oce_biogeo/global_oce_biogeo.html), which comes with MITgcm, provides a representative example. With `MITgcmTool.jl` you can just run it in [a notebook](https://juliaocean.github.io/MarineEcosystemsJuliaCon2021.jl/dev/MITgcm_tutorial_global_oce_biogeo.html) ([MITgcm\_tutorial\_global\_oce\_biogeo.jl](https://juliaocean.github.io/MarineEcosystemsJuliaCon2021.jl/dev/MITgcm_tutorial_global_oce_biogeo.jl)).

!!! note 
    Alternatively, the [MITgcm documentation](https://mitgcm.readthedocs.io/en/latest/getting_started/getting_started.html) explains how to complile and run tutorials at the linux command line. 

Once the model has run, it's output is found in the _run/_ subfolder (see notebook). The [`scan_rundir`](@ref) can be used to take a quick look inside.

```
rundir=joinpath(pth,"run")
sc=scan_rundir(rundir)
sc.completed
```

The result will indicate whether the model run has complete normally based upon the standard output file (_output.txt_ or _STDOUT.000_). 

[`scan_rundir`](@ref) gathers additional information by scanning files in the _run/_ folder. For example, it checks whether the output is binary (`sc.params_files.use_mdsio`) or netcdf (`sc.params_files.use_mnc`), and what type of grid was used (`sc.params_grid`).

With this information in hand, we are ready to read model output. The various files that MITgcm can generate are covered below.

- [Run Folder](@ref)
- [MDS Files](@ref) (binary)
- [MNC Files](@ref) (netcdf)
- [Grid Files](@ref) (binary or netcdf)
- [Standard Output](@ref) (text)
- [Input Files](@ref) (text)
- [Other Files](@ref)

Grid variables are often needed for analysis. The grid output can be read from file using either [`GridLoad_mdsio`](@ref) or [`GridLoad_mnc`](@ref). This will return `Γ.XC`, `Γ.YC`, etc formated using [MeshArrays.jl](https://github.com/JuliaClimate/MeshArrays.jl). See also [`GridLoad_native`](@ref).

!!! note 
    [MITgcm\_scan\_output.jl](https://github.com/gaelforget/MITgcmTools.jl/blob/master/examples/MITgcm_scan_output.jl) does this in bulk for all configurations in `MITgcm/verification` and further displays each grid ([this page](https://gaelforget.github.io/MITgcmTools.jl/dev/examples/MITgcm_scan_output.html)).

## MITgcm Configurations

The [`verification_experiments`](@ref) function lists standard model configurations found in the `joinpath(MITgcm_path[1],"verification")` folder. 

Each model configuration has a subfolder where MITgcm can be compiled and run. 

In `MITgcmTools.jl`, a model configuration is represented as a [`MITgcm_config`](@ref). This data structure allows you take advantage of the [ClimateModels.jl](https://github.com/gaelforget/ClimateModels.jl) interface for example.

The [`setup`](@ref) function can thus be used to prepare a temporary run directory for the `MITgcm_config`; then [`build`](@ref) to compile the model, and [`MITgcm_launch`](@ref) to run it.

!!! note
    For more on these aspects, see [Examples](@ref), [More On Configurations](@ref), and [ClimateModels Interface](@ref).

## More Functionalities

Interactive notebooks can be found in the [Examples](@ref) section (and the `examples/` subfolder). They demonstrate functionalities like plotting with [Makie.jl](https://makie.juliaplots.org/stable/) and particle tracking with  [IndividualDisplacements.jl](https://github.com/JuliaClimate/IndividualDisplacements.jl).

And more documentation is also found here:

- [API Reference](@ref)
- [Format Conversions](@ref)
- [Formulae etc](@ref)


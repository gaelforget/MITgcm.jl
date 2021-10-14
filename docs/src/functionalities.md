# Manual

`MITgcmTools.jl` provides a suite of tools for analyzing [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest) results, compiling the model, modifying its inputs, running simulations, etc all from within `julia`. Much of the functionalities are documented via the [Examples](@ref) section, as highlighted hereafter.

## Read / Write MITgcm Files

Let's assume that user has run an `MITgcm` configuration in standard fashion, and wants to examine the output in `Julia`. This is one of the common use case for `MITgcmTools.jl`.

The [tutorial\_global\_oce_biogeo](https://mitgcm.readthedocs.io/en/latest/examples/global_oce_biogeo/global_oce_biogeo.html) provided with MITgcm is a representative example. The [MITgcm documentation](https://mitgcm.readthedocs.io/en/latest/getting_started/getting_started.html) explains how to complile and run the tutorials. Alternatively, you can just run [this notebook](https://juliaocean.github.io/MarineEcosystemsJuliaCon2021.jl/dev/MITgcm_tutorial_global_oce_biogeo.html) ([MITgcm\_tutorial\_global\_oce\_biogeo.jl](https://juliaocean.github.io/MarineEcosystemsJuliaCon2021.jl/dev/MITgcm_tutorial_global_oce_biogeo.jl)).

Once the model has run, it's output is found in the _run/_ subfolder (of, say, folder `$(pth)`). The choice of `$(pth)` is up to the user. The first command one may want to run at this stage is [scan_rundir](@ref).

```
rundir=joinpath(pth,"run")
sc=scan_rundir(rundir)
sc.completed
```

will indicate whether the model run has complete normally based upon the ending of the standard output file (_output.txt_ or _STDOUT.000_). [scan_rundir](@ref) returns additional information from scanning the files in _run/_; e.g., whether the output is binary (`sc.params_files.use_mdsio`) or netcdf (`sc.params_files.use_mnc`), and what type of grid was used (`sc.params_grid`).

With this information in hand, one should be ready to read further model output. For example, one can read grid variables using either `Γ=GridLoad_mdsio(rundir)` or `Γ=GridLoad_mnc(rundir)` which will return `Γ.XC`, `Γ.YC`, etc formated using [MeshArrays.jl](https://github.com/JuliaClimate/MeshArrays.jl). [MITgcm\_scan\_output.jl](https://github.com/gaelforget/MITgcmTools.jl/blob/master/examples/MITgcm_scan_output.jl) does this in bulk for all configurations in `MITgcm/verification` and further displays each grid ([this page](https://gaelforget.github.io/MITgcmTools.jl/dev/examples/MITgcm_scan_output.html)).

For more on the various read / write functions, please refer to the following sections.

- [Parameter Files](@ref) (text)
- [MDSIO Files](@ref) (binary)
- [MNC Files](@ref) (netcdf)
- [Other Files](@ref)

## MITgcm Configurations

`verification_experiments` provides a list of standard MITgcm configurations found in the `verification/` subfolder of `MITgcm_path[1]`, which is also where MITgcm typically gets compiled. 

In `MITgcmTools.jl`, a model configuration is generally formulated as a `MITgcm_config` struct, which let's the user take advantage of the [ClimateModels.jl](https://github.com/gaelforget/ClimateModels.jl) interface.

The [setup](@ref) function can then be used to prepare a temporary run directory for the chosen `MITgcm_config`, [build](@ref) to compile the model within `MITgcm_path[1]`, and [MITgcm_launch](@ref) to run the model in the temporary run folder for `MITgcm_config`. 

Please refer to the [Examples](@ref), [More On Configurations](@ref), and [ClimateModels Interface](@ref) sections for further documentation of these aspects. Additional information about the [ClimateModels.jl](https://github.com/gaelforget/ClimateModels.jl) interface is also available in the [ClimateModels.jl docs](https://gaelforget.github.io/ClimateModels.jl/dev/).

## More Functionalities

Interactive notebooks found in the [Examples](@ref) section (and the `examples/` subfolder) demonstrate these functionalities. These cover other functionalities like plotting with [Makie.jl](https://makie.juliaplots.org/stable/) and using [IndividualDisplacements.jl](https://github.com/JuliaClimate/IndividualDisplacements.jl) in particle tracking applications.

More documentation can also be found in these docs:

- [Format Conversions](@ref)
- [Formulae etc](@ref)
- [API Reference](@ref)


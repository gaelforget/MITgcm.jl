# Overview

`MITgcm.jl` allows the analysis of [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest) results in `Julia`. It can also setup, build, and launch a chosen model configuration ([`MITgcm_config`](@ref)) from within `julia`. 

Functionalities are documented in the coming sections, and in [Examples, Notebooks](@ref).

## Getting Started

Installing the latest version of `MITgcm.jl` with the built-in [package manager](https://pkgdocs.julialang.org/) is the recommended method. 

```
using Pkg
Pkg.add("MITgcm")

using MITgcm
system_check(setenv=true)
```

!!! note
    The first time you do this, `MITgcm.jl` downloads a few `MITgcm` fortran code folders. This may take a few seconds to a few minutes depending on your network's performance.

## Running MITgcm

Let's start running MITgcm interactively. 

The first command defines a data structure, [`MITgcm_config`](@ref), that we can then use from Julia. The [`setup`](@ref) command, by default, creates a folder in your `tempdir()` to run `MITgcm`.

```@example 0
using MITgcm
MC=MITgcm_config(configuration="tutorial_held_suarez_cs")
setup(MC)
exe=joinpath(MITgcm.default_path(),"verification",MC.configuration,"build","mitgcmuv") #hide
show(MC)
```

After `setup`, the standard workflow is to call [`build`](@ref) and then [`launch`](@ref). 

Next we can use the `log` method to get a status report. 

```@example 0
build(MC)
launch(MC)
log(MC)
```

If we have a previous build of `MITgcm` for this then we can specify the file path as a parameter, `:exe`. 

If `:exe` is not specified, or if no file is found at the specified path, [`build`](@ref) will attempt to build the model for us.

```@example 0
exe=joinpath(default_path(),"verification",MC.configuration,"build","mitgcmuv")
MC.inputs[:setup][:build][:exe]=exe
```

!!! tip
    - For longer `MITgcm` simulations run, users often prefer to use a queuing system or batch script (not an interactive session).
    - [`setup`](@ref) can generate and submit a batch script, via [`create_script`](@ref) and `config.inputs[:setup][:main][:command] = "qsub submit.csh"`

## Using Model Output

As `MITgcm` users, we often want to read and visualise model output from an earlier model run. To this end, `MITgcm.jl` provides methods to read the various file formats that `MITgcm` generates.

Read example:

```@example 0
T=read_mdsio(joinpath(MC,"run"),"T.0000276496")
size(T)
```

Plot example:

```@example 0
using CairoMakie

fig, ax, hm = heatmap(T[1:32,:,1])
ax.title="temperature"
Colorbar(fig[1, 2], hm)

fig
```

!!! tip
    For more use cases, see [MeshArrays.jl](https://github.com/juliaclimate/MeshArrays.jl#readme), [Drifters.jl](https://github.com/juliaclimate/Drifters.jl#readme), [Climatology.jl](https://github.com/juliaocean/Climatology.jl#readme).
    - [geography notebook](https://juliaclimate.github.io/MeshArrays.jl/dev/tutorials/geography.html) and [vector tutorial](https://juliaclimate.github.io/MeshArrays.jl/dev/tutorials/vectors.html) present generic recipes, readily applicable to most MITgcm configurations
    - [ocean pathways](https://juliaclimate.github.io/Drifters.jl/dev/examples/global_ocean_circulation.html) can also be computed from MITgcm output
    - [climatology notebook](https://juliaocean.github.io/Climatology.jl/dev/examples/ECCO_standard_plots.html) shows a whole set of ocean variables, transports, etc computed from global MITgcm solutions (ECCO4 and OCCA2)

## MITgcm File Formats

`MITgcm` stores model output within a _run/_ folder, such as the standard _STDOUT_ text files, and other file formats listed below. [`scan_run_dir`](@ref) can be used to provide a summary of what's in the _run/_ folder. For more see:

- [Standard Output](@ref) (text)
- [Input Files](@ref) (text)
- [MDS Files](@ref) (binary output)
- [MNC Files](@ref) (netcdf output)
- [Grid Files](@ref) (binary or netcdf)
- [Other Files](@ref)

Grid variables are often needed for analysis. They can be read from file using either [`GridLoad_mdsio`](@ref) or [`GridLoad_mnc`](@ref). This will return `Γ.XC`, `Γ.YC`, etc formated using [MeshArrays.jl](https://github.com/JuliaClimate/MeshArrays.jl). See also [`GridLoad_native`](@ref).

!!! note 
    The [MITgcm\_scan\_output.jl](https://github.com/gaelforget/MITgcm.jl/blob/master/examples/MITgcm_scan_output.jl) notebook does this in bulk for all configurations in `MITgcm/verification` and displays the gridded model domain for each model configuration ([this page](https://gaelforget.github.io/MITgcm.jl/dev/examples/MITgcm_scan_output.html)).

## MITgcm Configurations

`MITgcm.jl` represents a model configuration using [`MITgcm_config`](@ref). This data structure allows you take advantage of the [ClimateModels.jl](https://github.com/gaelforget/ClimateModels.jl) interface for example. 

- [`setup`](@ref) prepares a run directory for the `MITgcm_config`
- [`build`](@ref) compiles the model (if needed)
- [`launch`](@ref) starts the model run

The [`scan_verification`](@ref) function provides a list of standard model configurations. Each one has a subfolder in `MITgcm_path[2]` which gets compiled using source code from `MITgcm_path[1]`.

!!! note
    For more on these aspects, see [Examples](@ref), [Model Configurations](@ref), and [ClimateModels Interface](@ref).

Interactive notebooks can be found in the [Examples](@ref) section (and the `examples/` subfolder). They demonstrate functionalities like plotting with [Makie.jl](https://makie.juliaplots.org/stable/) and particle tracking with  [Drifters.jl](https://github.com/JuliaClimate/Drifters.jl).

## Troubleshooting

The [`system_check`](@ref) method will try running MITgcm and report back. If the result is negative for any particular item, you may want to consult the [MITgcm documentation](http://mitgcm.readthedocs.io/en/latest/?badge=latest) for more guidance.

```@example 0
using MITgcm
MITgcm.system_check(setenv=true)
```

- [`MITgcm.setenv()`](@ref) can be used to set `NETCDF_ROOT` and `MPI_INC_DIR` to specified values.
- [`MITgcm.set_environment_variables_to_default()`](@ref) can be used to set `NETCDF_ROOT` and `MPI_INC_DIR` to default values.
- [`scan_build_dir`](@ref),  [`scan_run_dir`](@ref), and [`monitor`](@ref) can be used to inspect the experiment folders.
		
!!! tip
    - Building and running MITgcm requires a [fortran compiler](https://fortran-lang.org/learn/os_setup/install_gfortran). Some configurations further require installing [MPI](https://mitgcm.readthedocs.io/en/latest/getting_started/getting_started.html?highlight=mpi_INC_DIR#building-with-mpi) and [NetCDF](https://mitgcm.readthedocs.io/en/latest/outp_pkgs/outp_pkgs.html?highlight=NetCDF#netcdf-i-o-pkg-mnc) libraries.
	 - The [ECCO-Docker](https://github.com/gaelforget/ECCO-Docker#readme) _image_ has `MITgcm.jl` pre-installed, as well as `gfortran`, `MPI`, and `NetCDF` allowing to run any `MITgcm` configuration. The [ECCO-Binder](https://mybinder.org/v2/gh/gaelforget/ECCO-Docker/HEAD) _instance_ (free, but small) is available to try functionalities in the cloud.

# Overview

`MITgcm.jl` allows the analysis of [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest) results in `Julia`. It can also setup, build, and launch a chosen model configuration ([`MITgcm_config`](@ref)) from within `julia`. 

Functionalities are documented in the coming sections, and in [Examples, Notebooks](@ref).

## Getting Started

Installing the latest version of `MITgcm.jl` with the built-in [package manager](https://pkgdocs.julialang.org/) is the recommended method. 

```
using Pkg
Pkg.add("MITgcm")
```

### Running MITgcm

Let's start running MITgcm interactively. 

The first command defines a data structure, [`MITgcm_config`](@ref), that we can then use from Julia. The [`setup`](@ref) command, by default, creates a folder in your `tempdir()` to run `MITgcm`.

```@example 0
using MITgcm
MC=MITgcm_config(configuration="tutorial_held_suarez_cs")
setup(MC)
exe=joinpath(MITgcm.default_path(),"verification",MC.configuration,"build","mitgcmuv") #hide
show(MC)
```

After `setup`, the standard workflow is to call [`build`](@ref) and then [`MITgcm_launch`](@ref). 

Next we can use the `log` method to get a status report. 

```@example 0
build(MC)
launch(MC)
log(MC)
```

If we have a previous build of `MITgcm` for this then we can specify the file path as a parameter, `:exe`. 

If `:exe` is not specified, or if no file is found at the specified path, `build`](@ref) will attempt to build the model for us.

```@example 0
exe=joinpath(MITgcm.default_path(),"verification",
	MC.configuration,"build","mitgcmuv")
MC.inputs[:setup][:build][:exe]=exe
```

!!! tip
    - For longer `MITgcm` simulations run, users often prefer to use a queuing system or batch script (not an interactive session).
    - [`setup`](@ref) can generate a submission script for this, via [`create_script`](@ref)
    - `config.inputs[:setup][:main][:command] = "qsub submit.csh"`


### Using Model Output

As `MITgcm` users, we often want to read and analize model output from an earlier model run. To this end, `MITgcm.jl` provides methods to read the various file formats that `MITgcm` generates.

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
Colorbar(fig[:, 1], hm)

fig
```

!!! tip
    For more use cases, see [Climatology.jl](https://github.com/juliaocean/Climatology.jl#readme) , [MeshArrays.jl](https://github.com/juliaclimate/MeshArrays.jl#readme), [IndividualDisplacements.jl](https://github.com/juliaclimate/IndividualDisplacements.jl#readme).

## Main Features

- MITgcm File Formats
- MITgcm Configurations

### MITgcm File Formats

`MITgcm.jl` can be used to analyze model output from a previous `MITgcm` run. `MITgcm` stores model output within a _run/_ folder. This includes the standard _STDOUT_ text files, and other file formats listed below. [`scan_rundir`](@ref) can be used to provide a summary of what's in the _run/_ folder. 

- [Standard Output](@ref) (text)
- [Input Files](@ref) (text)
- [MDS Files](@ref) (binary output)
- [MNC Files](@ref) (netcdf output)
- [Grid Files](@ref) (binary or netcdf)
- [Other Files](@ref)

Grid variables are often needed for analysis. The grid output can be read from file using either [`GridLoad_mdsio`](@ref) or [`GridLoad_mnc`](@ref). This will return `Γ.XC`, `Γ.YC`, etc formated using [MeshArrays.jl](https://github.com/JuliaClimate/MeshArrays.jl). See also [`GridLoad_native`](@ref).

!!! note 
    The [MITgcm\_scan\_output.jl](https://github.com/gaelforget/MITgcm.jl/blob/master/examples/MITgcm_scan_output.jl) notebook does this in bulk for all configurations in `MITgcm/verification` and displays the gridded model domain for each model configuration ([this page](https://gaelforget.github.io/MITgcm.jl/dev/examples/MITgcm_scan_output.html)).

### MITgcm Configurations

In `MITgcm.jl`, a model configuration is represented as a [`MITgcm_config`](@ref). This data structure allows you take advantage of the [ClimateModels.jl](https://github.com/gaelforget/ClimateModels.jl) interface for example. [`setup`](@ref) can prepare a temporary directory for the `MITgcm_config` to run in. Then [`build`](@ref) can compile the model, and [`MITgcm_launch`](@ref) run it.

The [`verification_experiments`](@ref) function provides a list of standard model configurations. Each one has a subfolder in `joinpath(MITgcm_path[1],"verification")` where MITgcm can be compiled as `mitgcmuv`. 

!!! note
    For more on these aspects, see [Examples](@ref), [Model Configurations](@ref), and [ClimateModels Interface](@ref).

Interactive notebooks can be found in the [Examples](@ref) section (and the `examples/` subfolder). They demonstrate functionalities like plotting with [Makie.jl](https://makie.juliaplots.org/stable/) and particle tracking with  [IndividualDisplacements.jl](https://github.com/JuliaClimate/IndividualDisplacements.jl).

## Troubleshooting

The [`system_check`](@ref) method will try running MITgcm and report back. If the result is negative for any particular item, you may want to consult the [MITgcm documentation](http://mitgcm.readthedocs.io/en/latest/?badge=latest) for more guidance.

```@example 0
using MITgcm
MITgcm.system_check(setenv=true)
```

The [`set_environment_variables_to_default()`](@ref) method can be used to set `NETCDF_ROOT` and `MPI_INC_DIR` to default values.

The [`scan_rundir`](@ref) method can be used to inspect the run directory of an experiment.
		
!!! tip
    - Building and running MITgcm requires a [fortran compiler](https://fortran-lang.org/learn/os_setup/install_gfortran). Some configurations further require installing [MPI](https://mitgcm.readthedocs.io/en/latest/getting_started/getting_started.html?highlight=mpi_INC_DIR#building-with-mpi) and [NetCDF](https://mitgcm.readthedocs.io/en/latest/outp_pkgs/outp_pkgs.html?highlight=NetCDF#netcdf-i-o-pkg-mnc) libraries.
	 - The [ECCO-Docker](https://github.com/gaelforget/ECCO-Docker#readme) _image_ has `MITgcm.jl` pre-installed, as well as `gfortran`, `MPI`, and `NetCDF` allowing to run any `MITgcm` configuration. The [ECCO-Binder](https://mybinder.org/v2/gh/gaelforget/ECCO-Docker/HEAD) _instance_ (free, but small) is available to try functionalities in the cloud.

```@setup 1
using MITgcm
```

!!! note
	Compiling MITgcm requires [a fortran compiler](https://fortran-lang.org/learn/os_setup/install_gfortran). This is a requirement for all notebooks except `MITgcm_configurations.jl`.

# Standard Modeling Workflow

Here is the standard way to run a model configuration using `MITgcm.jl`.

```@example 1
using MITgcm
MITgcm_download()
MC=MITgcm_config(configuration="advect_xy")
```

```@example 1
setup(MC)
build(MC,"--allow-skip")
log(MC)
```

```@example 1
MC.inputs
```

```@example 1
MITgcm_launch(MC)
readdir(MC,"run")
```

```@example 1
rundir=joinpath(MC,"run")
sc=scan_rundir(rundir)
sc.params_grid
```

# Pluto Notebooks

Below are links to static html versions of the examples which one can open with a web browser.

If instead you wanted to run the notebooks using [Pluto.jl](https://plutojl.org), then you might proceed as follows:

1. open `julia` in terminal window
2. type the commands shown below at the `Julia` prompt
3. in web-browser, open one of the notebooks' _code link_ using the [Pluto interface](https://github.com/fonsp/Pluto.jl/wiki/🔎-Basic-Commands-in-Pluto).

```
using Pluto
Pluto.run(notebook="examples/MITgcm_configurations.jl")
```

Alternatively, you can run an example at the command line as, e.g., 

```
julia -e 'using Pluto
notebook="examples/MITgcm_configurations.jl"
Pluto.activate_notebook_environment(notebook)
using Pkg; Pkg.instantiate()
include(notebook)
'
```

# Notebooks Listing

- [MITgcm_configurations.jl](MITgcm_configurations.html) ([code link](https://raw.githubusercontent.com/gaelforget/MITgcm.jl/master/examples/MITgcm_configurations.jl)); explore MITgcm configurations and their parameters.
- [MITgcm\_scan\_output.jl](MITgcm_scan_output.html) ([code link](https://raw.githubusercontent.com/gaelforget/MITgcm.jl/master/examples/MITgcm_scan_output.jl)) : scan run directory, standard output, read grid files, and vizualize. 
- [MITgcm_run.jl](MITgcm_run.html) ([code link](https://raw.githubusercontent.com/gaelforget/MITgcm.jl/master/examples/MITgcm_run.jl)) : a detailed look into compiling and running the model.
- [MITgcm_worklow.jl](MITgcm_worklow.html) ([code link](https://raw.githubusercontent.com/gaelforget/MITgcm.jl/master/examples/MITgcm_worklow.jl)): build, setup, run, and plot for a chosen standard MITgcm configuration.

!!! note
	The `HS94*` series of examples need to be run in sequence, as they rely on output from one another. This tutorial runs the [Held and Suarez 94](https://mitgcm.readthedocs.io/en/latest/overview/global_atmos_hs.html) benchmark	 with MITgcm on a cube sphere grid, and illustrates particle tracking in the Atmosphere using	[MeshArrays.jl](https://juliaclimate.github.io/MeshArrays.jl/dev/) and [IndividualDisplacements.jl](https://juliaclimate.github.io/IndividualDisplacements.jl/dev/).

- [HS94_animation.jl](HS94_animation.html) ([code link](https://raw.githubusercontent.com/gaelforget/MITgcm.jl/master/examples/HS94_animation.jl)) : run `hs94.cs-32x32x5` configuration, read output, interpolate, and plot maps.
- [HS94_particles.jl](HS94_particles.html) ([code link](https://raw.githubusercontent.com/gaelforget/MITgcm.jl/master/examples/HS94_particles.jl)) : compute particle trajectories from `hs94.cs-32x32x5` output generated earlier.
- [HS94_Makie.jl](HS94_Makie.html) ([code link](https://raw.githubusercontent.com/gaelforget/MITgcm.jl/master/examples/HS94_Makie.jl)) : using `Makie.jl` instead of `Plots.jl`


# Examples

Below are links to static html versions of the examples which one can open with a web browser.

If instead you wanted to run the notebooks using [Pluto.jl](https://plutojl.org), then you might proceed as follows:

1. open `julia` in terminal window
2. type the commands shown below at the `Julia` prompt
3. in web-browser, open one of the notebooks listed hereafter using the [Pluto interface](https://github.com/fonsp/Pluto.jl/wiki/🔎-Basic-Commands-in-Pluto).

```
cd("examples/")
using Pluto
Pluto.run()
```

Alternatively, you can run an example at the command line as, e.g., `julia examples/HS94_animation.jl`, assuming that all requirements (e.g., packages + gfortran) for the chosen example are already installed.

## Examples List

!!! note
	Compiling MITgcm requires [a fortran compiler](https://fortran-lang.org/learn/os_setup/install_gfortran). This is a requirement for all notebooks except `MITgcm_configurations.jl`.

- [MITgcm_configurations.jl](MITgcm_configurations.html) ; explore MITgcm configurations and their parameters.
- [MITgcm_run.jl](MITgcm_run.html) : a detailed look into compiling and running the model.
- [MITgcm_worklow.jl](MITgcm_worklow.html) : build, setup, run, and plot for a chosen standard MITgcm configuration.
- [HS94_animation.jl](HS94_animation.html) : run `hs94.cs-32x32x5` configuration, read output, interpolate, and plot maps.
- [HS94_particles.jl](HS94_particles.html) : compute particle trajectories from `hs94.cs-32x32x5` output generated earlier.

!!! warning
	Links for `HS94_Makie.jl` and `MITgcm_scan_output.jl` point to notebook files rather than generated html files as `GLMakie.jl` could not be compiled as needed for that process.

- [HS94_Makie.jl](https://raw.githubusercontent.com/gaelforget/MITgcmTools.jl/master/examples/HS94_Makie.jl) : using `Makie.jl` instead of `Plots.jl`
- [MITgcm\_scan\_output.jl](https://raw.githubusercontent.com/gaelforget/MITgcmTools.jl/master/examples/MITgcm_scan_output.jl) : scan `output.txt`, read grid, vizualize with `Makie.jl` 

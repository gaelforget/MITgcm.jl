
# Examples

Below are links to static versions of the examples which one can open these a web browser.

If instead one wants to run the notebooks using [Pluto.jl](https://plutojl.org), then they might proceed as follows:

1. open `julia` in terminal window
2. type the commands below at the `Julia` prompt
3. in web-browser, open one of the notebooks listed hereafter

```
cd("examples/")
using Pluto
Pluto.run()
```

Within the [Pluto interface](https://github.com/fonsp/Pluto.jl/wiki/🔎-Basic-Commands-in-Pluto), one can run the notebooks listed hereafter. Compiling MITgcm, however, requires [a fortran compiler](https://fortran-lang.org/learn/os_setup/install_gfortran). This is a requirement for all notebooks except `MITgcm_configurations.jl`.

- [MITgcm_configurations.jl](MITgcm_configurations.html)
- [MITgcm_run.jl](MITgcm_run.html)
- [MITgcm_worklow.jl](MITgcm_worklow.html)
- [HS94_plotmap.jl](HS94_plotmap.html)
- [HS94_particles.jl](HS94_particles.html)
- [HS94_Makie.jl](https://raw.githubusercontent.com/gaelforget/MITgcmTools.jl/master/examples/HS94_Makie.jl)
- [MITgcm\_scan\_output.jl](https://raw.githubusercontent.com/gaelforget/MITgcmTools.jl/master/examples/MITgcm_scan_output.jl)

!!! warning
	Links for `HS94_Makie.jl` and `MITgcm_scan_output.jl` 
point to notebook files rather than generated html files as `GLMakie.jl` could not be compiled as needed for that process.

## Examples / Running Models
 
- `MITgcm_configurations.jl` : explore MITgcm configurations and their parameters.
- `MITgcm_worklow.jl` : build, setup, run, and plot for any standard configuration.
- `run_MITgcm.jl` : a more detailed look into compiling and running the model.

## Examples / Analyzing Results

- `HS94_plotmap.jl` : read `hs94.cs-32x32x5` output, interpolate, and plot map
- `HS94_particles.jl` : compute particle trajectories from `hs94.cs-32x32x5` output
- `HS94_Makie.jl` : example using `Makie.jl` instead of `Plots.jl`
- `MITgcm_scan_output.jl` : scan `output.txt`, read grid, viz with `Makie.jl` 


# Examples

Below are links to static versions of the examples which one can just open these in the web browser.

If you instead want to run the notebooks using [Pluto.jl](https://plutojl.org) then youn could proceed as follows. 

1. open `julia` in terminal window
2. type the following commands at the `Julia` prompt
3. in web-browser, open one of the notebooks listed hereafter

```
cd("examples/")
using Pluto
Pluto.run()
```

Within the [Pluto interface](https://github.com/fonsp/Pluto.jl/wiki/🔎-Basic-Commands-in-Pluto), you can then open and run any of the following notebooks. Compiling, however, MITgcm requires [a fortran compiler](https://fortran-lang.org/learn/os_setup/install_gfortran). This is a requirement for all notebooks except `MITgcm_configurations.jl`.

- [MITgcm_configurations.jl](MITgcm_configurations.html)
- [MITgcm_run.jl](MITgcm_run.html)
- [MITgcm_worklow.jl](MITgcm_worklow.html)
- [HS94_plotmap.jl](HS94_plotmap.html)
- [HS94_particles.jl](HS94_particles.html)
- [HS94_Makie.jl](HS94_Makie.html)
- [MITgcm\_scan\_output.jl](MITgcm_scan_output.html)

## Examples / Running Models
 
- `MITgcm_configurations.jl` : explore MITgcm configurations and their parameters.
- `MITgcm_worklow.jl` : build, setup, run, and plot for any standard configuration.
- `run_MITgcm.jl` : a more detailed look into compiling and running the model.

## Examples / Analyzing Results

- `MITgcm_scan_output.jl` : scan `output.txt`, read grid, viz with `Makie.jl` 
- `HS94_plotmap.jl` : read `hs94.cs-32x32x5` output, interpolate, and plot map
- `HS94_particles.jl` : compute particle trajectories from `hs94.cs-32x32x5` output
- `HS94_Makie.jl` : example using `Makie.jl` instead of `Plots.jl`


# Examples

To load one of the notebooks using `Pluto.jl`: 

1. open `julia` in terminal window
2. type the following commands at the `Julia` prompt
3. in web-browser, open one of the notebooks listed hereafter

```
cd("examples/")
using Pluto
Pluto.run()
```

## Examples / Running Models
 
- `MITgcm_configurations.jl` : explore MITgcm configurations and their parameters.
- `MITgcm_worklow.jl` : build, setup, run, and plot for any standard configuration.
- `run_MITgcm.jl` : a more detailed look into compiling and running the model.

## Examples / Analyzing Results

- `HS94_plotmap.jl` : read `hs94.cs-32x32x5` output, interpolate, and plot map
- `HS94_particles.jl` : compute particle trajectories from `hs94.cs-32x32x5` output
- `HS94_Makie.jl` : example using `Makie.jl` instead of `Plots.jl`

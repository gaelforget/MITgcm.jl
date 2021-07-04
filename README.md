# MITgcmTools.jl

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://gaelforget.github.io/MITgcmTools.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://gaelforget.github.io/MITgcmTools.jl/dev)
[![CI](https://github.com/gaelforget/MITgcmTools.jl/actions/workflows/ci.yml/badge.svg)](https://github.com/gaelforget/MITgcmTools.jl/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/gaelforget/MITgcmTools.jl/branch/master/graph/badge.svg?token=zUK0vO5K3J)](https://codecov.io/gh/gaelforget/MITgcmTools.jl)

[![DOI](https://zenodo.org/badge/236192181.svg)](https://zenodo.org/badge/latestdoi/236192181)

Set of tools for running [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest), analyzing its output, and/or modifying its inputs. A set of [Pluto.jl](https://github.com/fonsp/Pluto.jl) notebooks, which e.g. run `MITgcm` interactively, can be found in the `examples/` folder.

<details>
  <summary><b>Examples / How-To </b></summary>

To load one of the notebooks using `Pluto.jl`: 

1. open `julia` in terminal window
2. type the following commands at the `Julia` prompt
3. in browser, open one of the notebooks listed hereafter

```
cd("examples/")
using Pluto
Pluto.run()
```
</details>

<details>
  <summary><b>Examples / Running Models </b></summary>
  
- `MITgcm_configurations.jl` : explore MITgcm configurations and their parameters.
- `MITgcm_worklow.jl` : build, setup, run, and plot for any standard configuration.
- `MITgcm_run.jl` : a more detailed look into compiling and running the model.
</details>

<details>
  <summary><b>Examples / Analyzing Results </b></summary>
  
- `HS94_plotmap.jl` : read `hs94.cs-32x32x5` output, interpolate, and plot map
- `HS94_particles.jl` : compute particle trajectories from `hs94.cs-32x32x5` output
- `HS94_Makie.jl` : example using `Makie.jl` instead of `Plots.jl`
</details>

<img src="https://user-images.githubusercontent.com/20276764/111042787-12377e00-840d-11eb-8ddb-64cc1cfd57fd.png" width="90%"> 

<img src="https://user-images.githubusercontent.com/20276764/113531401-b1780d00-9596-11eb-8e96-990cf9533ada.png" width="70%"> 

<img src="https://user-images.githubusercontent.com/20276764/97648227-970b9780-1a2a-11eb-81c4-65ec2c87efc6.png" width="50%">  

<img src="https://user-images.githubusercontent.com/20276764/111195521-b7c82a00-8592-11eb-86a0-c85969de0850.png" width="30%">  




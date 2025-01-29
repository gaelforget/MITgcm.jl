# MITgcm.jl

[![Documentation](https://img.shields.io/badge/docs-dev-blue.svg)](https://gaelforget.github.io/MITgcm.jl/dev)
[![codecov](https://codecov.io/gh/gaelforget/MITgcm.jl/branch/master/graph/badge.svg?token=zUK0vO5K3J)](https://codecov.io/gh/gaelforget/MITgcm.jl)
[![CI](https://github.com/gaelforget/MITgcm.jl/actions/workflows/ci.yml/badge.svg)](https://github.com/gaelforget/MITgcm.jl/actions/workflows/ci.yml)

[![DOI](https://joss.theoj.org/papers/10.21105/joss.06710/status.svg)](https://doi.org/10.21105/joss.06710)
[![DOI](https://zenodo.org/badge/236192181.svg)](https://zenodo.org/badge/latestdoi/236192181)
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/gaelforget/MITgcm.jl/master)

Julia interface to [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest) that allows user not only to analyze model output, but also to download the source code, build the model executable, modify run-time parameters, run model simulations, or verify model results against a benchmark.

Forget, G., (2024). MITgcm.jl: a Julia Interface to the MITgcm. Journal of Open Source Software, 9(102), 6710, https://doi.org/10.21105/joss.06710

Notebooks and tutorials are found in [the docs](https://gaelforget.github.io/MITgcm.jl/dev/examples/).

<details>
  <summary><b>Examples / How-To </b></summary>

To open a notebook using [Pluto.jl](https://featured.plutojl.org): 

1. open `julia` in terminal window
2. type command below at the `Julia` prompt
3. _new web browser tab should show `Pluto` prompt_
4. copy/paste a notebook URL from [the docs](https://gaelforget.github.io/MITgcm.jl/dev/examples/)

```
cd("examples/"); using Pluto; Pluto.run()
```

</details>

<details>
  <summary><b>Examples / Running Models </b></summary>
  
- [MITgcm_configurations.jl](https://gaelforget.github.io/MITgcm.jl/dev/examples/MITgcm_configurations.html) : explore MITgcm configurations and their parameters.
- [MITgcm_worklow.jl](https://gaelforget.github.io/MITgcm.jl/dev/examples/MITgcm_worklow.html) : build, setup, run, and plot for any standard configuration.
- [MITgcm_run.jl](https://gaelforget.github.io/MITgcm.jl/dev/examples/MITgcm_run.html) : a more detailed look into compiling and running the model.
- [MITgcm\_scan\_output.jl](https://gaelforget.github.io/MITgcm.jl/dev/examples/MITgcm_scan_output.html) : scan `output.txt`, read grid, viz with `Makie.jl`
</details>

<details>
  <summary><b>Examples / Analyzing Results </b></summary>
  
- [HS94_animation.jl](https://gaelforget.github.io/MITgcm.jl/dev/examples/HS94_animation.html) : run `hs94.cs-32x32x5`, read output, interpolate, and animate map
- [HS94_particles.jl](https://gaelforget.github.io/MITgcm.jl/dev/examples/HS94_particles.html) : compute particle trajectories from `hs94.cs-32x32x5` output
- [HS94_Makie.jl](https://gaelforget.github.io/MITgcm.jl/dev/examples/HS94_Makie.html) : example using `Makie.jl` instead of `Plots.jl`
</details>

<img src="https://user-images.githubusercontent.com/20276764/111042787-12377e00-840d-11eb-8ddb-64cc1cfd57fd.png" width="90%"> 

<img src="https://user-images.githubusercontent.com/20276764/113531401-b1780d00-9596-11eb-8e96-990cf9533ada.png" width="70%"> 

<img src="https://user-images.githubusercontent.com/20276764/97648227-970b9780-1a2a-11eb-81c4-65ec2c87efc6.png" width="50%">  

<img src="https://user-images.githubusercontent.com/20276764/111195521-b7c82a00-8592-11eb-86a0-c85969de0850.png" width="30%">  




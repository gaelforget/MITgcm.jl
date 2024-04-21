# ClimateModels Interface

The [ClimateModels.jl](https://github.com/gaelforget/ClimateModels.jl/#readme)'s interface is implemented in this pacakge. This allows you to easily create model simulations using `MITgcm` conveniently from `Julia`.

```@example 1
using MITgcm
MC=MITgcm_config(configuration="advect_xy")
setup(MC)
build(MC,"--allow-skip")
launch(MC)
```

```@example 1
sc=scan_rundir(joinpath(MC,"run"))
keys(sc)
```

## Functionalities

```@docs
setup
build
compile
MITgcm_launch
clean
```

# ClimateModels Interface

The [ClimateModels.jl]()'s interface is implemented in this pacakge. This allows you to easily create model simulations using `MITgcm` conveniently from `Julia`.

```@example 1
using MITgcm
MC=MITgcm_config(configuration="advect_xy")
setup(MC)
build(MC,"--allow-skip")
MITgcm_launch(MC)
```

## Functionalities

```@docs
setup
build
compile
MITgcm_launch
clean
```

# ClimateModels Interface

This allows us to create model simulations using `MITgcm` conveniently from `Julia`.

```
MC=MITgcm_config(configuration="advect_xy")
setup(MC)
build(MC)
MITgcm_launch(MC)
```

```@docs
setup
build
compile
MITgcm_launch
clean
```

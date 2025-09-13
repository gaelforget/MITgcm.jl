```@meta
CollapsedDocStrings = true
```

# Additional Functionalities

### Related packages 

- [ClimateModels.jl](https://gaelforget.github.io/ClimateModels.jl/stable/) : standard interface for modeling workflows, used in `MITgcm.jl`. 
- [Climatology.jl](https://JuliaOcean.github.io/Climatology.jl/dev/) : accessing gridded data sets and using ECCO solutions. 
- [MeshArrays.jl](https://juliaclimate.github.io/MeshArrays.jl/dev/) : data structures for gridded model output.
- [Drifters.jl](https://juliaclimate.github.io/Drifters.jl/dev/) : particle tracking applications and pathway simulations. 

### More Examples

The [notebook from JuliaCon2021](https://juliaocean.github.io/MarineEcosystemsJuliaCon2021.jl/dev/MITgcm_tutorial_global_oce_biogeo.html) ([MITgcm\_tutorial\_global\_oce\_biogeo.jl](https://juliaocean.github.io/MarineEcosystemsJuliaCon2021.jl/dev/MITgcm_tutorial_global_oce_biogeo.jl)) reads and visualize results from the standard `MITgcm` configuration called [tutorial\_global\_oce_biogeo](https://mitgcm.readthedocs.io/en/latest/examples/global_oce_biogeo/global_oce_biogeo.html).

!!! note 
    This notebook builds and runs `tutorial_global_oce_biogeo` from within `Julia`. Alternatively, the [MITgcm documentation](https://mitgcm.readthedocs.io/en/latest/getting_started/getting_started.html) explains how to build and run tutorials at the command line in `linux`. 

### Format Conversions

```@docs
findtiles
cube2compact
compact2cube
convert2array
convert2gcmfaces
```

### Formulas, Parameters

```@docs
SeaWaterDensity
MixedLayerDepth
```

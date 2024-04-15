# MITgcm.jl

Julia interface to [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest) which allows user to not only read and analyze model output, but also download the source code, build the model executable, set run-time parameters, run simulations, verify model results, and perform other tasks related to MITgcm. These functionalities are documented via the [Examples](@ref) section.

## [Table Of Contents](@id main-contents)

```@contents
Pages = [
    "functionalities.md",
    "functionalities_interface.md",
    "functionalities_configurations.md",
    "functionalities_read.md",
    "examples.md",
    "contributing.md",
]
Depth = 2
```

## Main Features

- Read / Write / Scan the various MITgcm File types
- Standard Modeling Worflow (Climate Model Interface)
- Standard MITgcm configurations (`verification`)
- [Examples](@ref) (notebooks in `examples/`)

## Related packages 

- [ClimateModels.jl](https://gaelforget.github.io/ClimateModels.jl/stable/) provides a standard interface for such workflows. 
- [OceanStateEstimation.jl](https://JuliaOcean.github.io/OceanStateEstimation.jl/dev/) for accessing and using ECCO solutions. 
- [MeshArrays.jl](https://juliaclimate.github.io/MeshArrays.jl/dev/) defining in-memory containters for gridded model output.
- [IndividualDisplacements.jl](https://juliaclimate.github.io/IndividualDisplacements.jl/dev/) enabling particle tracking applications. 

## Visuals 

![Simulated particles from HS94 on cube sphere grid](https://user-images.githubusercontent.com/20276764/111042787-12377e00-840d-11eb-8ddb-64cc1cfd57fd.png)

![The impossible MITgcm rendering](https://user-images.githubusercontent.com/20276764/97648227-970b9780-1a2a-11eb-81c4-65ec2c87efc6.png)

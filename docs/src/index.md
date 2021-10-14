# MITgcmTools.jl

Tools for using [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest) or analyzing its output. This includes not only manipulating model inputs and outputs, but also compiling and running the model. Much of the functionalities are documented via the [Examples](@ref) section, as highlighted hereafter.

[ClimateModels.jl](https://gaelforget.github.io/ClimateModels.jl/stable/) provides a standard interface for such workflows. Related packages also include [MeshArrays.jl](https://juliaclimate.github.io/MeshArrays.jl/dev/) defining in-memory containters for gridded model output, and [IndividualDisplacements.jl](https://juliaclimate.github.io/IndividualDisplacements.jl/dev/) enabling particle tracking applications. 

## Main Features

- Scan / Read / Write MITgcm Files
- Standard MITgcm configurations
- Climate Model Interface
- [Examples](@ref) (notebooks in `examples/`)

## [Table Of Contents](@id main-contents)

```@contents
Pages = [
    "functionalities.md",
    "functionalities_read.md",
    "functionalities_configurations.md",
    "functionalities_interface.md",
    "functionalities_more.md",
    "examples.md",
]
Depth = 2
```

![Simulated particles from HS94 on cube sphere grid](https://user-images.githubusercontent.com/20276764/111042787-12377e00-840d-11eb-8ddb-64cc1cfd57fd.png)

![The impossible MITgcm rendering](https://user-images.githubusercontent.com/20276764/97648227-970b9780-1a2a-11eb-81c4-65ec2c87efc6.png)

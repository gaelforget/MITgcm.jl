# MITgcm.jl

Julia interface to [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest) which allows user to not only read and analyze model output, but also download the source code, build the model executable, set run-time parameters, run simulations, verify model results, and perform other tasks related to MITgcm. These functionalities are documented via the [Examples](@ref) section.

## [Table Of Contents](@id main-contents)

```@contents
Pages = [
    "functionalities.md",
    "functionalities_configurations.md",
    "functionalities_interface.md",
    "functionalities_read.md",
    "examples.md",
    "contributing.md",
]
Depth = 1
```

## Major Features

- Standard model run workflow (MITgcm-Julia interface)
- Standard MITgcm configurations (`verification`, `ECCO`)
- Read and write the various MITgcm file types
- Examples (docs, and notebooks in `examples/`)

```@raw html
<img src="https://user-images.githubusercontent.com/20276764/111042787-12377e00-840d-11eb-8ddb-64cc1cfd57fd.png" alt="Simulated particles from HS94 on cube sphere grid" width="200">
<img src="https://user-images.githubusercontent.com/20276764/97648227-970b9780-1a2a-11eb-81c4-65ec2c87efc6.png" alt="The impossible MITgcm rendering" width="200">
```

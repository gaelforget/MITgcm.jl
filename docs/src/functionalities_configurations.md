
```@meta
CollapsedDocStrings = true
```

# Model Configurations

In `MITgcm.jl`, a model configuration is represented as a [`MITgcm_config`](@ref). Model parameters are handled as ordered dictionaries and stored as `TOML` files. Standard model configurations are readily supported.

## MITgcm_config

The data structure that enables `MITgcm.jl` is called `MITgcm_config`.

```@docs
MITgcm_config
```

## Model Parameters

Model parameters can be stored as a `TOML` file, and represented as a nested `OrderedDic`.

```@example 2
using MITgcm # hide
p0=dirname(pathof(MITgcm)) # hide
fil=joinpath(p0,"..","examples","configurations","OCCA2.toml") # hide
read_toml(fil)
```

## Global ECCO Configuration

Global ocean model configurations used in [NASA's ECCO](https://ecco-group.org) ocean state estimation program.
- ECCO4 : [Forget et al., 2015](http://www.geosci-model-dev.net/8/3071/2015/) (`doi:10.5194/gmd-8-3071-2015`)
- OCCA2 : [Forget, 2024](https://doi.org/10.21203/rs.3.rs-3979671/v1) (_under review_).

!!! warning
	Running these solutions requires at least 16 cores (preferably 96) and downloading forcing fields (96G at least). To try with a smaller model solution, see [Verification Experiments](@ref).

```@docs
setup_ECCO4!
ECCO4_inputs.download_input_folder
ECCO4_inputs.get_files
ECCO4_inputs.get_list
ECCO4_testreport.compute
ECCO4_testreport.compare
```

## Verification Experiments

The [MITgcm/verification](https://mitgcm.readthedocs.io/en/latest/getting_started/getting_started.html) sub-folder of the `MITgcm` source code provides a suite of small model configurations, often used by model developers for testing. 

To list of these model configurations (as installed) is provided by [`verification_experiments`](@ref). 

```@example 1
using MITgcm # hide
ves=verification_experiments()
[ve.configuration for ve in ves]
```

They can be used via a [`MITgcm_config`](@ref) as follows.

```@example 1
MITgcm_config(configuration="MLAdjust")
```

## Functionalities

```@docs
verification_experiments
setup_verification!
testreport
```

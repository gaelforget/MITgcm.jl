# Model Configurations

## MITgcm_config

The data structure that enables `MITgcm.jl` is called `MITgcm_config`.

```@docs
MITgcm_config
```

## ECCO solutions

Global ocean model configurations used in [NASA's ECCO](https://ecco-group.org) ocean state estimation program.
- ECCO4 : [Forget et al., 2015](http://www.geosci-model-dev.net/8/3071/2015/) (`doi:10.5194/gmd-8-3071-2015`)
- OCCA2 : [Forget, 2024](https://doi.org/10.21203/rs.3.rs-3979671/v1) (_under review_).

```@docs
setup_ECCO4!
```

## MITgcm/Verification

The [MITgcm/verification](https://mitgcm.readthedocs.io/en/latest/getting_started/getting_started.html) folder is found in the `MITgcm` source code. It provides a suite of basic configurations used by model developers. 

To use them, you want to download the `MITgcm` source code. For example :

```@example 1
using MITgcm
MITgcm_download()
MITgcm_path[1]
```

To get the list of standard model configurations in `MITgcm/verification/` : 

```@example 1
verification_experiments()
```

To choose one : 

```@example 1
MITgcm_config(configuration="MLAdjust")
```

## Functionalities

```@docs
setup_verification!
verification_experiments
MITgcm_download
MITgcm_path
```

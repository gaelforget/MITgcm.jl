# Model Configurations

## MITgcm_config

```@docs
MITgcm_config
setup_verification!
setup_ECCO4!
```

## Verification Experiments

Get the default version of `MITgcm` source code, and locate the local copy:

```@example 1
using MITgcm
MITgcm_download()
MITgcm_path[1]
```

Get the lsit of standard model configurations in `verification/` : 

```@example 1
verification_experiments()
```

Choose one : 

```@example 1
MITgcm_config(configuration="MLAdjust")
```

## Functionalities

```@docs
MITgcm_download
MITgcm_path
verification_experiments
```

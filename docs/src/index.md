# MITgcmTools.jl

Set of tools for running [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest), analyzing its results, and preparing model inputs. Examples are provided in `test/runtests.jl` as well as in `MITgcm_worklow.jl`,  `run_MITgcm.jl`, `monitor_run.jl`, and other notebooks in `examples/`.

![Simulated particles from HS94 on cube sphere grid](https://user-images.githubusercontent.com/20276764/111042787-12377e00-840d-11eb-8ddb-64cc1cfd57fd.png)

## Explore And Run MITgcm

The recommended, simple, method to run the model is via the climate model interface (see [docs@ClimateModels.jl](https://gaelforget.github.io/ClimateModels.jl/dev/) for detail). The `MITgcm_launch` function can be used to run a `MITgcm` configuration after setting up the `MITgcm_config`. Using this interface facilitates operations like compiling and setting up a temporary folder to run the model. Key functions, incl. the climate model interface, are documented further down in the docs. 

The `verification_experiments` function provides a list of the most-standard MITgcm configurations that can all be run either in batch mode or interactively. The `MITgcm_path` variable points to where MITgcm is compiled. Interactive / reactive notebooks are found in the `examples/` folder (e.g. `run_MITgcm.jl`  seen just below). 


![Compiling and running MITgcm](https://user-images.githubusercontent.com/20276764/111195521-b7c82a00-8592-11eb-86a0-c85969de0850.png)

```@docs
testreport
verification_experiments
MITgcm_namelist
```

## ClimateModels / MITgcm interface

```@docs
MITgcm_config
build
compile 
setup
MITgcm_launch
clean
```

## Reading MITgcm outputs

```@docs
read_mdsio
read_meta
read_namelist
write_namelist
read_available_diagnostics
read_bin
read_flt
read_nctiles
```

## Format conversions

![The impossible MITgcm rendering](https://user-images.githubusercontent.com/20276764/97648227-970b9780-1a2a-11eb-81c4-65ec2c87efc6.png)

```@docs
findtiles
cube2compact
compact2cube
convert2array
convert2gcmfaces
```

## Formulae etc

```@docs
SeaWaterDensity
MixedLayerDepth
```
## Index

```@index
```

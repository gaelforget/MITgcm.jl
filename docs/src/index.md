# MITgcmTools.jl

Set of tools for running [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest), analyzing its results, and preparing model inputs. Examples are provided in `test/runtests.jl` as well as in `examples/run_MITgcm.jl`, `monitor_run.jl`, etc.


![Simulated particles from HS94 on cube sphere grid](https://user-images.githubusercontent.com/20276764/111042787-12377e00-840d-11eb-8ddb-64cc1cfd57fd.png)

## Index

```@index
```

## Explore And Run MITgcm

The `MITgcm_path` variable provides the path to the MITgcm folder being used. The `MITgcm_launch` function can be used to run a `MITgcm` configuration either directly or (recommended) using the climate model interface (see `ClimateModels.jl`). 

Interactive / reactive notebooks (see `Pluto.jl`) are found in the `examples/` folder (e.g. `run_MITgcm.jl`  depicted below). The `verification_experiments` function provides a list of the most-standard MITgcm configurations that can all be run in such fashion.

![Compiling and running MITgcm](https://user-images.githubusercontent.com/20276764/111195521-b7c82a00-8592-11eb-86a0-c85969de0850.png)

```@docs
MITgcm_config
MITgcm_namelist
clean
build
compile 
setup
MITgcm_launch
testreport
verification_experiments
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

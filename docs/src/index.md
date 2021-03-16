# MITgcmTools.jl

Set of tools for running [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest), analyzing its results, and preparing model inputs. Examples are provided in `test/runtests.jl` as well as in `examples/run_MITgcm.jl`, `monitor_run.jl`, etc.


![Simulated particles from HS94 on cube sphere grid](https://user-images.githubusercontent.com/20276764/111042787-12377e00-840d-11eb-8ddb-64cc1cfd57fd.png)

## Index

```@index
```

## MITgcm path and run script

The `MITgcm_path` variable provides the path to the MITgcm folder being used. The `MITgcm_compile` and `MITgcm_run` functions can be used to, respectively, compile and run a `MITgcm` configuration (via the `testreport` script). An interactive / reactive example can be found in the `examples/run_MITgcm.jl` Pluto notebook shown below.

![Compiling and running MITgcm](https://user-images.githubusercontent.com/20276764/111195521-b7c82a00-8592-11eb-86a0-c85969de0850.png)

```@docs
MITgcm_cleanup
MITgcm_compile 
MITgcm_run
testreport
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

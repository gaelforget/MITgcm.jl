# MITgcmTools.jl

Set of tools for running [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest), analyzing its results, and preparing model inputs.

![Simulated particles from HS94 on cube sphere grid](https://user-images.githubusercontent.com/20276764/111042787-12377e00-840d-11eb-8ddb-64cc1cfd57fd.png)

## Index

```@index
```

![The impossible MITgcm rendering](https://user-images.githubusercontent.com/20276764/97648227-970b9780-1a2a-11eb-81c4-65ec2c87efc6.png)

## MITgcm path and run script

The `MITgcm_path` variable provides the path to the MITgcm folder being used. The `testreport` function can be used to call the run script of the same name.

```@docs
testreport
```

## Reading MITgcm outputs

```@docs
read_mdsio
read_meta
read_available_diagnostics
read_bin
read_flt
read_nctiles
```

## Format conversions

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


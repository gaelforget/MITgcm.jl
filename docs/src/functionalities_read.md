# MITgcm File Formats

The two main output formats of MITgcm gridded fields are called `MDS` and `MNC`. In addition, the standard output is a text file that records events during the model run, including its successful completion.

## Standard Output

```@docs
scan_rundir
scan_stdout
read_available_diagnostics
```

## MDS Files

```@docs
read_mdsio
read_meta
```

## MNC Files

```@docs
read_mnc
```

## Grid Files

```@docs
GridLoad_mdsio
GridLoad_mnc
GridLoad_native
```

## Input Files

```@docs
MITgcm_namelist
read_namelist
write_namelist
read_toml
```

## Other Files

```@docs
read_flt
read_bin
read_nctiles
```

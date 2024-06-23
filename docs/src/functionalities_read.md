# MITgcm File Formats

The two main output formats of MITgcm gridded fields are called `MDS` and `MNC`. In addition, the `standard output file` is a text file that records events during the model run, including its successful completion.

## Standard Output

The `standard output file` can be scanned to collect information about the model run. 

```@docs
monitor
scan_rundir
scan_stdout
read_available_diagnostics
```

## Input Files

Run-time parameters to `MITgcm` are provided via text files. This package can read and write them in two formats (standard `TOML` format, or the native `MITgcm_namelist` format).

Other inputs (binary or NetCDF files) for gridded data, forcing fields, etc can be provided via an `input_folder` or downloaded as shown in the [`setup_ECCO4!`](@ref).

```@docs
read_toml
MITgcm_namelist
read_namelist
write_namelist
read_all_namelists
write_all_namelists
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

## Other Output Files

```@docs
read_flt
read_bin
read_nctiles
```


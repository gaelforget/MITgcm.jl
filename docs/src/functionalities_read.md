```@meta
CollapsedDocStrings = true
```

# MITgcm File Formats

The two main output formats of MITgcm gridded fields are called `MDS` and `MNC`. In addition, the `standard output file` is a text file that records events during the model run, including its successful completion.

## Standard Output

The `standard output file` can be scanned to collect information about the model run. 

```@docs
monitor
MITgcm_run_dir
scan_run_dir
scan_stdout
scan_build_dir
```

## Input Files

Run-time parameters to `MITgcm` are provided via text files, which can be accessed via the `MITgcm_namelist` data structure. `MITgcm.jl` can read and write these parameter files in two formats : standard `TOML` format, or the native `MITgcm` format.

Other inputs (binary or NetCDF files) for gridded data, forcing fields, etc can be provided via an `input_folder` as shown in [`setup_verification!`](@ref) and [`setup_ECCO4!`](@ref).

```@docs
MITgcm_namelist
read_toml
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

## Other Files

```@docs
read_available_diagnostics
read_flt
read_bin
read_nctiles
```


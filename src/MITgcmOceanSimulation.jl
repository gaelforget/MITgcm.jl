"""
    AbstractMITgcmGrid

Abstract supertype for MITgcm grid coordinate systems.
"""
abstract type AbstractMITgcmGrid end

"""
    SphericalPolarGrid <: AbstractMITgcmGrid

Spherical polar (latitude-longitude) grid. Coordinates `xc` and `yc`
represent longitude and latitude in degrees.
"""
struct SphericalPolarGrid <: AbstractMITgcmGrid end

"""
    CartesianGrid <: AbstractMITgcmGrid

Cartesian grid. Coordinates `xc` and `yc` represent distances in meters.
"""
struct CartesianGrid <: AbstractMITgcmGrid end

"""
    CubedSphereGrid <: AbstractMITgcmGrid

Cubed-sphere grid. Coordinates `xc` and `yc` represent longitude and
latitude mapped onto a cube-sphere mesh.
"""
struct CubedSphereGrid <: AbstractMITgcmGrid end

"""
    CylindricalGrid <: AbstractMITgcmGrid

Cylindrical grid. Coordinates `xc` and `yc` represent azimuthal angle
and radius.
"""
struct CylindricalGrid <: AbstractMITgcmGrid end

"""
    read_namelist_parameter(contents, name, T; default)

Parse a single parameter value from MITgcm namelist file contents.
Supports numeric types and `Bool` (Fortran-style `.TRUE.`/`.FALSE.`).
Returns `default` if the parameter is not found.
"""
function read_namelist_parameter(contents::String, name::String, ::Type{T}; default::T) where T<:Number
    m = match(Regex(name * raw"\s*=\s*([^,\n]+)", "i"), contents)
    m === nothing && return default
    return parse(T, strip(m.captures[1]))
end

function read_namelist_parameter(contents::String, name::String, ::Type{Bool}; default::Bool)
    m = match(Regex(name * raw"\s*=\s*([^,\n]+)", "i"), contents)
    m === nothing && return default
    val = strip(m.captures[1])
    return occursin(r"\.?[Tt]([Rr][Uu][Ee])?\.?", val)
end

"""
    infer_grid_type(run_dir)

Infer the MITgcm grid type from the `data` namelist in `run_dir`.
Checks the `usingCartesianGrid`, `usingCylindricalGrid`,
`usingSphericalPolarGrid`, and `usingCurvilinearGrid` flags.
Defaults to `SphericalPolarGrid()` (MITgcm's own default).
"""
function infer_grid_type(run_dir::String)
    data_file = joinpath(run_dir, "data")
    !isfile(data_file) && return SphericalPolarGrid()

    contents = read(data_file, String)

    if read_namelist_parameter(contents, "usingCartesianGrid", Bool; default=false)
        return CartesianGrid()
    elseif read_namelist_parameter(contents, "usingCylindricalGrid", Bool; default=false)
        return CylindricalGrid()
    elseif read_namelist_parameter(contents, "usingCurvilinearGrid", Bool; default=false)
        return CubedSphereGrid()
    else
        return SphericalPolarGrid()
    end
end

"""
    infer_physical_constants(run_dir)

Read `rhoConst` and `HeatCapacity_Cp` from the `data` namelist in `run_dir`.
Returns `(reference_density, heat_capacity)` using MITgcm defaults (1029.0 kg/m³ and 3994.0 J/(kg·K))
when parameters are not specified.
"""
function infer_physical_constants(run_dir::String)
    data_file = joinpath(run_dir, "data")
    !isfile(data_file) && return (1029.0, 3994.0)

    contents = read(data_file, String)
    reference_density = read_namelist_parameter(contents, "rhoConst",        Float64; default=1029.0)
    heat_capacity     = read_namelist_parameter(contents, "HeatCapacity_Cp", Float64; default=3994.0)
    return (reference_density, heat_capacity)
end

"""
    MITgcmOceanSimulation

High-level wrapper around a MITgcm shared library instance.
Holds cached state arrays (refreshed from the Fortran library on demand)
and grid metadata. Can be used standalone or as an ocean component in
NumericalEarth.jl's `EarthSystemModel`.

The grid type parameter `G <: AbstractMITgcmGrid` indicates the coordinate
system used by the MITgcm configuration:
- `SphericalPolarGrid` — latitude-longitude (default)
- `CartesianGrid` — Cartesian x-y coordinates
- `CubedSphereGrid` — cubed-sphere
- `CylindricalGrid` — cylindrical coordinates

The grid type is inferred automatically from the `data` namelist in the
run directory.
"""
mutable struct MITgcmOceanSimulation{G<:AbstractMITgcmGrid, L}
    library  :: L
    # Cached prognostic state (refreshed via refresh_state!)
    theta    :: Array{Float64, 3}
    salt     :: Array{Float64, 3}
    uvel     :: Array{Float64, 3}
    vvel     :: Array{Float64, 3}
    etan     :: Array{Float64, 2}
    # Grid arrays (fetched once at initialization)
    xc       :: Array{Float64, 2}
    yc       :: Array{Float64, 2}
    rc       :: Array{Float64, 1}
    drf      :: Array{Float64, 1}
    hfacc    :: Array{Float64, 3}
    # Surface forcing arrays (pre-allocated for coupling)
    fu       :: Array{Float64, 2}
    fv       :: Array{Float64, 2}
    qnet     :: Array{Float64, 2}
    empmr    :: Array{Float64, 2}
    qsw      :: Array{Float64, 2}
    saltflux :: Array{Float64, 2}
    # Physical constants
    reference_density :: Float64
    heat_capacity     :: Float64
end

function MITgcmOceanSimulation{G}(lib, theta, salt, uvel, vvel, etan,
                                   xc, yc, rc, drf, hfacc,
                                   fu, fv, qnet, empmr, qsw, saltflux,
                                   reference_density, heat_capacity) where G<:AbstractMITgcmGrid
    return new{G, typeof(lib)}(lib, theta, salt, uvel, vvel, etan,
                                xc, yc, rc, drf, hfacc,
                                fu, fv, qnet, empmr, qsw, saltflux,
                                reference_density, heat_capacity)
end

"""
    MITgcmOceanSimulation(mitgcm_dir; kwargs...)

Build MITgcm as a shared library, initialize the model, and return a
`MITgcmOceanSimulation` with pre-allocated arrays. The grid type and
physical constants are inferred automatically from the `data` namelist.

Arguments
=========
- `mitgcm_dir::String`: Path to the MITgcm source directory.

Keyword Arguments
=================
- `output_dir::String`: Build output directory (default: temp directory).
- `code_dir::String`: Code directory for `genmake2 -mods` (default: `global_oce_latlon/code`).
- `input_dir::String`: Input directory with runtime config files (default: `global_oce_latlon/input`).
- `verbose::Bool`: Show MITgcm Fortran output (default: `true`).
"""
function MITgcmOceanSimulation(mitgcm_dir::String;
                               output_dir::String = mktempdir(),
                               code_dir::String   = default_code_dir(mitgcm_dir),
                               input_dir::String  = default_input_dir(mitgcm_dir),
                               verbose::Bool = true)

    # Build the shared library
    result = build_mitgcm_library(mitgcm_dir; output_dir, code_dir, input_dir)

    # Create and initialize the library interface
    lib = MITgcmLibrary(result.library_path, result.run_dir; verbose)
    init!(lib)

    return allocate_ocean_simulation(lib)
end

"""
    MITgcmOceanSimulation(library_path, run_dir; kwargs...)

Create a `MITgcmOceanSimulation` from a pre-built shared library.
The grid type and physical constants are inferred automatically from
the `data` namelist in `run_dir`.
"""
function MITgcmOceanSimulation(library_path::String, run_dir::String;
                                verbose::Bool = true)

    lib = MITgcmLibrary(library_path, run_dir; verbose)
    init!(lib)

    return allocate_ocean_simulation(lib)
end

function allocate_ocean_simulation(lib)
    G = typeof(infer_grid_type(lib.run_dir))
    reference_density, heat_capacity = infer_physical_constants(lib.run_dir)

    Nx, Ny, Nr = lib.dims.Nx, lib.dims.Ny, lib.dims.Nr

    # Allocate state arrays
    theta = zeros(Float64, Nx, Ny, Nr)
    salt  = zeros(Float64, Nx, Ny, Nr)
    uvel  = zeros(Float64, Nx, Ny, Nr)
    vvel  = zeros(Float64, Nx, Ny, Nr)
    etan  = zeros(Float64, Nx, Ny)

    # Allocate and fetch grid arrays
    xc    = zeros(Float64, Nx, Ny)
    yc    = zeros(Float64, Nx, Ny)
    rc    = zeros(Float64, Nr)
    drf   = zeros(Float64, Nr)
    hfacc = zeros(Float64, Nx, Ny, Nr)

    get_xc!(lib, xc)
    get_yc!(lib, yc)
    get_rc!(lib, rc)
    get_drf!(lib, drf)
    get_hfacc!(lib, hfacc)

    # Allocate surface forcing arrays
    fu       = zeros(Float64, Nx, Ny)
    fv       = zeros(Float64, Nx, Ny)
    qnet     = zeros(Float64, Nx, Ny)
    empmr    = zeros(Float64, Nx, Ny)
    qsw      = zeros(Float64, Nx, Ny)
    saltflux = zeros(Float64, Nx, Ny)

    # Fetch initial state
    ocean = MITgcmOceanSimulation{G}(lib, theta, salt, uvel, vvel, etan,
                                      xc, yc, rc, drf, hfacc,
                                      fu, fv, qnet, empmr, qsw, saltflux,
                                      reference_density, heat_capacity)
    refresh_state!(ocean)

    return ocean
end

"""
    refresh_state!(ocean::MITgcmOceanSimulation)

Fetch the current prognostic state (theta, salt, uvel, vvel, etan) from
the MITgcm shared library into the cached Julia arrays.
"""
function refresh_state!(ocean::MITgcmOceanSimulation)
    lib = ocean.library
    get_theta!(lib, ocean.theta)
    get_salt!(lib,  ocean.salt)
    get_uvel!(lib,  ocean.uvel)
    get_vvel!(lib,  ocean.vvel)
    get_etan!(lib,  ocean.etan)
    return nothing
end

Base.eltype(::MITgcmOceanSimulation) = Float64

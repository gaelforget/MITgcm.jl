"""
    MITgcmOceanSimulation

High-level wrapper around a MITgcm shared library instance.
Holds cached state arrays (refreshed from the Fortran library on demand)
and grid metadata. Can be used standalone or as an ocean component in
NumericalEarth.jl's `EarthSystemModel`.
"""
mutable struct MITgcmOceanSimulation{L}
    library :: L
    # Cached prognostic state (refreshed via refresh_state!)
    theta   :: Array{Float64, 3}
    salt    :: Array{Float64, 3}
    uvel    :: Array{Float64, 3}
    vvel    :: Array{Float64, 3}
    etan    :: Array{Float64, 2}
    # Grid arrays (fetched once at initialization)
    xc      :: Array{Float64, 2}
    yc      :: Array{Float64, 2}
    rc      :: Array{Float64, 1}
    drf     :: Array{Float64, 1}
    hfacc   :: Array{Float64, 3}
    # Surface forcing arrays (pre-allocated for coupling)
    fu      :: Array{Float64, 2}
    fv      :: Array{Float64, 2}
    qnet    :: Array{Float64, 2}
    empmr   :: Array{Float64, 2}
    qsw     :: Array{Float64, 2}
    saltflux :: Array{Float64, 2}
    # Physical constants
    ρ₀      :: Float64
    cₚ      :: Float64
end

"""
    MITgcmOceanSimulation(mitgcm_dir; kwargs...)

Build MITgcm as a shared library, initialize the model, and return a
`MITgcmOceanSimulation` with pre-allocated arrays.

Arguments
=========
- `mitgcm_dir::String`: Path to the MITgcm source directory.

Keyword Arguments
=================
- `output_dir::String`: Build output directory (default: temp directory).
- `code_dir::String`: Code directory for `genmake2 -mods` (default: `global_oce_latlon/code`).
- `input_dir::String`: Input directory with runtime config files (default: `global_oce_latlon/input`).
- `reference_density::Float64`: Ocean reference density in kg/m³ (default: `1029.0`).
- `heat_capacity::Float64`: Ocean heat capacity in J/(kg·K) (default: `3994.0`).
- `verbose::Bool`: Show MITgcm Fortran output (default: `true`).
"""
function MITgcmOceanSimulation(mitgcm_dir::String;
                               output_dir::String = mktempdir(),
                               code_dir::String   = default_code_dir(mitgcm_dir),
                               input_dir::String  = default_input_dir(mitgcm_dir),
                               reference_density::Float64 = 1029.0,
                               heat_capacity::Float64 = 3994.0,
                               verbose::Bool = true)

    # Build the shared library
    result = build_mitgcm_library(mitgcm_dir; output_dir, code_dir, input_dir)

    # Create and initialize the library interface
    lib = MITgcmLibrary(result.library_path, result.run_dir; verbose)
    init!(lib)

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
    ocean = MITgcmOceanSimulation(lib, theta, salt, uvel, vvel, etan,
                                   xc, yc, rc, drf, hfacc,
                                   fu, fv, qnet, empmr, qsw, saltflux,
                                   reference_density, heat_capacity)
    refresh_state!(ocean)

    return ocean
end

"""
    MITgcmOceanSimulation(library_path, run_dir; kwargs...)

Create a `MITgcmOceanSimulation` from a pre-built shared library.
"""
function MITgcmOceanSimulation(library_path::String, run_dir::String;
                                reference_density::Float64 = 1029.0,
                                heat_capacity::Float64 = 3994.0,
                                verbose::Bool = true)

    lib = MITgcmLibrary(library_path, run_dir; verbose)
    init!(lib)

    Nx, Ny, Nr = lib.dims.Nx, lib.dims.Ny, lib.dims.Nr

    theta = zeros(Float64, Nx, Ny, Nr)
    salt  = zeros(Float64, Nx, Ny, Nr)
    uvel  = zeros(Float64, Nx, Ny, Nr)
    vvel  = zeros(Float64, Nx, Ny, Nr)
    etan  = zeros(Float64, Nx, Ny)

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

    fu       = zeros(Float64, Nx, Ny)
    fv       = zeros(Float64, Nx, Ny)
    qnet     = zeros(Float64, Nx, Ny)
    empmr    = zeros(Float64, Nx, Ny)
    qsw      = zeros(Float64, Nx, Ny)
    saltflux = zeros(Float64, Nx, Ny)

    ocean = MITgcmOceanSimulation(lib, theta, salt, uvel, vvel, etan,
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
    get_salt!(lib, ocean.salt)
    get_uvel!(lib, ocean.uvel)
    get_vvel!(lib, ocean.vvel)
    get_etan!(lib, ocean.etan)
    return nothing
end

Base.eltype(::MITgcmOceanSimulation) = Float64

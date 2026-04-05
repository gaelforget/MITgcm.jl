#=
 run_global_oce_latlon.jl

 Drive the MITgcm global_oce_latlon experiment from Julia using MITgcm.jl.

 This script:
   1. Loads the MITgcm shared library via MITgcmOceanSimulation
   2. Steps forward in time with prescribed atmospheric forcing
   3. Prints diagnostics at each step

 Usage:
   Option A: Build from scratch (requires MITgcm source):
     julia -e 'using MITgcm; ocean = MITgcmOceanSimulation("/path/to/MITgcm")'

   Option B: Use pre-built library:
     cd examples/global_oce_latlon
     julia run_global_oce_latlon.jl
=#

using Printf
using MITgcm

# ============================================================
# Setup: use pre-built library or build from scratch
# ============================================================

const SCRIPT_DIR = @__DIR__

# Check for pre-built library
function find_prebuilt_library()
    for ext in (".dylib", ".so")
        path = joinpath(SCRIPT_DIR, "libmitgcm" * ext)
        isfile(path) && return path
    end
    return nothing
end

prebuilt = find_prebuilt_library()
run_dir  = joinpath(SCRIPT_DIR, "run")

if prebuilt !== nothing && isdir(run_dir)
    println("Using pre-built library: $prebuilt")
    ocean = MITgcmOceanSimulation(prebuilt, run_dir)
else
    mitgcm_dir = get(ENV, "MITGCM_DIR", joinpath(SCRIPT_DIR, "..", "..", "..", "MITgcm"))
    if !isdir(mitgcm_dir)
        error("MITgcm directory not found: $mitgcm_dir\n" *
              "Set MITGCM_DIR environment variable or run build_mitgcm_lib.sh first.")
    end
    println("Building MITgcm from: $mitgcm_dir")
    ocean = MITgcmOceanSimulation(mitgcm_dir; output_dir=SCRIPT_DIR)
end

# ============================================================
# Grid info
# ============================================================

lib = ocean.library
dims = lib.dims
Nx, Ny, Nr = dims.Nx, dims.Ny, dims.Nr

println()
println("=" ^ 60)
println("  MITgcm global_oce_latlon - Julia Interface Demo")
println("=" ^ 60)
println()
println("Grid dimensions: Nx=$Nx  Ny=$Ny  Nr=$Nr")
@printf("Longitude range: %.1f to %.1f deg\n", minimum(ocean.xc), maximum(ocean.xc))
@printf("Latitude range:  %.1f to %.1f deg\n", minimum(ocean.yc), maximum(ocean.yc))
println()

# Ocean mask from hFacC surface layer
ocean_mask_2d = ocean.hfacc[:, :, 1]

# ============================================================
# Prescribed atmospheric forcing (simple bulk formulas)
# ============================================================

const ρ_air   = 1.225      # air density (kg/m³)
const c_p_air = 1004.0     # specific heat of air (J/kg/K)
const C_d     = 1.2e-3     # drag coefficient
const C_h     = 1.0e-3     # heat transfer coefficient

u_wind = zeros(Float64, Nx, Ny)
T_air  = zeros(Float64, Nx, Ny)

for j in 1:Ny, i in 1:Nx
    lat_rad = ocean.yc[i, j] * π / 180.0
    u_wind[i, j] = -10.0 * cos(3.0 * lat_rad)
    T_air[i, j]  = 25.0 - 40.0 * sin(lat_rad)^2
end

println("Prescribed atmosphere:")
@printf("  u_wind range: %.2f to %.2f m/s\n", minimum(u_wind), maximum(u_wind))
@printf("  T_air  range: %.2f to %.2f °C\n",  minimum(T_air),  maximum(T_air))
println()

function compute_bulk_fluxes!(fu, fv, qnet, sst, u_wind, T_air, mask)
    Nx, Ny = size(fu)
    for j in 1:Ny, i in 1:Nx
        if mask[i, j] > 0
            wind_speed = abs(u_wind[i, j])
            fu[i, j]   = ρ_air * C_d * wind_speed * u_wind[i, j]
            fv[i, j]   = 0.0
            qnet[i, j] = ρ_air * c_p_air * C_h * wind_speed * (sst[i, j] - T_air[i, j])
        else
            fu[i, j]   = 0.0
            fv[i, j]   = 0.0
            qnet[i, j] = 0.0
        end
    end
end

# ============================================================
# Helper: print field statistics
# ============================================================

function field_stats(name::String, arr; mask=nothing)
    valid = mask !== nothing ? arr[mask .> 0] : arr[:]
    isempty(valid) && return @printf("  %-8s: no valid points\n", name)
    @printf("  %-8s: min=%12.5e  max=%12.5e  mean=%12.5e\n",
            name, minimum(valid), maximum(valid), sum(valid) / length(valid))
end

# ============================================================
# Time stepping loop
# ============================================================

println("Initial state:")
field_stats("theta", ocean.theta[:,:,1], mask=ocean_mask_2d)
field_stats("salt",  ocean.salt[:,:,1],  mask=ocean_mask_2d)
println()

nsteps = 20
println("Running $nsteps time steps...")
println("-" ^ 60)

for s in 1:nsteps
    # Compute bulk fluxes from current SST
    sst = @view ocean.theta[:, :, 1]
    compute_bulk_fluxes!(ocean.fu, ocean.fv, ocean.qnet, sst, u_wind, T_air, ocean_mask_2d)

    # Set forcing in MITgcm
    set_fu!(lib, ocean.fu)
    set_fv!(lib, ocean.fv)
    set_qnet!(lib, ocean.qnet)
    set_empmr!(lib, ocean.empmr)
    set_saltflux!(lib, ocean.saltflux)

    # Step forward
    step!(lib)
    refresh_state!(ocean)

    niter = get_niter(lib)
    mtime = get_time(lib)

    @printf("\nStep %3d | iter=%5d | time=%10.1f s (%.2f days)\n",
            s, niter, mtime, mtime / 86400.0)
    field_stats("theta", ocean.theta[:,:,1], mask=ocean_mask_2d)
    field_stats("salt",  ocean.salt[:,:,1],  mask=ocean_mask_2d)
    field_stats("uVel",  ocean.uvel[:,:,1],  mask=ocean_mask_2d)
    field_stats("etaN",  ocean.etan,         mask=ocean_mask_2d)
end

println()
println("-" ^ 60)
println("Timestepping complete.")
println()

# Finalize
println("Finalizing MITgcm...")
MITgcm.finalize!(lib)
println("Done!")

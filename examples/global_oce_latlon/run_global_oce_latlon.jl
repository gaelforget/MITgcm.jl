#=
 run_global_oce_latlon.jl

 Drive the MITgcm global_oce_latlon experiment from Julia.

 This script:
   1. Loads the MITgcm shared library (libmitgcm)
   2. Initializes the model (grid, parameters, initial conditions)
   3. Steps forward in time, printing diagnostics at each step
   4. Visualizes the prognostic state (theta, salt, uVel, vVel, etaN)

 Usage:
   cd examples/global_oce_latlon
   julia run_global_oce_latlon.jl
=#

using Printf

# ============================================================
# Library path and run directory
# ============================================================
# NOTE: The shared library must be built with -D_BYTESWAPIO so that
# MITgcm performs its own byte-swapping for big-endian binary files.
# gfortran's -fconvert=big-endian flag does NOT work in shared
# libraries loaded from non-Fortran hosts like Julia.

const SCRIPT_DIR = @__DIR__
const RUN_DIR = joinpath(SCRIPT_DIR, "run")

# MITgcm reads input files (data, eedata, *.bin) from the working
# directory, so we cd into the run/ directory automatically.
if !isdir(RUN_DIR)
    error("Run directory not found: $RUN_DIR\n" *
          "Run build_mitgcm_lib.sh first to set up the run directory.")
end
cd(RUN_DIR)

# Find the shared library
function find_library()
    candidates = [
        joinpath(SCRIPT_DIR, "libmitgcm.dylib"),
        joinpath(SCRIPT_DIR, "libmitgcm.so"),
        joinpath(RUN_DIR, "libmitgcm.dylib"),
        joinpath(RUN_DIR, "libmitgcm.so"),
    ]
    for c in candidates
        isfile(c) && return c
    end
    error("libmitgcm not found. Run build_mitgcm_lib.sh first.\n" *
          "Searched: $(join(candidates, "\n  "))")
end

const LIBMITGCM = find_library()
println("Using library: $LIBMITGCM")

# ============================================================
# Julia wrappers around Fortran library calls
# ============================================================

"""
    mitgcm_get_dims()

Query grid dimensions from the MITgcm library.
Returns a NamedTuple with sNx, sNy, Nr, OLx, OLy, nSx, nSy, nPx, nPy, Nx, Ny.
"""
function mitgcm_get_dims()
    sNx = Ref{Int32}(0)
    sNy = Ref{Int32}(0)
    Nr  = Ref{Int32}(0)
    OLx = Ref{Int32}(0)
    OLy = Ref{Int32}(0)
    nSx = Ref{Int32}(0)
    nSy = Ref{Int32}(0)
    nPx = Ref{Int32}(0)
    nPy = Ref{Int32}(0)
    Nx  = Ref{Int32}(0)
    Ny  = Ref{Int32}(0)

    ccall((:mitgcm_lib_get_dims_, LIBMITGCM), Cvoid,
          (Ref{Int32}, Ref{Int32}, Ref{Int32},
           Ref{Int32}, Ref{Int32},
           Ref{Int32}, Ref{Int32},
           Ref{Int32}, Ref{Int32},
           Ref{Int32}, Ref{Int32}),
          sNx, sNy, Nr, OLx, OLy, nSx, nSy, nPx, nPy, Nx, Ny)

    return (sNx=Int(sNx[]), sNy=Int(sNy[]), Nr=Int(Nr[]),
            OLx=Int(OLx[]), OLy=Int(OLy[]),
            nSx=Int(nSx[]), nSy=Int(nSy[]),
            nPx=Int(nPx[]), nPy=Int(nPy[]),
            Nx=Int(Nx[]),   Ny=Int(Ny[]))
end

"""
    mitgcm_init()

Initialize MITgcm: boot execution environment, set up grid/parameters,
load initial conditions.
"""
function mitgcm_init()
    ccall((:mitgcm_lib_init_, LIBMITGCM), Cvoid, ())
end

"""
    mitgcm_step()

Advance the model by one forward time step.
"""
function mitgcm_step()
    ccall((:mitgcm_lib_step_, LIBMITGCM), Cvoid, ())
end

"""
    mitgcm_finalize()

Shut down MITgcm and print timing statistics.
"""
function mitgcm_finalize()
    ccall((:mitgcm_lib_finalize_, LIBMITGCM), Cvoid, ())
end

"""
    mitgcm_get_niter()

Return the current iteration number.
"""
function mitgcm_get_niter()
    niter = Ref{Int32}(0)
    ccall((:mitgcm_lib_get_niter_, LIBMITGCM), Cvoid, (Ref{Int32},), niter)
    return Int(niter[])
end

"""
    mitgcm_get_time()

Return the current model time in seconds.
"""
function mitgcm_get_time()
    t = Ref{Float64}(0.0)
    ccall((:mitgcm_lib_get_time_, LIBMITGCM), Cvoid, (Ref{Float64},), t)
    return t[]
end

# -- Field getters: copy Fortran data into Julia arrays --

function mitgcm_get_theta!(arr::Array{Float64,3})
    ccall((:mitgcm_lib_get_theta_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_salt!(arr::Array{Float64,3})
    ccall((:mitgcm_lib_get_salt_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_uvel!(arr::Array{Float64,3})
    ccall((:mitgcm_lib_get_uvel_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_vvel!(arr::Array{Float64,3})
    ccall((:mitgcm_lib_get_vvel_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_wvel!(arr::Array{Float64,3})
    ccall((:mitgcm_lib_get_wvel_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_etan!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_get_etan_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_xc!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_get_xc_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_yc!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_get_yc_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_rc!(arr::Array{Float64,1})
    ccall((:mitgcm_lib_get_rc_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_drf!(arr::Array{Float64,1})
    ccall((:mitgcm_lib_get_drf_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_hfacc!(arr::Array{Float64,3})
    ccall((:mitgcm_lib_get_hfacc_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_rlow!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_get_rlow_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

# -- Setters --

function mitgcm_set_theta!(arr::Array{Float64,3})
    ccall((:mitgcm_lib_set_theta_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_set_salt!(arr::Array{Float64,3})
    ccall((:mitgcm_lib_set_salt_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

# -- Surface forcing getters --

function mitgcm_get_fu!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_get_fu_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_fv!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_get_fv_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_qnet!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_get_qnet_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_empmr!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_get_empmr_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_qsw!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_get_qsw_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_get_saltflux!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_get_saltflux_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

# -- Surface forcing setters --

function mitgcm_set_fu!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_set_fu_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_set_fv!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_set_fv_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_set_qnet!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_set_qnet_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_set_empmr!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_set_empmr_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_set_qsw!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_set_qsw_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

function mitgcm_set_saltflux!(arr::Array{Float64,2})
    ccall((:mitgcm_lib_set_saltflux_, LIBMITGCM), Cvoid, (Ref{Float64},), arr)
end

# ============================================================
# Helper: print field statistics (min, max, mean)
# ============================================================

function field_stats(name::String, arr; mask=nothing)
    if mask !== nothing
        valid = arr[mask .> 0]
        if isempty(valid)
            @printf("  %-8s: no valid points\n", name)
            return
        end
    else
        valid = arr[:]
    end
    mn = minimum(valid)
    mx = maximum(valid)
    avg = sum(valid) / length(valid)
    @printf("  %-8s: min=%12.5e  max=%12.5e  mean=%12.5e\n", name, mn, mx, avg)
end

# ============================================================
# Main: Initialize, step, and inspect state
# ============================================================

println("=" ^ 60)
println("  MITgcm global_oce_latlon - Julia Interface Demo")
println("=" ^ 60)
println()

# -- Initialize --
println("Initializing MITgcm...")
mitgcm_init()
println("  Initialization complete.")
println()

# -- Query dimensions --
dims = mitgcm_get_dims()
println("Grid dimensions:")
println("  sNx=$(dims.sNx)  sNy=$(dims.sNy)  Nr=$(dims.Nr)")
println("  OLx=$(dims.OLx)  OLy=$(dims.OLy)")
println("  nSx=$(dims.nSx)  nSy=$(dims.nSy)  nPx=$(dims.nPx)  nPy=$(dims.nPy)")
println("  Global: Nx=$(dims.Nx)  Ny=$(dims.Ny)")
println()

# -- Allocate arrays for field extraction --
Nx, Ny, Nr = dims.Nx, dims.Ny, dims.Nr

theta = zeros(Float64, Nx, Ny, Nr)
salt  = zeros(Float64, Nx, Ny, Nr)
uvel  = zeros(Float64, Nx, Ny, Nr)
vvel  = zeros(Float64, Nx, Ny, Nr)
wvel  = zeros(Float64, Nx, Ny, Nr)
etan  = zeros(Float64, Nx, Ny)

xc    = zeros(Float64, Nx, Ny)
yc    = zeros(Float64, Nx, Ny)
rc    = zeros(Float64, Nr)
drf   = zeros(Float64, Nr)
hfacc = zeros(Float64, Nx, Ny, Nr)
rlow  = zeros(Float64, Nx, Ny)

# -- Get grid info --
mitgcm_get_xc!(xc)
mitgcm_get_yc!(yc)
mitgcm_get_rc!(rc)
mitgcm_get_drf!(drf)
mitgcm_get_hfacc!(hfacc)
mitgcm_get_rlow!(rlow)

println("Grid info:")
@printf("  Longitude range: %.1f to %.1f deg\n", minimum(xc), maximum(xc))
@printf("  Latitude range:  %.1f to %.1f deg\n", minimum(yc), maximum(yc))
println("  Depth levels (rC): ", join([@sprintf("%.0f", z) for z in rc], ", "), " m")
println("  Level thicknesses: ", join([@sprintf("%.0f", dz) for dz in drf], ", "), " m")
println()

# -- Mask for ocean points (hFacC > 0 at surface) --
ocean_mask_2d = hfacc[:, :, 1]

# ============================================================
# Surface forcing: simple bulk formulas computed in Julia
# ============================================================
#
# Wind stress:  τ = ρ_air * C_d * |U_wind| * U_wind
# Heat flux:    Q = ρ_air * c_p_air * C_h * |U_wind| * (SST - T_air)
#               (positive Qnet = ocean cooling, MITgcm convention)
# Salt flux:    0 (no freshwater or salt flux)

# Bulk parameters
const ρ_air   = 1.225      # air density (kg/m³)
const c_p_air = 1004.0     # specific heat of air (J/kg/K)
const C_d     = 1.2e-3     # drag coefficient (dimensionless)
const C_h     = 1.0e-3     # heat transfer coefficient (dimensionless)

# Prescribed atmospheric state (simple latitude-dependent profiles)
# Zonal wind: u_wind(lat) = 10 m/s * cos(3 * lat_rad) gives
#   easterlies in tropics, westerlies at mid-latitudes
# Meridional wind: v_wind = 0
# Air temperature: T_air(lat) = 25 - 40 * sin²(lat) gives
#   ~25°C at equator, ~-15°C at poles

u_wind = zeros(Float64, Nx, Ny)
v_wind = zeros(Float64, Nx, Ny)
T_air  = zeros(Float64, Nx, Ny)

for j in 1:Ny, i in 1:Nx
    lat_rad = yc[i, j] * π / 180.0
    u_wind[i, j] = -10.0 * cos(3.0 * lat_rad)
    v_wind[i, j] = 0.0
    T_air[i, j]  = 25.0 - 40.0 * sin(lat_rad)^2
end

println("Prescribed atmosphere:")
@printf("  u_wind range: %.2f to %.2f m/s\n", minimum(u_wind), maximum(u_wind))
@printf("  T_air  range: %.2f to %.2f °C\n",  minimum(T_air),  maximum(T_air))
println()

# Allocate forcing arrays
fu_arr       = zeros(Float64, Nx, Ny)
fv_arr       = zeros(Float64, Nx, Ny)
qnet_arr     = zeros(Float64, Nx, Ny)
empmr_arr    = zeros(Float64, Nx, Ny)  # zero freshwater flux
saltflux_arr = zeros(Float64, Nx, Ny)  # zero salt flux

"""
    compute_bulk_fluxes!(fu, fv, qnet, sst, u_wind, v_wind, T_air, mask)

Compute surface fluxes from bulk formulas:
  - fu, fv: wind stress (N/m²)
  - qnet: net heat flux (W/m², positive = ocean cooling)
Uses the ocean mask to only set fluxes at ocean points.
"""
function compute_bulk_fluxes!(fu, fv, qnet, sst,
                              u_wind, v_wind, T_air, mask)
    Nx, Ny = size(fu)
    for j in 1:Ny, i in 1:Nx
        if mask[i, j] > 0
            wind_speed = sqrt(u_wind[i, j]^2 + v_wind[i, j]^2)
            # Wind stress: τ = ρ_air * C_d * |U| * U
            fu[i, j] = ρ_air * C_d * wind_speed * u_wind[i, j]
            fv[i, j] = ρ_air * C_d * wind_speed * v_wind[i, j]
            # Heat flux: Q = ρ_air * c_p * C_h * |U| * (SST - T_air)
            # positive = ocean loses heat (MITgcm convention)
            qnet[i, j] = ρ_air * c_p_air * C_h * wind_speed *
                          (sst[i, j] - T_air[i, j])
        else
            fu[i, j]   = 0.0
            fv[i, j]   = 0.0
            qnet[i, j] = 0.0
        end
    end
end

# -- Initial state --
println("Initial state (before timestepping):")
mitgcm_get_theta!(theta)
mitgcm_get_salt!(salt)
mitgcm_get_uvel!(uvel)
mitgcm_get_vvel!(vvel)
mitgcm_get_etan!(etan)

field_stats("theta", theta[:,:,1], mask=ocean_mask_2d)
field_stats("salt",  salt[:,:,1],  mask=ocean_mask_2d)
field_stats("uVel",  uvel[:,:,1],  mask=ocean_mask_2d)
field_stats("vVel",  vvel[:,:,1],  mask=ocean_mask_2d)
field_stats("etaN",  etan,         mask=ocean_mask_2d)
println()

# -- Time stepping loop --
# The global_oce_latlon data file has nTimeSteps=20
nsteps = 20

println("Running $nsteps time steps...")
println("-" ^ 60)

for step in 1:nsteps
    # -- Compute bulk fluxes from current SST --
    mitgcm_get_theta!(theta)
    sst = @view theta[:, :, 1]

    compute_bulk_fluxes!(fu_arr, fv_arr, qnet_arr, sst,
                         u_wind, v_wind, T_air, ocean_mask_2d)

    # -- Set surface forcing in MITgcm --
    mitgcm_set_fu!(fu_arr)
    mitgcm_set_fv!(fv_arr)
    mitgcm_set_qnet!(qnet_arr)
    mitgcm_set_empmr!(empmr_arr)        # zero
    mitgcm_set_saltflux!(saltflux_arr)   # zero

    # -- Step forward --
    mitgcm_step()

    niter = mitgcm_get_niter()
    mtime = mitgcm_get_time()

    # Extract fields after step
    mitgcm_get_theta!(theta)
    mitgcm_get_salt!(salt)
    mitgcm_get_uvel!(uvel)
    mitgcm_get_vvel!(vvel)
    mitgcm_get_wvel!(wvel)
    mitgcm_get_etan!(etan)

    @printf("\nStep %3d | iter=%5d | time=%10.1f s (%.2f days)\n",
            step, niter, mtime, mtime / 86400.0)

    # Surface fields (k=1)
    field_stats("theta", theta[:,:,1], mask=ocean_mask_2d)
    field_stats("salt",  salt[:,:,1],  mask=ocean_mask_2d)
    field_stats("uVel",  uvel[:,:,1],  mask=ocean_mask_2d)
    field_stats("vVel",  vvel[:,:,1],  mask=ocean_mask_2d)
    field_stats("wVel",  wvel[:,:,1],  mask=ocean_mask_2d)
    field_stats("etaN",  etan,         mask=ocean_mask_2d)

    # Forcing diagnostics (every 5 steps)
    if step % 5 == 0
        field_stats("fu",   fu_arr,   mask=ocean_mask_2d)
        field_stats("Qnet", qnet_arr, mask=ocean_mask_2d)
    end
end

println()
println("-" ^ 60)
println("Timestepping complete.")
println()

# -- Final state summary --
println("Final state summary (all levels):")
for k in 1:Nr
    mask_k = hfacc[:, :, k]
    valid_theta = theta[:,:,k][mask_k .> 0]
    valid_salt  = salt[:,:,k][mask_k .> 0]
    if !isempty(valid_theta)
        @printf("  Level %2d (z=%6.0fm): theta=[%7.2f, %7.2f]  salt=[%7.2f, %7.2f]  (%d ocean pts)\n",
                k, rc[k],
                minimum(valid_theta), maximum(valid_theta),
                minimum(valid_salt), maximum(valid_salt),
                length(valid_theta))
    end
end
println()


# Julia interface for MITgcm-style Fortran library
#
# Key insight: MITgcm uses arrays with halo regions, e.g.:
#   theta(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr)
# This means the actual array size is (sNx+2*OLx, sNy+2*OLy, Nr)
# and index 1 in Fortran maps to index OLx+1 in the Julia array

const libpath = joinpath(@__DIR__, "libmitgcm.dylib")

# Get dimensions from Fortran
function get_dims()
    nx = Ref{Int32}(0)
    ny = Ref{Int32}(0)
    nz = Ref{Int32}(0)
    olx = Ref{Int32}(0)
    oly = Ref{Int32}(0)
    ccall((:get_dims_, libpath), Cvoid,
          (Ref{Int32}, Ref{Int32}, Ref{Int32}, Ref{Int32}, Ref{Int32}),
          nx, ny, nz, olx, oly)
    return (sNx=nx[], sNy=ny[], Nr=nz[], OLx=olx[], OLy=oly[])
end

# Initialize the model
function mitgcm_init()
    ccall((:mitgcm_init_, libpath), Cvoid, ())
end

# Run one timestep
function mitgcm_timestep(deltaT::Float64)
    ccall((:mitgcm_timestep_, libpath), Cvoid, (Ref{Float64},), deltaT)
end

# Direct access to COMMON blocks
# Note: Arrays include halo regions, so size is (sNx+2*OLx, sNy+2*OLy, ...)
function get_dynvars_ptr()
    dims = get_dims()
    nx_full = Int(dims.sNx + 2*dims.OLx)  # 4 + 2*1 = 6
    ny_full = Int(dims.sNy + 2*dims.OLy)  # 3 + 2*1 = 5
    nr = Int(dims.Nr)

    ptr = cglobal((:dynvars_r_, libpath), Float64)

    # Calculate sizes and offsets for each variable in COMMON block
    # Order in COMMON: etaN, uVel, vVel, theta, salt
    size_2d = nx_full * ny_full           # 6*5 = 30
    size_3d = nx_full * ny_full * nr      # 6*5*2 = 60

    offset = 0
    etaN = unsafe_wrap(Array, ptr + offset*8, (nx_full, ny_full))
    offset += size_2d

    uVel = unsafe_wrap(Array, ptr + offset*8, (nx_full, ny_full, nr))
    offset += size_3d

    vVel = unsafe_wrap(Array, ptr + offset*8, (nx_full, ny_full, nr))
    offset += size_3d

    theta = unsafe_wrap(Array, ptr + offset*8, (nx_full, ny_full, nr))
    offset += size_3d

    salt = unsafe_wrap(Array, ptr + offset*8, (nx_full, ny_full, nr))

    return (etaN=etaN, uVel=uVel, vVel=vVel, theta=theta, salt=salt)
end

function get_grid_ptr()
    dims = get_dims()
    nx_full = Int(dims.sNx + 2*dims.OLx)
    ny_full = Int(dims.sNy + 2*dims.OLy)
    nr = Int(dims.Nr)

    ptr = cglobal((:grid_rs_, libpath), Float64)

    # Order in COMMON: dxC, dyC, drF, xC, yC, rC
    size_2d = nx_full * ny_full
    size_1d = nr

    offset = 0
    dxC = unsafe_wrap(Array, ptr + offset*8, (nx_full, ny_full))
    offset += size_2d

    dyC = unsafe_wrap(Array, ptr + offset*8, (nx_full, ny_full))
    offset += size_2d

    drF = unsafe_wrap(Array, ptr + offset*8, nr)
    offset += size_1d

    xC = unsafe_wrap(Array, ptr + offset*8, (nx_full, ny_full))
    offset += size_2d

    yC = unsafe_wrap(Array, ptr + offset*8, (nx_full, ny_full))
    offset += size_2d

    rC = unsafe_wrap(Array, ptr + offset*8, nr)

    return (dxC=dxC, dyC=dyC, drF=drF, xC=xC, yC=yC, rC=rC)
end

# Helper to extract interior (non-halo) region
function interior(arr, dims)
    olx, oly = dims.OLx, dims.OLy
    if ndims(arr) == 2
        return arr[olx+1:end-olx, oly+1:end-oly]
    else
        return arr[olx+1:end-olx, oly+1:end-oly, :]
    end
end

# ============ Demo ============
println("=== MITgcm-style Fortran-Julia Demo ===\n")

dims = get_dims()
println("Grid dimensions:")
println("  sNx=$(dims.sNx), sNy=$(dims.sNy), Nr=$(dims.Nr)")
println("  OLx=$(dims.OLx), OLy=$(dims.OLy) (halo size)")
println("  Full array size: ($(dims.sNx + 2*dims.OLx), $(dims.sNy + 2*dims.OLy))\n")

println("1. Initializing model...")
mitgcm_init()

# Get direct pointers to COMMON block data
dynvars = get_dynvars_ptr()
grid = get_grid_ptr()

println("\n2. Grid variables (xC, yC at interior points):")
println("   xC (interior):")
display(interior(grid.xC, dims))

println("\n   rC (depth levels): ", grid.rC)

println("\n3. Initial state - theta (interior, level 1):")
display(interior(dynvars.theta[:,:,1], dims))

println("\n4. Running 10 timesteps (deltaT=100s)...")
for i in 1:10
    mitgcm_timestep(100.0)
end

println("\n5. State after 10 timesteps - theta (interior, level 1):")
display(interior(dynvars.theta[:,:,1], dims))

println("\n6. Modifying theta directly from Julia...")
# Set a warm anomaly in the center
dynvars.theta[4, 3, 1] = 25.0  # Note: includes halo offset
println("   theta after modification (interior, level 1):")
display(interior(dynvars.theta[:,:,1], dims))

println("\n7. Accessing salt (interior, all levels):")
for k in 1:dims.Nr
    println("   Level $k: salt = ", interior(dynvars.salt[:,:,k], dims)[1,1])
end

println("\n=== Memory Layout Info ===")
println("COMMON /DYNVARS_R/ contains:")
println("  etaN:  $(prod(size(dynvars.etaN))) elements")
println("  uVel:  $(prod(size(dynvars.uVel))) elements")
println("  vVel:  $(prod(size(dynvars.vVel))) elements")
println("  theta: $(prod(size(dynvars.theta))) elements")
println("  salt:  $(prod(size(dynvars.salt))) elements")
total = prod(size(dynvars.etaN)) + 4*prod(size(dynvars.theta))
println("  Total: $total Float64 values = $(total * 8) bytes")

println("\n=== Done ===")

# Julia interface for Fortran library with multiple named arrays in COMMON block

const libpath = joinpath(@__DIR__, "libmylib_multi.dylib")

# Low-level functions
function get_dims()
    m = Ref{Int32}(0)
    n = Ref{Int32}(0)
    ccall((:getdims_, libpath), Cvoid, (Ref{Int32}, Ref{Int32}), m, n)
    return (m[], n[])
end

function get_num_arrays()
    n = Ref{Int32}(0)
    ccall((:getnumarrays_, libpath), Cvoid, (Ref{Int32},), n)
    return n[]
end

function get_array_name(idx::Integer)
    name = Vector{UInt8}(undef, 16)
    ccall((:getarrayname_, libpath), Cvoid, (Ref{Int32}, Ptr{UInt8}), Int32(idx), name)
    return String(strip(String(name)))  # strip whitespace and convert to String
end

function get_array_index(name::String)
    # Pad name to 16 chars for Fortran
    padded = rpad(name, 16)[1:16]
    idx = Ref{Int32}(0)
    ccall((:getarrayindex_, libpath), Cvoid, (Ptr{UInt8}, Ref{Int32}, Clong), padded, idx, 16)
    return idx[]
end

function init_arrays()
    ccall((:initarrays_, libpath), Cvoid, ())
end

# Array access by index
function get_array_by_idx(idx::Integer)
    m, n = get_dims()
    arr = zeros(Float64, m, n)
    ccall((:getarraybyidx_, libpath), Cvoid,
          (Ref{Int32}, Ptr{Float64}, Ref{Int32}, Ref{Int32}),
          Int32(idx), arr, Int32(m), Int32(n))
    return arr
end

function set_array_by_idx(idx::Integer, arr::Matrix{Float64})
    m, n = size(arr)
    ccall((:setarraybyidx_, libpath), Cvoid,
          (Ref{Int32}, Ptr{Float64}, Ref{Int32}, Ref{Int32}),
          Int32(idx), arr, Int32(m), Int32(n))
end

function scale_array_by_idx(idx::Integer, factor::Float64)
    ccall((:scalearraybyidx_, libpath), Cvoid,
          (Ref{Int32}, Ref{Float64}), Int32(idx), factor)
end

# High-level interface: access by name
function list_arrays()
    n = get_num_arrays()
    return [get_array_name(i) for i in 1:n]
end

function get_array(name::AbstractString)
    idx = get_array_index(String(name))
    idx == 0 && error("Array '$name' not found. Available: $(list_arrays())")
    return get_array_by_idx(idx)
end

function set_array(name::AbstractString, arr::Matrix{Float64})
    idx = get_array_index(String(name))
    idx == 0 && error("Array '$name' not found. Available: $(list_arrays())")
    set_array_by_idx(idx, arr)
end

function scale_array(name::AbstractString, factor::Float64)
    idx = get_array_index(String(name))
    idx == 0 && error("Array '$name' not found. Available: $(list_arrays())")
    scale_array_by_idx(idx, factor)
end

# Direct COMMON block access - arrays stored contiguously
function get_common_block_ptr(name::AbstractString)
    idx = get_array_index(String(name))
    idx == 0 && error("Array '$name' not found. Available: $(list_arrays())")

    m, n = get_dims()
    # All arrays in COMMON block are contiguous, each is m*n elements
    ptr = cglobal((:datablock_, libpath), Float64)
    offset = (idx - 1) * m * n
    return unsafe_wrap(Array, ptr + offset * sizeof(Float64), (m, n))
end

# ============ Demo ============
println("=== Multiple Named Arrays Demo ===\n")

println("Available arrays: ", list_arrays())
m, n = get_dims()
println("Array dimensions: $m x $n\n")

println("1. Initializing all arrays...")
init_arrays()

for name in list_arrays()
    println("\n--- $name ---")
    display(get_array(name))
end

println("\n\n2. Scaling 'velocity' by 10.0...")
scale_array("velocity", 10.0)
println("Velocity after scaling:")
display(get_array("velocity"))

println("\n3. Setting 'pressure' from Julia...")
new_pressure = fill(1000.0, m, n)
new_pressure[1,:] .= 999.0  # first row
set_array("pressure", new_pressure)
println("Pressure after set:")
display(get_array("pressure"))

println("\n4. Direct COMMON block access to 'temperature':")
temp_ptr = get_common_block_ptr("temperature")
println("Type: ", typeof(temp_ptr))
temp_ptr[1,1] = 0.0  # modify directly
temp_ptr[m,n] = 500.0
println("Temperature after direct modification:")
display(get_array("temperature"))

println("\n=== COMMON Block Memory Layout ===")
ptr = cglobal((:datablock_, libpath), Float64)
total_elements = m * n * get_num_arrays()
all_data = unsafe_wrap(Array, ptr, total_elements)
println("Total elements in COMMON block: $total_elements")
println("Layout: [temperature($(m*n))] [velocity($(m*n))] [pressure($(m*n))]")
println("\nFirst 3 elements of each array:")
for (i, name) in enumerate(list_arrays())
    start = (i-1) * m * n + 1
    println("  $name: $(all_data[start:start+2])")
end

println("\n=== Done ===")

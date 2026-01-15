# Julia interface to Fortran library with 2D array in COMMON block
#
# Key point: Fortran and Julia both use COLUMN-MAJOR order,
# so a Fortran array(4,3) maps directly to a Julia Matrix(4,3)

const libpath = joinpath(@__DIR__, "libmylib2d.dylib")

function init_array2d()
    ccall((:initarray2d_, libpath), Cvoid, ())
end

function scale_array2d(factor::Float64)
    ccall((:scalearray2d_, libpath), Cvoid, (Ref{Float64},), factor)
end

function sum_array2d()
    ccall((:sumarray2d_, libpath), Float64, ())
end

function get_dims()
    m = Ref{Int32}(0)
    n = Ref{Int32}(0)
    ccall((:getdims_, libpath), Cvoid, (Ref{Int32}, Ref{Int32}), m, n)
    return (m[], n[])
end

function get_array2d()
    m, n = get_dims()
    arr = zeros(Float64, m, n)
    ccall((:getarray2d_, libpath), Cvoid,
          (Ptr{Float64}, Ref{Int32}, Ref{Int32}), arr, m, n)
    return arr
end

function set_array2d(arr::Matrix{Float64})
    m, n = size(arr)
    ccall((:setarray2d_, libpath), Cvoid,
          (Ptr{Float64}, Ref{Int32}, Ref{Int32}), arr, Int32(m), Int32(n))
end

# Direct access to COMMON block - the 2D array is stored as contiguous column-major memory
function get_common_block_ptr()
    m, n = get_dims()
    ptr = cglobal((:arrayblock2d_, libpath), Float64)
    # unsafe_wrap creates a Julia view directly into Fortran memory
    return unsafe_wrap(Array, ptr, (m, n))
end

# ============ Demo ============
println("=== Fortran-Julia 2D Array Interop Demo ===\n")

m, n = get_dims()
println("Array dimensions: $m x $n")

println("\n1. Initializing 2D array...")
init_array2d()
arr = get_array2d()
println("   Array after init:")
display(arr)

println("\n2. Scaling array by 0.5...")
scale_array2d(0.5)
arr = get_array2d()
println("   Array after scaling:")
display(arr)

println("\n3. Sum of array: ", sum_array2d())

println("\n4. Setting array from Julia matrix...")
new_arr = Float64[
    1.0  5.0   9.0
    2.0  6.0  10.0
    3.0  7.0  11.0
    4.0  8.0  12.0
]
println("   Julia matrix to send (4x3):")
display(new_arr)
set_array2d(new_arr)
println("\n   Fortran array after set:")
display(get_array2d())

println("\n5. Direct COMMON block access (zero-copy view):")
common_arr = get_common_block_ptr()
println("   Type: ", typeof(common_arr))
display(common_arr)

println("\n6. Modifying COMMON block directly from Julia...")
common_arr[1, 1] = 999.0   # top-left
common_arr[4, 3] = 888.0   # bottom-right
common_arr[:, 2] .= 0.0    # entire second column
println("   After modification:")
display(get_array2d())

println("\n=== Memory Layout Demonstration ===")
println("Fortran COMMON block memory (linear view):")
ptr = cglobal((:arrayblock2d_, libpath), Float64)
linear = unsafe_wrap(Array, ptr, m * n)
println(linear)
println("\nNote: Column-major order - columns are contiguous in memory")
println("Column 1: $(linear[1:4])")
println("Column 2: $(linear[5:8])")
println("Column 3: $(linear[9:12])")

println("\n=== Done ===")

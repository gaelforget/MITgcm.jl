# Julia interface to Fortran library with COMMON block
#
# Build commands (run from this directory):
#   gfortran -c -fPIC mylib.f -o mylib.o
#   ar rcs libmylib.a mylib.o                           # static library
#   gfortran -shared -o libmylib.dylib mylib.o          # shared library (macOS)
#   # On Linux use: gfortran -shared -o libmylib.so mylib.o

const libpath = joinpath(@__DIR__, "libmylib.dylib")

# Note: gfortran adds underscores to subroutine names by default

function init_array()
    ccall((:initarray_, libpath), Cvoid, ())
end

function scale_array(factor::Float64)
    ccall((:scalearray_, libpath), Cvoid, (Ref{Float64},), factor)
end

function sum_array()
    ccall((:sumarray_, libpath), Float64, ())
end

function get_array()
    arr = zeros(Float64, 10)
    n = Ref{Int32}(10)
    ccall((:getarray_, libpath), Cvoid, (Ptr{Float64}, Ref{Int32}), arr, n)
    return arr
end

function set_array(arr::Vector{Float64})
    n = Ref{Int32}(length(arr))
    ccall((:setarray_, libpath), Cvoid, (Ptr{Float64}, Ref{Int32}), arr, n)
end

# Alternative: Direct access to COMMON block data
function get_common_block_ptr()
    # The COMMON block is exported as a symbol (lowercase with underscore)
    ptr = cglobal((:arrayblock_, libpath), Float64)
    return unsafe_wrap(Array, ptr, 10)
end

# ============ Demo ============
println("=== Fortran-Julia Interop Demo ===\n")

println("1. Initializing array...")
init_array()
arr = get_array()
println("   Array after init: ", arr)

println("\n2. Scaling array by 2.0...")
scale_array(2.0)
arr = get_array()
println("   Array after scaling: ", arr)

println("\n3. Sum of array: ", sum_array())

println("\n4. Setting array from Julia...")
set_array([100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0, 1000.0])
arr = get_array()
println("   Array after set: ", arr)

println("\n5. Direct COMMON block access:")
common_arr = get_common_block_ptr()
println("   Direct read: ", common_arr)

println("\n6. Modifying COMMON block directly from Julia...")
common_arr[1] = 999.0
common_arr[10] = 111.0
println("   After modification: ", get_array())

println("\n=== Done ===")

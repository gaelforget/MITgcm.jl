using Libdl

"""
    MITgcmError(message)

Exception thrown when MITgcm encounters a fatal error (Fortran STOP).
Instead of killing the Julia process, the error is caught and re-thrown
as a Julia exception.
"""
struct MITgcmError <: Exception
    message::String
end
Base.showerror(io::IO, e::MITgcmError) = print(io, "MITgcmError: ", e.message)

"""
    MITgcmLibrary

Low-level interface to MITgcm loaded as a shared library.
Wraps dlopen/dlclose and provides ccall wrappers for all Fortran subroutines
exposed by `mitgcm_wrapper.F`.

Set `verbose=false` to suppress MITgcm's Fortran stdout/stderr output.
"""
mutable struct MITgcmLibrary
    handle      :: Ptr{Nothing}
    tmpfile     :: String
    libpath     :: String
    run_dir     :: String
    dims        :: @NamedTuple{sNx::Int, sNy::Int, Nr::Int,
                               OLx::Int, OLy::Int,
                               nSx::Int, nSy::Int,
                               nPx::Int, nPy::Int,
                               Nx::Int,  Ny::Int}
    initialized :: Bool
    verbose     :: Bool
end

function MITgcmLibrary(libpath::String, run_dir::String; verbose::Bool = true)
    return MITgcmLibrary(
        C_NULL,
        "",
        libpath,
        run_dir,
        (sNx=0, sNy=0, Nr=0, OLx=0, OLy=0, nSx=0, nSy=0, nPx=0, nPy=0, Nx=0, Ny=0),
        false,
        verbose,
    )
end

# Resolve a symbol from the loaded library
function _sym(lib::MITgcmLibrary, name::Symbol)
    lib.handle == C_NULL && error("MITgcm library not loaded")
    return dlsym(lib.handle, name)
end

# Suppress C-level stdout/stderr by redirecting file descriptors to /dev/null.
# This captures Fortran WRITE(*,*) output which goes directly to fd 1/2.
function _with_output_control(f, lib::MITgcmLibrary)
    if lib.verbose
        return f()
    end
    old_stdout = ccall(:dup, Cint, (Cint,), 1)
    old_stderr = ccall(:dup, Cint, (Cint,), 2)
    devnull = ccall(:open, Cint, (Cstring, Cint), "/dev/null", 1)  # O_WRONLY
    ccall(:dup2, Cint, (Cint, Cint), devnull, 1)
    ccall(:dup2, Cint, (Cint, Cint), devnull, 2)
    ccall(:close, Cint, (Cint,), devnull)
    try
        return f()
    finally
        ccall(:dup2, Cint, (Cint, Cint), old_stdout, 1)
        ccall(:dup2, Cint, (Cint, Cint), old_stderr, 2)
        ccall(:close, Cint, (Cint,), old_stdout)
        ccall(:close, Cint, (Cint,), old_stderr)
    end
end

# Run a function in the MITgcm run directory, then restore the previous directory.
# MITgcm reads/writes files relative to CWD during all operations.
function _in_run_dir(f, lib::MITgcmLibrary)
    prev = pwd()
    cd(lib.run_dir)
    try
        return f()
    finally
        cd(prev)
    end
end

# Retrieve the error message from the last intercepted STOP.
function _get_error_message(lib::MITgcmLibrary)
    buf = Vector{UInt8}(undef, 512)
    buflen = Ref{Int32}(512)
    ccall(_sym(lib, :mitgcm_lib_get_error_msg_), Cvoid,
          (Ptr{UInt8}, Ref{Int32}), buf, buflen)
    return String(buf[1:buflen[]])
end

"""
    load!(lib::MITgcmLibrary)

Load the shared library. Uses a temp-file copy so the library can be
reloaded in the same Julia session (macOS dyld caches dlopen handles).
"""
function load!(lib::MITgcmLibrary)
    if lib.handle != C_NULL
        dlclose(lib.handle)
        lib.handle = C_NULL
    end
    ext = Sys.isapple() ? ".dylib" : ".so"
    lib.tmpfile = tempname() * ext
    cp(lib.libpath, lib.tmpfile)
    lib.handle = dlopen(lib.tmpfile)
    return lib
end

"""
    unload!(lib::MITgcmLibrary)

Unload the shared library and clean up the temp file.
"""
function unload!(lib::MITgcmLibrary)
    if lib.handle != C_NULL
        dlclose(lib.handle)
        lib.handle = C_NULL
    end
    if !isempty(lib.tmpfile) && isfile(lib.tmpfile)
        rm(lib.tmpfile, force=true)
        lib.tmpfile = ""
    end
    lib.initialized = false
    return lib
end

"""
    init!(lib::MITgcmLibrary)

Initialize MITgcm: boot execution environment, load grid, parameters,
and initial conditions. Must be called from the run directory.

If MITgcm hits a fatal error (Fortran STOP), throws `MITgcmError` instead
of killing the Julia process.
"""
function init!(lib::MITgcmLibrary)
    load!(lib)
    _in_run_dir(lib) do
        _with_output_control(lib) do
            status = ccall(_sym(lib, :mitgcm_lib_safe_init_), Cint, ())
            if status != 0
                msg = _get_error_message(lib)
                throw(MITgcmError("MITgcm initialization failed: $msg"))
            end
        end
        lib.dims = get_dims(lib)
        lib.initialized = true
    end
    return lib
end

"""
    step!(lib::MITgcmLibrary)

Advance MITgcm by one forward time step.

If MITgcm hits a fatal error (Fortran STOP), throws `MITgcmError` instead
of killing the Julia process.
"""
function step!(lib::MITgcmLibrary)
    _in_run_dir(lib) do
        _with_output_control(lib) do
            status = ccall(_sym(lib, :mitgcm_lib_safe_step_), Cint, ())
            if status != 0
                msg = _get_error_message(lib)
                throw(MITgcmError("MITgcm step failed: $msg"))
            end
        end
    end
end

"""
    finalize!(lib::MITgcmLibrary)

Shut down MITgcm (close files, print timers) and unload the library.
"""
function finalize!(lib::MITgcmLibrary)
    if lib.initialized
        _in_run_dir(lib) do
            ccall(_sym(lib, :mitgcm_lib_finalize_), Cvoid, ())
        end
    end
    unload!(lib)
    return lib
end

# ============================================================
# Dimension and state queries
# ============================================================

function get_dims(lib::MITgcmLibrary)
    sNx = Ref{Int32}(0); sNy = Ref{Int32}(0); Nr  = Ref{Int32}(0)
    OLx = Ref{Int32}(0); OLy = Ref{Int32}(0)
    nSx = Ref{Int32}(0); nSy = Ref{Int32}(0)
    nPx = Ref{Int32}(0); nPy = Ref{Int32}(0)
    Nx  = Ref{Int32}(0); Ny  = Ref{Int32}(0)
    ccall(_sym(lib, :mitgcm_lib_get_dims_), Cvoid,
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

function get_niter(lib::MITgcmLibrary)
    niter = Ref{Int32}(0)
    ccall(_sym(lib, :mitgcm_lib_get_niter_), Cvoid, (Ref{Int32},), niter)
    return Int(niter[])
end

function get_time(lib::MITgcmLibrary)
    t = Ref{Float64}(0.0)
    ccall(_sym(lib, :mitgcm_lib_get_time_), Cvoid, (Ref{Float64},), t)
    return t[]
end

# ============================================================
# Timestep control
# ============================================================

"""
    set_timestep!(lib::MITgcmLibrary, dt::Float64)

Set all MITgcm timestep parameters (deltaT, deltaTMom, deltaTClock,
deltaTFreeSurf, dTtracerLev) to `dt` seconds.
"""
function set_timestep!(lib::MITgcmLibrary, dt::Float64)
    ccall(_sym(lib, :mitgcm_lib_set_timestep_), Cvoid, (Ref{Float64},), Ref{Float64}(dt))
end

"""
    get_timestep(lib::MITgcmLibrary)

Return the current base timestep (deltaT) in seconds.
"""
function get_timestep(lib::MITgcmLibrary)
    dt = Ref{Float64}(0.0)
    ccall(_sym(lib, :mitgcm_lib_get_deltat_), Cvoid, (Ref{Float64},), dt)
    return dt[]
end

# ============================================================
# 3D field getters (Nx, Ny, Nr)
# ============================================================

get_theta!(lib::MITgcmLibrary, a::Array{Float64,3})  = ccall(_sym(lib, :mitgcm_lib_get_theta_),  Cvoid, (Ref{Float64},), a)
get_salt!(lib::MITgcmLibrary, a::Array{Float64,3})   = ccall(_sym(lib, :mitgcm_lib_get_salt_),   Cvoid, (Ref{Float64},), a)
get_uvel!(lib::MITgcmLibrary, a::Array{Float64,3})   = ccall(_sym(lib, :mitgcm_lib_get_uvel_),   Cvoid, (Ref{Float64},), a)
get_vvel!(lib::MITgcmLibrary, a::Array{Float64,3})   = ccall(_sym(lib, :mitgcm_lib_get_vvel_),   Cvoid, (Ref{Float64},), a)
get_wvel!(lib::MITgcmLibrary, a::Array{Float64,3})   = ccall(_sym(lib, :mitgcm_lib_get_wvel_),   Cvoid, (Ref{Float64},), a)
get_hfacc!(lib::MITgcmLibrary, a::Array{Float64,3})  = ccall(_sym(lib, :mitgcm_lib_get_hfacc_),  Cvoid, (Ref{Float64},), a)

# ============================================================
# 2D field getters (Nx, Ny)
# ============================================================

get_etan!(lib::MITgcmLibrary, a::Array{Float64,2})   = ccall(_sym(lib, :mitgcm_lib_get_etan_),   Cvoid, (Ref{Float64},), a)
get_xc!(lib::MITgcmLibrary, a::Array{Float64,2})     = ccall(_sym(lib, :mitgcm_lib_get_xc_),     Cvoid, (Ref{Float64},), a)
get_yc!(lib::MITgcmLibrary, a::Array{Float64,2})     = ccall(_sym(lib, :mitgcm_lib_get_yc_),     Cvoid, (Ref{Float64},), a)
get_rlow!(lib::MITgcmLibrary, a::Array{Float64,2})   = ccall(_sym(lib, :mitgcm_lib_get_rlow_),   Cvoid, (Ref{Float64},), a)

# ============================================================
# 1D field getters (Nr)
# ============================================================

get_rc!(lib::MITgcmLibrary, a::Array{Float64,1})     = ccall(_sym(lib, :mitgcm_lib_get_rc_),     Cvoid, (Ref{Float64},), a)
get_drf!(lib::MITgcmLibrary, a::Array{Float64,1})    = ccall(_sym(lib, :mitgcm_lib_get_drf_),    Cvoid, (Ref{Float64},), a)

# ============================================================
# 3D field setters (Nx, Ny, Nr)
# ============================================================

set_theta!(lib::MITgcmLibrary, a::Array{Float64,3})  = ccall(_sym(lib, :mitgcm_lib_set_theta_),  Cvoid, (Ref{Float64},), a)
set_salt!(lib::MITgcmLibrary, a::Array{Float64,3})   = ccall(_sym(lib, :mitgcm_lib_set_salt_),   Cvoid, (Ref{Float64},), a)

# ============================================================
# Surface forcing getters (Nx, Ny)
# ============================================================

get_fu!(lib::MITgcmLibrary, a::Array{Float64,2})       = ccall(_sym(lib, :mitgcm_lib_get_fu_),       Cvoid, (Ref{Float64},), a)
get_fv!(lib::MITgcmLibrary, a::Array{Float64,2})       = ccall(_sym(lib, :mitgcm_lib_get_fv_),       Cvoid, (Ref{Float64},), a)
get_qnet!(lib::MITgcmLibrary, a::Array{Float64,2})     = ccall(_sym(lib, :mitgcm_lib_get_qnet_),     Cvoid, (Ref{Float64},), a)
get_empmr!(lib::MITgcmLibrary, a::Array{Float64,2})    = ccall(_sym(lib, :mitgcm_lib_get_empmr_),    Cvoid, (Ref{Float64},), a)
get_qsw!(lib::MITgcmLibrary, a::Array{Float64,2})      = ccall(_sym(lib, :mitgcm_lib_get_qsw_),      Cvoid, (Ref{Float64},), a)
get_saltflux!(lib::MITgcmLibrary, a::Array{Float64,2}) = ccall(_sym(lib, :mitgcm_lib_get_saltflux_), Cvoid, (Ref{Float64},), a)

# ============================================================
# Surface forcing setters (Nx, Ny)
# ============================================================

set_fu!(lib::MITgcmLibrary, a::Array{Float64,2})       = ccall(_sym(lib, :mitgcm_lib_set_fu_),       Cvoid, (Ref{Float64},), a)
set_fv!(lib::MITgcmLibrary, a::Array{Float64,2})       = ccall(_sym(lib, :mitgcm_lib_set_fv_),       Cvoid, (Ref{Float64},), a)
set_qnet!(lib::MITgcmLibrary, a::Array{Float64,2})     = ccall(_sym(lib, :mitgcm_lib_set_qnet_),     Cvoid, (Ref{Float64},), a)
set_empmr!(lib::MITgcmLibrary, a::Array{Float64,2})    = ccall(_sym(lib, :mitgcm_lib_set_empmr_),    Cvoid, (Ref{Float64},), a)
set_qsw!(lib::MITgcmLibrary, a::Array{Float64,2})      = ccall(_sym(lib, :mitgcm_lib_set_qsw_),      Cvoid, (Ref{Float64},), a)
set_saltflux!(lib::MITgcmLibrary, a::Array{Float64,2}) = ccall(_sym(lib, :mitgcm_lib_set_saltflux_), Cvoid, (Ref{Float64},), a)

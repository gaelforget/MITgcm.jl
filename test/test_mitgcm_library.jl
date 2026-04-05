using MITgcm, Test

# Skip the entire file if gfortran is not available
has_gfortran = try success(`gfortran --version`) catch; false end

if !has_gfortran
    @warn "gfortran not found — skipping MITgcm shared library tests"
else

n_test=2

@testset "MITgcm shared library" begin
    # Download MITgcm source (cached in scratch space)
    mitgcm_dir = MITgcm.download_mitgcm_source()
    @test isdir(mitgcm_dir)
    @test isfile(joinpath(mitgcm_dir, "tools", "genmake2"))

    # Use default global_oce_latlon experiment
    code_dir  = MITgcm.default_code_dir(mitgcm_dir)
    input_dir = MITgcm.default_input_dir(mitgcm_dir)
    @test isdir(code_dir)
    @test isdir(input_dir)

    output_dir = mktempdir()

    #@testset "build_mitgcm_library" 
    if n_test>1
        result = MITgcm.build_mitgcm_library(mitgcm_dir;
                                              output_dir, code_dir, input_dir, verbose=true)
        @test isfile(result.library_path)
        @test isdir(result.run_dir)
    end

    lib_name = Sys.isapple() ? "libmitgcm.dylib" : "libmitgcm.so"
    library_path = joinpath(output_dir, lib_name)
    run_dir = joinpath(output_dir, "run")

    #@testset "MITgcmLibrary low-level"
    if n_test>2
        lib = MITgcmLibrary(library_path, run_dir; verbose=false)
        @test lib.handle == C_NULL
        @test lib.initialized == false

        # Initialize
        init!(lib)
        @test lib.initialized == true
        @test lib.handle != C_NULL

        # Dimensions
        dims = lib.dims
        @test dims.Nx > 0
        @test dims.Ny > 0
        @test dims.Nr > 0
        @test dims.Nx == dims.sNx * dims.nSx * dims.nPx
        @test dims.Ny == dims.sNy * dims.nSy * dims.nPy

        Nx, Ny, Nr = dims.Nx, dims.Ny, dims.Nr

        # Iteration and time
        @test MITgcm.get_niter(lib) == 0
        @test MITgcm.get_time(lib) == 0.0

        # Timestep control
        dt0 = get_timestep(lib)
        @test dt0 > 0.0

        set_timestep!(lib, 600.0)
        @test get_timestep(lib) == 600.0

        # Restore original
        set_timestep!(lib, dt0)
        @test get_timestep(lib) == dt0

        # 3D field getters
        theta = zeros(Float64, Nx, Ny, Nr)
        salt  = zeros(Float64, Nx, Ny, Nr)
        uvel  = zeros(Float64, Nx, Ny, Nr)
        vvel  = zeros(Float64, Nx, Ny, Nr)
        wvel  = zeros(Float64, Nx, Ny, Nr)
        hfacc = zeros(Float64, Nx, Ny, Nr)

        get_theta!(lib, theta)
        get_salt!(lib, salt)
        get_uvel!(lib, uvel)
        get_vvel!(lib, vvel)
        get_wvel!(lib, wvel)
        get_hfacc!(lib, hfacc)

        # Check that initial theta/salt are non-trivial (not all zeros)
        @test any(theta .!= 0)
        @test any(salt .!= 0)
        # hFacC should have values in [0, 1]
        @test all(0 .<= hfacc .<= 1)

        # 2D field getters
        etan = zeros(Float64, Nx, Ny)
        xc   = zeros(Float64, Nx, Ny)
        yc   = zeros(Float64, Nx, Ny)
        rlow = zeros(Float64, Nx, Ny)

        get_etan!(lib, etan)
        get_xc!(lib, xc)
        get_yc!(lib, yc)
        get_rlow!(lib, rlow)

        # Grid coordinates should be non-trivial
        @test any(xc .!= 0)
        @test any(yc .!= 0)
        # Longitude should span roughly [0, 360) or [-180, 180)
        @test maximum(xc) - minimum(xc) > 100.0

        # 1D field getters
        rc  = zeros(Float64, Nr)
        drf = zeros(Float64, Nr)
        get_rc!(lib, rc)
        get_drf!(lib, drf)

        @test all(rc .< 0)       # depth centers are negative
        @test all(drf .> 0)      # level thicknesses are positive

        # 3D field setters (round-trip)
        theta_orig = copy(theta)
        theta_mod  = theta .+ 1.0
        set_theta!(lib, theta_mod)
        get_theta!(lib, theta)
        @test theta ≈ theta_mod

        # Restore original
        set_theta!(lib, theta_orig)

        # Surface forcing setters/getters (round-trip)
        fu_set = rand(Float64, Nx, Ny) .- 0.5
        set_fu!(lib, fu_set)
        fu_get = zeros(Float64, Nx, Ny)
        get_fu!(lib, fu_get)
        @test fu_get ≈ fu_set

        fv_set = rand(Float64, Nx, Ny) .- 0.5
        set_fv!(lib, fv_set)
        fv_get = zeros(Float64, Nx, Ny)
        get_fv!(lib, fv_get)
        @test fv_get ≈ fv_set

        qnet_set = rand(Float64, Nx, Ny) .* 200 .- 100
        set_qnet!(lib, qnet_set)
        qnet_get = zeros(Float64, Nx, Ny)
        get_qnet!(lib, qnet_get)
        @test qnet_get ≈ qnet_set

        empmr_set = rand(Float64, Nx, Ny) .* 1e-4
        set_empmr!(lib, empmr_set)
        empmr_get = zeros(Float64, Nx, Ny)
        get_empmr!(lib, empmr_get)
        @test empmr_get ≈ empmr_set

        qsw_set = rand(Float64, Nx, Ny) .* 300
        set_qsw!(lib, qsw_set)
        qsw_get = zeros(Float64, Nx, Ny)
        get_qsw!(lib, qsw_get)
        @test qsw_get ≈ qsw_set

        sf_set = rand(Float64, Nx, Ny) .* 1e-3
        set_saltflux!(lib, sf_set)
        sf_get = zeros(Float64, Nx, Ny)
        get_saltflux!(lib, sf_get)
        @test sf_get ≈ sf_set

        # Take a step
        step!(lib)
        @test MITgcm.get_niter(lib) == 1
        @test MITgcm.get_time(lib) > 0.0

        # State should have changed after stepping
        get_theta!(lib, theta)
        @test any(theta .!= theta_orig)

        # Finalize
        MITgcm.finalize!(lib)
        @test lib.initialized == false
        @test lib.handle == C_NULL
    end

    #@testset "MITgcmOceanSimulation"
    if n_test>3
        ocean = MITgcmOceanSimulation(library_path, run_dir; verbose=false)

        Nx = ocean.library.dims.Nx
        Ny = ocean.library.dims.Ny
        Nr = ocean.library.dims.Nr

        # Check array sizes
        @test size(ocean.theta) == (Nx, Ny, Nr)
        @test size(ocean.salt)  == (Nx, Ny, Nr)
        @test size(ocean.uvel)  == (Nx, Ny, Nr)
        @test size(ocean.vvel)  == (Nx, Ny, Nr)
        @test size(ocean.etan)  == (Nx, Ny)
        @test size(ocean.xc)    == (Nx, Ny)
        @test size(ocean.yc)    == (Nx, Ny)
        @test size(ocean.rc)    == (Nr,)
        @test size(ocean.drf)   == (Nr,)
        @test size(ocean.hfacc) == (Nx, Ny, Nr)
        @test size(ocean.fu)    == (Nx, Ny)
        @test size(ocean.fv)    == (Nx, Ny)
        @test size(ocean.qnet)  == (Nx, Ny)
        @test size(ocean.empmr) == (Nx, Ny)
        @test size(ocean.qsw)   == (Nx, Ny)
        @test size(ocean.saltflux) == (Nx, Ny)

        # State should have been fetched during construction
        @test any(ocean.theta .!= 0)
        @test any(ocean.salt  .!= 0)

        # Physical constants
        @test ocean.reference_density > 0
        @test ocean.heat_capacity > 0

        # eltype
        @test eltype(ocean) == Float64

        # refresh_state! should work
        refresh_state!(ocean)
        @test any(ocean.theta .!= 0)

        # Clean up
        MITgcm.finalize!(ocean.library)
    end

    #@testset "MITgcmOceanSimulation from source"
    if n_test>4
        output_dir2 = mktempdir()
        ocean = MITgcmOceanSimulation(mitgcm_dir;
                                       output_dir = output_dir2,
                                       verbose = false)

        @test ocean.library.initialized == true
        @test size(ocean.theta, 3) == ocean.library.dims.Nr
        @test any(ocean.theta .!= 0)

        MITgcm.finalize!(ocean.library)
    end

    #@testset "MITgcmError on invalid library"
    if n_test>5
        # Test that MITgcmError type works correctly
        err = MITgcmError("test error message")
        @test err.message == "test error message"
        io = IOBuffer()
        Base.showerror(io, err)
        @test String(take!(io)) == "MITgcmError: test error message"
    end
end

end # has_gfortran

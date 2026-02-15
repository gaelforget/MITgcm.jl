using Scratch

const MITGCM_GIT_URL = "https://github.com/MITgcm/MITgcm.git"
const MITGCM_DEFAULT_CHECKOUT = "checkpoint69j"

"""
    download_mitgcm_source(; destination, checkout, url)

Clone the MITgcm source repository.  Returns the path to the cloned directory.

If `destination` already contains a valid MITgcm checkout (has `tools/genmake2`),
it is reused without re-cloning.

Keyword Arguments
=================
- `destination::String`: Where to clone (default: a scratch directory).
- `checkout::String`: Git tag or branch to check out (default: `"$MITGCM_DEFAULT_CHECKOUT"`).
- `url::String`: Git repository URL (default: the official MITgcm repo).
"""
function download_mitgcm_source(;
        destination::String = joinpath(Scratch.@get_scratch!("mitgcm_source"), "MITgcm"),
        checkout::String    = MITGCM_DEFAULT_CHECKOUT,
        url::String         = MITGCM_GIT_URL)

    genmake = joinpath(destination, "tools", "genmake2")
    if isfile(genmake)
        @info "MITgcm source already present at $destination"
        return destination
    end

    @info "Cloning MITgcm from $url (checkout: $checkout) ..."
    mkpath(dirname(destination))
    run(`git clone --depth 1 --branch $checkout $url $destination`)
    @info "MITgcm source downloaded to $destination"
    return destination
end

"""
    build_mitgcm_library(mitgcm_dir; experiment, output_dir)

Build MITgcm as a shared library for a given verification experiment.

Arguments
=========
- `mitgcm_dir::String`: Path to the MITgcm source directory.

Keyword Arguments
=================
- `experiment::String`: Name of the MITgcm verification experiment (default: `"global_oce_latlon"`).
- `output_dir::String`: Directory for the shared library and run files (default: a temp directory).

Returns a NamedTuple `(library_path, run_dir)`.
"""
function build_mitgcm_library(mitgcm_dir::String;
                               experiment::String = "global_oce_latlon",
                               output_dir::String = mktempdir())

    mitgcm_dir = abspath(mitgcm_dir)
    output_dir = abspath(output_dir)

    pkg_dir = pkgdir(MITgcm)
    build_script = joinpath(pkg_dir, "lib", "build_mitgcm_lib.sh")
    wrapper_src  = joinpath(pkg_dir, "lib", "mitgcm_wrapper.F")

    if !isfile(build_script)
        error("Build script not found: $build_script")
    end
    if !isfile(wrapper_src)
        error("Fortran wrapper not found: $wrapper_src")
    end
    if !isdir(mitgcm_dir)
        error("MITgcm directory not found: $mitgcm_dir")
    end

    mkpath(output_dir)

    cmd = `bash $build_script $mitgcm_dir $experiment $output_dir $wrapper_src`
    @info "Building MITgcm shared library..." experiment output_dir
    run(cmd)

    lib_name = Sys.isapple() ? "libmitgcm.dylib" : "libmitgcm.so"
    library_path = joinpath(output_dir, lib_name)
    run_dir = joinpath(output_dir, "run")

    if !isfile(library_path)
        error("Build succeeded but library not found at: $library_path")
    end

    @info "MITgcm shared library built successfully." library_path run_dir
    return (; library_path, run_dir)
end

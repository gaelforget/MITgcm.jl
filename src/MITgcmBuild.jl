using Scratch
using MITgcm.ClimateModels.Suppressor

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

const DEFAULT_EXPERIMENT = "global_oce_latlon"

default_code_dir(mitgcm_dir) = joinpath(mitgcm_dir, "verification", DEFAULT_EXPERIMENT, "code")
default_input_dir(mitgcm_dir) = joinpath(mitgcm_dir, "verification", DEFAULT_EXPERIMENT, "input")

"""
    build_mitgcm_library(mitgcm_dir; output_dir, code_dir, input_dir)

Build MITgcm as a shared library.

Arguments
=========
- `mitgcm_dir::String`: Path to the MITgcm source directory.

Keyword Arguments
=================
- `output_dir::String`: Directory for the shared library and run files (default: a temp directory).
- `code_dir::String`: Code directory passed to `genmake2 -mods` (contains `SIZE.h`,
  `packages.conf`, CPP option headers). Defaults to the `global_oce_latlon` verification
  experiment's code directory.
- `input_dir::String`: Input directory with runtime config files (`data`, `data.pkg`, etc.).
  Defaults to the `global_oce_latlon` verification experiment's input directory.

Returns a NamedTuple `(library_path, run_dir)`.
"""
function build_mitgcm_library(mitgcm_dir::String;
                              output_dir::String = mktempdir(),
                              code_dir::String   = default_code_dir(mitgcm_dir),
                              input_dir::String  = default_input_dir(mitgcm_dir),
                              verbose=false)

    mitgcm_dir = abspath(mitgcm_dir)
    output_dir = abspath(output_dir)
    code_dir   = abspath(code_dir)
    input_dir  = abspath(input_dir)

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
    if !isdir(code_dir)
        error("Code directory not found: $code_dir")
    end
    if !isdir(input_dir)
        error("Input directory not found: $input_dir")
    end

    mkpath(output_dir)

    cmd = `bash $build_script $mitgcm_dir $output_dir $code_dir $input_dir $wrapper_src`
    @info "Building MITgcm shared library..." output_dir code_dir input_dir
    if verbose
        run(cmd)
    else
        @suppress run(cmd)
    end

    lib_name = Sys.isapple() ? "libmitgcm.dylib" : "libmitgcm.so"
    library_path = joinpath(output_dir, lib_name)
    run_dir = joinpath(output_dir, "run")

    if !isfile(library_path)
        error("Build succeeded but library not found at: $library_path")
    end

    @info "MITgcm shared library built successfully." library_path run_dir
    return (; library_path, run_dir)
end

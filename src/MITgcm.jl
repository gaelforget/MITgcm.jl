module MITgcm

using Dates, Printf, SparseArrays, UUIDs
using MeshArrays, ClimateModels
using Glob, FortranFiles

using ClimateModels.DataFrames
using ClimateModels.Suppressor
using ClimateModels.OrderedCollections

include("Types.jl")
include("ReadFiles.jl")
include("ReadNativeGridFiles.jl")
import MITgcm.ReadNativeGridFiles.GridLoad_native
include("ModelSteps.jl")
include("FormatConversions.jl")
include("PhysicalOceanography.jl")
include("ModelConfigurations.jl")
include("ShellScripting.jl")

export MITgcm_path, MITgcmScratchSpaces
export MITgcm_download, HS94_pickup_download
export MITgcm_config, MITgcm_namelist, MITgcm_launch
export testreport, build, compile, setup, clean, launch
export monitor #pause, stop, clock, train, help
export verification_experiments, setup_verification!, testreport
export setup_ECCO4!, ECCO4_inputs, ECCO4_testreport
export read_namelist, write_namelist, read_toml
export read_all_namelists, write_all_namelists
export read_mdsio, read_meta, read_available_diagnostics
export scan_rundir, scan_stdout
export read_bin, read_flt, read_mnc, read_nctiles, findtiles
export GridLoad_mnc, GridLoad_mdsio, GridLoad_native
export cube2compact, compact2cube, convert2array, convert2gcmfaces
export SeaWaterDensity, MixedLayerDepth

"""
    MITgcm_path

Path to a MITgcm folder. `MITgcm_path[1]` should generally be used. `MITgcm_path[2]` is mostly
meant to facilitate comparisons between e.g. MITgcm releases when needed.
"""
MITgcm_path = [ "" , ""]

module downloads
    import MITgcm.MITgcmScratchSpaces
    import MITgcm.MITgcm_path
    import Dataverse

    """
        MITgcm_download()
    
    Download default, compact version of MITgcm from zenodo.
    """
    function MITgcm_download()
        url = "https://zenodo.org/record/5750290/files/MITgcm_test.tar.gz"
        fil="MITgcm_test.tar.gz"
        dir_out=joinpath(MITgcmScratchSpaces.path,"MITgcm_test")
        if !isdir(dir_out)
            MITgcmScratchSpaces.download_dataset(url,MITgcmScratchSpaces.path)
            tmp_path=Dataverse.untargz(joinpath(MITgcmScratchSpaces.path,fil))
            mv(joinpath(tmp_path,fil[1:end-7]),dir_out)
            rm(joinpath(MITgcmScratchSpaces.path,fil))
        end
        MITgcm_path[1]=joinpath(MITgcmScratchSpaces.path,"MITgcm_test")
    end

    function HS94_pickup_download()
        url = "https://zenodo.org/record/5422009/files/pickup_hs94.cs-32x32x5.tar.gz"
        fil="pickup_hs94.cs-32x32x5.tar.gz"
        dir_out=joinpath(MITgcmScratchSpaces.path,"pickup_hs94.cs-32x32x5")
        if !isdir(dir_out)
            MITgcmScratchSpaces.download_dataset(url,MITgcmScratchSpaces.path)
            tmp_path=Dataverse.untargz(joinpath(MITgcmScratchSpaces.path,fil))
            mv(tmp_path,dir_out)
            rm(joinpath(MITgcmScratchSpaces.path,fil))
        end
    end

end

MITgcm_download=downloads.MITgcm_download
HS94_pickup_download=downloads.HS94_pickup_download

#more:
#
#using Plots; include("recipes_plots.jl"); export qwckplot

#deprecated:
#
#include("deprecated.jl"); export prep_MTRX, read_SPM

end # module
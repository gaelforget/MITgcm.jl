module MITgcmTools

using Dates, Printf, SparseArrays, UUIDs, Suppressor
using OrderedCollections, DataFrames, NetCDF, MeshArrays, ClimateModels

include("Types.jl")
include("ReadFiles.jl")
include("ModelSteps.jl")
include("FormatConversions.jl")
include("PhysicalOceanography.jl")
include("verification_experiments.jl")

export MITgcm_path, MITgcm_download, HS94_pickup_download
export MITgcm_config, MITgcm_namelist, MITgcm_launch
export testreport, build, compile, setup, clean
#export pause, stop, clock, monitor, train, help
export verification_experiments, read_namelist, write_namelist
export read_mdsio, read_meta, read_available_diagnostics
export scan_rundir, scan_stdout
export read_bin, read_flt, read_mnc, read_nctiles, findtiles
export GridLoad_mnc, GridLoad_mdsio
export cube2compact, compact2cube, convert2array, convert2gcmfaces
export SeaWaterDensity, MixedLayerDepth

"""
    MITgcm_path

Path to a MITgcm folder. `MITgcm_path[1]` should generally be used. `MITgcm_path[2]` is mostly 
meant to facilitate comparisons between e.g. MITgcm releases when needed.
"""
MITgcm_path = [ "" , ""]

module downloads
    import MITgcmTools.MITgcmScratchSpaces
    import MITgcmTools.MITgcm_path
    using Tar, CodecZlib

    function MITgcm_download()
        url = "https://zenodo.org/record/5750290/files/MITgcm_test.tar.gz"
        fil="MITgcm_test.tar.gz"
        dir_out=joinpath(MITgcmScratchSpaces.path,"MITgcm_test")
        if !isdir(dir_out)
            MITgcmScratchSpaces.download_dataset(url,MITgcmScratchSpaces.path)
            tmp_path=open(joinpath(MITgcmScratchSpaces.path,fil)) do io
                Tar.extract(CodecZlib.GzipDecompressorStream(io))
            end
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
            tmp_path=open(joinpath(MITgcmScratchSpaces.path,fil)) do io
                Tar.extract(CodecZlib.GzipDecompressorStream(io))
            end
            mv(joinpath(tmp_path,fil[1:end-7]),dir_out)
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

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
include("System.jl")
include("Downloads.jl")

getdata=datadeps.getdata

export MITgcm_path, MITgcmScratchSpaces
export MITgcm_download, HS94_pickup_download
export MITgcm_config, MITgcm_namelist, MITgcm_launch
export testreport, build, compile, setup, clean, launch
export monitor #pause, stop, clock, train, help
export verification_experiments, setup_verification!, testreport
export setup_ECCO4!, ECCO4_inputs, ECCO4_testreport
export setup_darwin3!
export read_namelist, write_namelist, read_toml
export read_all_namelists, write_all_namelists, parse_param
export read_mdsio, read_meta, read_available_diagnostics
export scan_run_dir, scan_stdout, scan_build_dir, scan_verification
export create_script, default_path, set_environment_variables_to_default
export system_check, MITgcm_system_check, test_run 
export read_bin, read_flt, read_mnc, read_nctiles, findtiles
export GridLoad_mnc, GridLoad_mdsio, GridLoad_native
export cube2compact, compact2cube, convert2array, convert2gcmfaces
export SeaWaterDensity, MixedLayerDepth

"""
    MITgcm_path

Path to a MITgcm folder. 

- `MITgcm_path[1]` is the MITgcm root dir.
- `MITgcm_path[2]` is the MITgcm/verification dir.
"""
MITgcm_path = [ "" , ""]

MITgcm_download(;do_warn=false)=begin
    MITgcm.getdata("mitgcmsmall")
    MITgcm.getdata("mitgcmsmallverif")
end
HS94_pickup_download()=MITgcm.getdata("hs94pickup")
Darwin3_1D_configs_download()=begin
    MITgcm.getdata("darwin3code")
    MITgcm.getdata("darwin3oneD")
end

"""
    MITgcm.default_path()

Return default path, and download via MITgcm_download if needed.
"""
default_path()=MITgcm_path[1]

__init__() = begin
    datadeps.__init__source_code()
    MITgcm_path[1]=joinpath(MITgcm.getdata("mitgcmsmall"),"MITgcm")
    MITgcm_path[2]=joinpath(MITgcm.getdata("mitgcmsmallverif"),"MITgcm","verification")
end

end # module

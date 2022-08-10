
using Markdown
using InteractiveUtils
using MITgcmTools, ClimateModels, PlutoUI, Printf, Plots
using UUIDs
using NCDatasets
using Pkg

# # setting up python...
# ENV["PYTHON"] = "/opt/anaconda3/envs/eaps-env/bin/python"
# Pkg.build("PyCall")
# using PyCall
# @pyimport xarray

##################
# helpers
##################
# change a parameter
function update_param(file_name, group_name, param_name, new_param_value)
    # read the contents of the data file into a namelist 
    data_file = file_name
    fil = joinpath(rundir, data_file)
    nml = read(fil, MITgcm_namelist())

    # which param group do you want to modify?
    nmlgroup = group_name
    group_idx =findall(nml.groups.==Symbol(nmlgroup))[1]
    parms = nml.params[group_idx]

    # what parameter do you want to modify?
    p_name = param_name
    p_value = new_param_value

    # write changed parameter
    # tmptype= haskey(nml.params[group_idx], Symbol(p_name)) ? typeof(nml.params[group_idx][Symbol(p_name)]) : typeof(p_value)
    #nml.params[group_idx][Symbol(p_name)]=parse(tmptype,p_value)
    nml.params[group_idx][Symbol(p_name)] = p_value
    tmpfil=joinpath(rundir,data_file)
    rm(tmpfil)
    write(tmpfil,nml)
    tmpfil=joinpath("tracked_parameters",data_file)
    ClimateModels.git_log_fil(config_obj,tmpfil,"updated $(p_name) parameter file in $(data_file) to $(p_value)")
end
##################
# END helpers
##################


##################
# TODO: copy and paste in the correct config_id
# (from the output of darwin-setup)
##################
MITgcm_path[1] = "/Users/birdy/Documents/eaps_research/darwin3" # CHANGE ME 
config_id = "ad870f4d-600f-4e64-a77d-93c50214db78" # CHANGE ME

# reload the config 
config_name = "darwin-single-box"
folder = joinpath(MITgcm_path[1], "verification/darwin-single-box/run")
config_obj = MITgcm_config(configuration=config_name, ID=UUID(config_id), folder=folder)
rundir = joinpath(folder, config_id, "run")

##################
# Modify runtime parameters here
# file > group > parameter
##################

# timing 
update_param("data", "PARM03", "nenditer", 2880) # end after 1 years

# NOTE: values taken from large Darwin model run (lat=20.5, lon=202.5)
# load up seed nc file 
seed_file = "/Users/birdy/Documents/eaps_research/gcm_analysis/gcm_data/jan_7_2022/3d.0000000000.nc"
ds = Dataset(seed_file)

# selection using indices
x = 203
y = 121
z = 1
#t = 21 # APRIL 
t = 50 # OCTOBER

# Nutrients *10 
for i = 1:20
    tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
    tracer_name = "TRAC"*tracer_id 
    new_value = ds[tracer_name][x, y, z, t]*10
    update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,$i)", new_value)
end

# PRO and SYN
for i = 21:22
    tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
    tracer_name = "TRAC"*tracer_id 
    new_value = ds[tracer_name][x, y, z, t]
    update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,$i)", new_value)
end

##################
# run model
##################
println("launching...")
t = @elapsed begin
    MITgcm_launch(config_obj)
end
println("run completed")
println("time elapse: ", t, " seconds")
println()
println("Output in directory $rundir, most recent ecco folder ")
# TODO: print out the subfolder (i.e. "ecco_gud_DATE_0001")


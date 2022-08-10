
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

# let
#     import Pkg
#     Pkg.activate(".")
# 	# CHANGE ME - put correct path in 
#     Pkg.add(path="/Users/birdy/Documents/eaps_research/julia stuff/MITgcmTools.jl")
# end

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
config_id = "conservation-test-3" # CHANGE ME

# reload the config 
config_name = "darwin-single-box"
folder = joinpath(MITgcm_path[1], "verification/darwin-single-box/run")
config_obj = MITgcm_config(configuration=config_name, ID=config_id, folder=folder)
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

# # NO BIOLOGY
# # we want the first 19 tracers

# # DIC
# tracer_name = "TRAC01"
# new_value = ds[tracer_name][x, y, z, t]
# update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,1)", new_value)

# # NO3
# tracer_name = "TRAC02"
# new_value = ds[tracer_name][x, y, z, t]
# update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,2)", new_value)

for i = 1:3
    tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
    tracer_name = "TRAC"*tracer_id 
    new_value = ds[tracer_name][x, y, z, t]
    update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,$i)", new_value)
end
# # set biology to 0
# for i = 21:70
#     tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
#     tracer_name = "TRAC"*tracer_id 
#     new_value = 0
#     update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,$i)", new_value)
# end
# WITH PRO and SYN
# we want the first 21 tracers
# for i = 1:20
#     tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
#     tracer_name = "TRAC"*tracer_id 
#     new_value = ds[tracer_name][x, y, z, t]*10
#     update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,$i)", new_value)
# end

# for i = 21:22
#     tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
#     tracer_name = "TRAC"*tracer_id 
#     new_value = ds[tracer_name][x, y, z, t]
#     update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,$i)", new_value)
# end

# # with pro's predator 
# pro_pred = ds["TRAC53"][x, y, z, t]
# update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,53)", pro_pred/10)

# # with syns's predator 
# pro_pred = ds["TRAC54"][x, y, z, t]
# update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,54)", pro_pred/10)

# # with hetero bact
# het = ds["TRAC69"][x, y, z, t]
# update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,69)", het)

# # WITH EVERYTHING except diazotrophs 
# for i = 1:29
#     tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
#     tracer_name = "TRAC"*tracer_id 
#     new_value = ds[tracer_name][x, y, z, t]
#     update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,$i)", new_value)
# end
# for i = 35:70
#     tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
#     tracer_name = "TRAC"*tracer_id 
#     new_value = ds[tracer_name][x, y, z, t]
#     update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,$i)", new_value)
# end

# temperature = 22.65576 # TODO: not actually taken from darwin data
# salinity = 33.23 # TODO: not actually taken from darwin data
# update_param("data", "PARM01", "tRef", temperature)
# update_param("data", "PARM01", "sRef", salinity)

# TODO: save file to output dir with info about runtime params


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


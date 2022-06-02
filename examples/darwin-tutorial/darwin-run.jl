
using Markdown
using InteractiveUtils
using MITgcmTools, ClimateModels, PlutoUI, Printf, Plots
using UUIDs


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
config_id = "b622268f-ecfb-4626-a456-505e86fb1e5a" # CHANGE ME

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
update_param("data", "PARM03", "nenditer", 2880) # end after 1 year

update_param("data", "PARM01", "tRef", 30.0)
update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,21)", 1e-3)

# # enter the name of the data file you want to modify 
# data_file = "data" # CHANGE ME

# # read the contents of the data file into a namelist 
# fil = joinpath(rundir, data_file)
# nml = read(fil, MITgcm_namelist())
# println("Reading from file ", fil)
# println("groups: ", nml.groups)

# # which param group do you want to modify?
# nmlgroup = "PARM01" # CHANGE ME
# group_idx =findall(nml.groups.==Symbol(nmlgroup))[1]
# parms = nml.params[group_idx]
# println("parms: ", parms)

# # what parameter do you want to modify?
# p_name = "tRef" # CHANGE ME
# p_value = "30.0" # CHANGE ME

# # write changed parameter
# tmptype=typeof(nml.params[group_idx][Symbol(p_name)])
# nml.params[group_idx][Symbol(p_name)]=parse(tmptype,p_value)
# tmpfil=joinpath(rundir,data_file)
# rm(tmpfil)
# write(tmpfil,nml)
# tmpfil=joinpath("tracked_parameters",data_file)
# ClimateModels.git_log_fil(config_obj,tmpfil,"updated $(p_name) parameter file in $(data_file) to $(p_value)")


# ##################
# # add pro
# ##################
# # ### goal: turn on pro
# # enter the name of the data file you want to modify 
# data_file = "data.ptracers" # CHANGE ME

# # read the contents of the data file into a namelist 
# fil = joinpath(rundir, data_file)
# nml = read(fil, MITgcm_namelist())
# println("Reading from file ", fil)
# println("groups: ", nml.groups)

# # which param group do you want to modify?
# nmlgroup = "PTRACERS_PARM01" # CHANGE ME
# group_idx =findall(nml.groups.==Symbol(nmlgroup))[1]
# parms = nml.params[group_idx]
# println("parms: ", parms)

# # what parameter do you want to modify?
# p_name = "PTRACERS_ref( :,21)" # CHANGE ME
# p_value = "1E-3" # CHANGE ME

# # write (or add) changed parameter
# #tmptype=typeof(nml.params[group_idx][Symbol(p_name)])
# println(tmptype)
# tmptype=typeof(0.5)
# nml.params[group_idx][Symbol(p_name)]=parse(tmptype,p_value)
# tmpfil=joinpath(rundir,data_file)
# rm(tmpfil)
# write(tmpfil,nml)
# tmpfil=joinpath("tracked_parameters",data_file)
# ClimateModels.git_log_fil(config_obj,tmpfil,"updated $(p_name) parameter file in $(data_file) to $(p_value)")


##################
# run model
##################
println("launching...")
t = @elapsed begin
    MITgcm_launch(config_obj)
end
println("run completed")
println("time elapse: ", t, " seconds")

# TODO: print out the subfolder (i.e. "ecco_gud_DATE_0001")


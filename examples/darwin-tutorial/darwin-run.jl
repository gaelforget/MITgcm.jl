
using Markdown
using InteractiveUtils
using MITgcmTools, ClimateModels, PlutoUI, Printf, Plots
using UUIDs

##################
# TODO: copy and paste in the correct config_id
# (from the output of darwin-setup)
##################
config_id = "873f1019-bdc3-4aba-8c78-2b25a035498a" # CHANGE ME

# reload the config 
config_name = "darwin-single-box"
folder = "/Users/birdy/Documents/eaps_research/darwin3/verification/darwin-single-box/run"
config_obj = MITgcm_config(configuration=config_name, ID=UUID(config_id), folder=folder)
rundir = joinpath(folder, config_id, "run")

##################
# Modify runtime parameters here
# file > group > parameter
##################

# enter the name of the data file you want to modify 
data_file = "data" # CHANGE ME

# read the contents of the data file into a namelist 
fil = joinpath(rundir, data_file)
nml = read(fil, MITgcm_namelist())
println("Reading from file ", fil)
println("groups: ", nml.groups)

# which param group do you want to modify?
nmlgroup = "PARM01" # CHANGE ME
group_idx =findall(nml.groups.==Symbol(nmlgroup))[1]
parms = nml.params[group_idx]
println("parms: ", parms)

# what parameter do you want to modify?
p_name = "tRef" # CHANGE ME
p_value = "30.0" # CHANGE ME

# write changed parameter
tmptype=typeof(nml.params[group_idx][Symbol(p_name)])
nml.params[group_idx][Symbol(p_name)]=parse(tmptype,p_value)
tmpfil=joinpath(rundir,data_file)
rm(tmpfil)
write(tmpfil,nml)
tmpfil=joinpath("tracked_parameters",data_file)
ClimateModels.git_log_fil(config_obj,tmpfil,"updated $(p_name) parameter file in $(data_file) to $(p_value)")


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

using Markdown
using InteractiveUtils
using MITgcmTools, ClimateModels, PlutoUI, Printf, Plots
using UUIDs

##################
# TODO: copy and paste in the correct config_id
# (from the output of darwin-setup)
##################
config_id = "1e867561-76e2-4d16-87e5-8d76f13eca15" # CHANGE ME

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
p_value = "22.2" # CHANGE ME

# write changed parameter
tmptype=typeof(nml.params[group_idx][Symbol(p_name)])
println(tmptype)
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

##################
# plot results
##################

# plot the results
# search output.txt for 'dynstat_theta_mean'
filout = joinpath(rundir, "output.txt")
filstat = joinpath(rundir, "onestat.txt")
run(pipeline(`grep dynstat_theta_mean $(filout)`,filstat))
tmp0 = read(filstat,String)
tmp0 = split(tmp0,"\n")
Tmean=[parse(Float64,split(tmp0[i],"=")[2]) for i in 1:length(tmp0)-1]
my_plot = plot(Tmean,label="mean temperature")
display(my_plot)
savefig(my_plot, "output-temp.png")

# TODO: plot NC files! 

# read in .nc files, xarray style? 

# nutrients 

# pro 

# sunlight

# temp





# pro concentration
# ds = Dataset(joinpath(rundir, "ecco_gud_20220524_0001", "3d.0000002880.t001.nc")) 
# plot()
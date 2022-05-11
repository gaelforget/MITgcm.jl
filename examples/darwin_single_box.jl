# Recreation of MITgcm_worklow.jl

using Markdown
using InteractiveUtils
using MITgcmTools, ClimateModels, PlutoUI, Printf
using UUIDs
# select configuration
# exps=verification_experiments()
# myexp = "advect_xy"
# iexp=findall([exps[i].configuration==myexp for i in 1:length(exps)])[1]
# println("config at index ", iexp)
# println(exps[iexp])

# # compile 
# filexe=joinpath(MITgcm_path[1],"verification",exps[iexp].configuration,"build","mitgcmuv")
# # does filexe already exist? 
# println("looking for filexe: ", filexe)
# println("does filexe already exist? ", isfile(filexe))

# # TODO: right now i'm pointing it to the mitgcmuv i created using the CL
# !isfile(filexe) ? build(exps[iexp]) : nothing

# create config
config_name = "darwin-single-box"
config_id = UUIDs.uuid4()
folder = "/Users/birdy/Documents/eaps_research/darwin3/verification/darwin-single-box/run"
config_obj = MITgcm_config(configuration=config_name, ID=config_id, folder=folder)


# # setup 
# rundir=joinpath(exps[iexp].folder,string(exps[iexp].ID),"run")
# filout=joinpath(rundir,"output.txt")
# filstat=joinpath(rundir,"onestat.txt")
# setup(exps[iexp])
# ClimateModels.git_log_prm(exps[iexp])
# filexe
filexe=joinpath(MITgcm_path[1],"verification",config_name,"build","mitgcmuv")
rundir=joinpath("/Users/birdy/Documents/eaps_research/darwin3","verification",config_name,"run")
filout=joinpath(rundir,"output.txt")
filstat=joinpath(rundir,"onestat.txt")
print("MITgcm_path[1]: ", MITgcm_path[1])
setup(config_obj)
print("done with setup")
# ClimateModels.git_log_prm(config_name)
println("filexe=", filexe)

println("Where Is `mitgcmuv` run? ", rundir)

# # run 
# println("is channel empty: ", isempty(exps[iexp].channel))
# # we do this in setup
# #isempty(exps[iexp].channel) ? put!(exps[iexp].channel,MITgcm_launch) : nothing
# launch(exps[iexp])
# refresh_plot=true
# println("""Model run for the **$(exps[iexp].configuration)** configuration has completed!

# 🏇 🏁 🏁 🏁 🎉 🎊 """)
println("MITgcm config: ", config_obj)
println("launching...")
MITgcm_launch(config_obj)
refresh_plot=true
println("Model run for the **$(config_name)** configuration has completed!")



# explore model output 
# plots test
using Plots
pyplot()             # or plotlyjs()
display(plot(sin, -2pi, pi, label="sine function"))

pyplot()
x = 1:10; y = rand(10); # These are the plotting data
display(plot(x, y))
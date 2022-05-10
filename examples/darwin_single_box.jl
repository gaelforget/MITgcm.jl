# Recreation of MITgcm_worklow.jl

using Markdown
using InteractiveUtils
using MITgcmTools, ClimateModels, PlutoUI, Printf
	
# select configuration
exps=verification_experiments()
myexp = "advect_xy"
iexp=findall([exps[i].configuration==myexp for i in 1:length(exps)])[1]
println("config at index ", iexp)
println(exps[iexp])

# compile 
filexe=joinpath(MITgcm_path[1],"verification",exps[iexp].configuration,"build","mitgcmuv")
# does filexe already exist? 
println("looking for filexe: ", filexe)
println("does filexe already exist? ", isfile(filexe))

# TODO: right now i'm pointing it to the mitgcmuv i created using the CL 
!isfile(filexe) ? build(exps[iexp]) : nothing


# setup 
rundir=joinpath(exps[iexp].folder,string(exps[iexp].ID),"run")
filout=joinpath(rundir,"output.txt")
filstat=joinpath(rundir,"onestat.txt")
setup(exps[iexp])
ClimateModels.git_log_prm(exps[iexp])
filexe

println("Where Is `mitgcmuv` run? ", rundir)

# run 
println("is channel empty: ", isempty(exps[iexp].channel))
# we do this in setup
#isempty(exps[iexp].channel) ? put!(exps[iexp].channel,MITgcm_launch) : nothing
launch(exps[iexp])
refresh_plot=true
println("""Model run for the **$(exps[iexp].configuration)** configuration has completed!

🏇 🏁 🏁 🏁 🎉 🎊 """)

# explore model output 
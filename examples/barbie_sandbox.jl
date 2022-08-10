# Recreation of MITgcm_worklow.jl

using Markdown
using InteractiveUtils
using MITgcmTools, ClimateModels, PlutoUI, Printf, Plots

# plots test
pyplot()             # or plotlyjs()
display(plot(sin, -2pi, pi, label="sine function"))

pyplot()
x = 1:10; y = rand(10); # These are the plotting data
display(plot(x, y))

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
t_slow = 10
begin
	refresh_plot
	
	if exps[iexp].configuration=="advect_xy"||exps[iexp].configuration=="advect_xz"
		tmp2=readdir(rundir)
		
		x=read_mdsio(rundir,"XC")
		y=read_mdsio(rundir,"YC")
		z=read_mdsio(rundir,"RC")

		tmp2=tmp2[findall(occursin.("T.0000",tmp2))]
		tmp2=tmp2[findall(occursin.("001.001.data",tmp2))]
		tmp2=[i[1:end-13] for i in tmp2]
	
		!isnothing(t_slow) ? i=mod(t_slow,length(tmp2)) : i=length(tmp2)-1
		
		tmp3=read_mdsio(rundir,tmp2[i+1])
		length(size(tmp3))==3 ? tmp3=dropdims(tmp3;dims=2) : nothing

		if exps[iexp].configuration=="advect_xz"
			display(contourf(1e-3*x[:,1],reverse(z[:]),reverse(tmp3,dims=2),xlabel="km",ylabel="m",
				levels=(-0.04:0.02:0.2),leg=:none,c = :terrain))
		else
			display(contourf(1e-3*x[:,1],1e-3*y[1,:],tmp3,xlabel="km",ylabel="km",
				levels=(-0.02:0.01:0.1),leg=:none,c = :terrain))
		end
	else
		plot(Tmean,label="mean temperature")
	end
end

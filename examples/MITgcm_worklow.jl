### A Pluto.jl notebook ###
# v0.14.1

using Markdown
using InteractiveUtils

# This Pluto notebook uses @bind for interactivity. When running this notebook outside of Pluto, the following 'mock version' of @bind gives bound variables a default value (instead of an error).
macro bind(def, element)
    quote
        local el = $(esc(element))
        global $(esc(def)) = Core.applicable(Base.get, el) ? Base.get(el) : missing
        el
    end
end

# ╔═╡ 8cf4d8ca-84eb-11eb-22d2-255ce7237090
begin
	using MITgcmTools, ClimateModels, PlutoUI, Printf, GR
	exps=verification_experiments()	
	🏁 = "🏁"
end

# ╔═╡ f588eaba-84ef-11eb-0755-bf1b85b2b561
begin
	md"""# MITgcm_workflow.jl

	### 

	Here we setup, run and plot [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest) interactively via [MITgcmTools.jl](https://gaelforget.github.io/MITgcmTools.jl/dev/) to generate something like this:
	
	![plot](https://user-images.githubusercontent.com/20276764/111042787-12377e00-840d-11eb-8ddb-64cc1cfd57fd.png)
	"""
end

# ╔═╡ 98b6621c-85ab-11eb-29d1-af0433598c6a
md"""## Select Model Configuration

_Note: changing this top level parameter should update multiple-choice menus and results below_
"""

# ╔═╡ a28f7354-84eb-11eb-1830-1f401bf2db97
@bind myexp Select([exps[i].configuration for i in 1:length(exps)],default="global_with_exf")

# ╔═╡ 766f87b8-f3d4-4e39-8cae-d91679d9af6f
begin
	iexp=findall([exps[i].configuration==myexp for i in 1:length(exps)])[1]
	exps[iexp]
end

# ╔═╡ ee0ed0a0-8817-11eb-124d-a197f1d4545a
md"""### Where Is `mitgcmuv` located?

###

The model executable `mitcmuv` is generally found in the `$(MITgcm_path)/verification/$(configuration)/build/` subfolder of the selected model configuration.



If `mitcmuv` is not found at this stage then it is assumed that the chosen model configuration has never been compiled -- such that we need to compile and run the model a first time. This might take a lot longer than a normal model run due to the one-time cost of compiling the model.

Once `mitgcmuv` is found, then the executable file name should appear just below.
"""

# ╔═╡ eca925ba-8816-11eb-1d6d-39bf08bfe979
begin
	filexe=joinpath(MITgcm_path,"verification",exps[iexp].configuration,"build","mitgcmuv")
	!isfile(filexe) ? build(exps[iexp]) : nothing
	rundir=joinpath(exps[iexp].folder,string(exps[iexp].ID),"run")
	filout=joinpath(rundir,"output.txt")
	filstat=joinpath(rundir,"onestat.txt")
	setup(exps[iexp])
	filexe
end

# ╔═╡ f051e094-85ab-11eb-22d4-5bd61ac572a1
md"""## Browse Model Parameters

Once the model has been setup for the selected configuration, then `data` and `PARM01` should be found in the model run directory which should appear just below. 

Once model has been compiled and the run directory setup, we are ready to call `launch(exps[iexp])` and thus run the model as done below. If an error message suggests that something has gone wrong, sometimes it helps to run the `clean(exps[iexp])` and restart this notebook.
"""

# ╔═╡ f7e66980-9ec5-43cf-98b1-37aa6823d64a
rundir

# ╔═╡ d7f2c656-8512-11eb-2fdf-47a3e57a55e6
begin
	function list_namelist_files(pth)
		tmpA=readdir(pth)
		tmpA=tmpA[findall([length(tmpA[i])>3 for i in 1:length(tmpA)])]
		tmpA=tmpA[findall([tmpA[i][1:4]=="data" for i in 1:length(tmpA)])]
	end
	dats=list_namelist_files(rundir)
	try
		@bind mydats Select([dats[i] for i in 1:length(dats)])
	catch e
		"Error: could not find any namelist in $(rundir)"
	end
end

# ╔═╡ c7670d00-868c-11eb-1889-4d3ffe621dd2
md"""## Modify Parameters

###

In the example just below, we change the run duration for the **$(exps[iexp].configuration)** configuration. 

###

Selecting **Update Parameter File** triggers the following sequence:

- update file in the run directory
- launch the model
- update the plot accordingly 

###

_Note: some configurations use `nTimeSteps`, others use `endTime`, but using both at once generates an error message._

"""

# ╔═╡ dff9a4c8-880c-11eb-37e1-439de05c5166
@bind update_file Select(["allset" => "Use Previous Parameters", "update" => "Update Parameter File"])

# ╔═╡ 15746ef0-8617-11eb-1160-5f48a95d94d0
begin
	update_file
	
	tmpfil=joinpath(rundir,"data")
	tmplist=read(tmpfil,MITgcm_namelist())
	i1=findall((tmplist.groups.==:PARM03))[1]
	
	if haskey(tmplist.params[i1],:nTimeSteps)
		tmplist.params[i1][:nTimeSteps]+=20
	elseif haskey(tmplist.params[i1],:deltaT)
		tmplist.params[i1][:endTime]+=tmplist.params[i1][:deltaT]
	elseif haskey(tmplist.params[i1],:deltaTtracer)
		tmplist.params[i1][:endTime]+=tmplist.params[i1][:deltaTtracer]
	elseif haskey(tmplist.params[i1],:deltaTClock)
		tmplist.params[i1][:endTime]+=tmplist.params[i1][:deltaTClock]
	end
	
	if update_file!=="allset"
		rm(tmpfil)
		write(tmpfil,tmplist)
	end

	do_run1="🐎"
end

# ╔═╡ 4b62b282-86bd-11eb-2fed-bbbe8ef2d4af
md"""## Run Model

###

Click on button when ready to run the model **$(exps[iexp].configuration)**. 

This should also happen automatically after modifying parameters.
"""

# ╔═╡ 6f618b2c-86bd-11eb-1607-a179a349378e
@bind do_run2 Button("Launch Model")

# ╔═╡ 96492c18-86bd-11eb-35ca-dff79e6e7818
begin
	do_run1
	do_run2
	isempty(exps[iexp].channel) ? put!(exps[iexp].channel,MITgcm_launch) : nothing
	launch(exps[iexp])
	refresh_plot=true
	🏁
end

# ╔═╡ 0f920f90-86e9-11eb-3f6d-2d530bd2e9db
md"""## Plot Model Result

Here we show average temperature in **$(exps[iexp].configuration)**
"""

# ╔═╡ d0bbb668-86e0-11eb-1a9b-8f2b0175f7c1
begin
	refresh_plot
	run(pipeline(`grep dynstat_theta_mean $(filout)`,filstat))
	
	tmp0 = read(filstat,String)
	tmp0 = split(tmp0,"\n")
	Tmean=[parse(Float64,split(tmp0[i],"=")[2]) for i in 1:length(tmp0)-1]
	plot(Tmean)	
end

# ╔═╡ af176e6c-8695-11eb-3e34-91fbdb9c52fa
md"""### Appendices"""

# ╔═╡ 348c692e-84fe-11eb-3288-dd0a1dedce90
begin	
	do_run1
	fil=joinpath(rundir,mydats)
	nml=read(fil,MITgcm_namelist())
	🏁
end

# ╔═╡ ca7bb004-8510-11eb-379f-632c3b40723d
try
	@bind nmlgroup Select(String.(nml.groups))
catch e
	"Error: could not find any namelist in $(rundir)"
end

# ╔═╡ 1c4fc1a0-4061-46f5-8f3d-b88fe5e6dc3e
fil

# ╔═╡ 52d7c7a2-8693-11eb-016f-4fc3eb516d44
begin
        inml=findall(nml.groups.==Symbol(nmlgroup))[1]
        🏁
end

# ╔═╡ d87b220a-2c4f-4943-8e79-fd42ebec81b9
nml.params[inml]

# ╔═╡ 734e2b5a-8866-11eb-0025-bd9544f4c30d
begin
	#Read grid (as if rectangular domain for initial test) 
	
	try
		XC=read_mdsio(rundir,"XC"); siz=size(XC)

		mread(xx::Array,x::MeshArray) = read(xx,x)	
		function mread(fil::String,x::MeshArray)
			d=dirname(fil)
			b=basename(fil)[1:end-5]
			read(read_mdsio(d,b),x)
		end

		γ=gcmgrid(rundir,"PeriodicChannel",1,fill(siz,1), [siz[1] siz[2]], eltype(XC), mread, write)
		Γ=GridLoad(γ)
	catch e
		γ=[]
		Γ=[]
		println("no grid files")
	end
	
	🏁
end

# ╔═╡ 901d2844-83be-4767-b169-dfb7701ce15e
begin
	md"""### 
	
	![plot](https://user-images.githubusercontent.com/20276764/97648227-970b9780-1a2a-11eb-81c4-65ec2c87efc6.png)

	"""
end

# ╔═╡ Cell order:
# ╟─f588eaba-84ef-11eb-0755-bf1b85b2b561
# ╟─98b6621c-85ab-11eb-29d1-af0433598c6a
# ╟─a28f7354-84eb-11eb-1830-1f401bf2db97
# ╟─766f87b8-f3d4-4e39-8cae-d91679d9af6f
# ╟─ee0ed0a0-8817-11eb-124d-a197f1d4545a
# ╟─eca925ba-8816-11eb-1d6d-39bf08bfe979
# ╟─f051e094-85ab-11eb-22d4-5bd61ac572a1
# ╟─f7e66980-9ec5-43cf-98b1-37aa6823d64a
# ╟─d7f2c656-8512-11eb-2fdf-47a3e57a55e6
# ╟─ca7bb004-8510-11eb-379f-632c3b40723d
# ╟─d87b220a-2c4f-4943-8e79-fd42ebec81b9
# ╟─c7670d00-868c-11eb-1889-4d3ffe621dd2
# ╟─dff9a4c8-880c-11eb-37e1-439de05c5166
# ╟─15746ef0-8617-11eb-1160-5f48a95d94d0
# ╟─1c4fc1a0-4061-46f5-8f3d-b88fe5e6dc3e
# ╟─4b62b282-86bd-11eb-2fed-bbbe8ef2d4af
# ╟─6f618b2c-86bd-11eb-1607-a179a349378e
# ╟─96492c18-86bd-11eb-35ca-dff79e6e7818
# ╟─0f920f90-86e9-11eb-3f6d-2d530bd2e9db
# ╟─d0bbb668-86e0-11eb-1a9b-8f2b0175f7c1
# ╟─af176e6c-8695-11eb-3e34-91fbdb9c52fa
# ╟─8cf4d8ca-84eb-11eb-22d2-255ce7237090
# ╟─348c692e-84fe-11eb-3288-dd0a1dedce90
# ╟─52d7c7a2-8693-11eb-016f-4fc3eb516d44
# ╟─734e2b5a-8866-11eb-0025-bd9544f4c30d
# ╟─901d2844-83be-4767-b169-dfb7701ce15e

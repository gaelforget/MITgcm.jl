### A Pluto.jl notebook ###
# v0.12.21

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
	
	tst=fill(false,length(exps))
	for i in 1:length(exps)
		pth0=joinpath(MITgcm_path,"verification",exps[i].configuration,"run")
		tst[i]=!isempty(findall(occursin.("XC",readdir(pth0))))
	end
	#exps=exps[findall(tst)]
	
	🏁 = "🏁"
end

# ╔═╡ f588eaba-84ef-11eb-0755-bf1b85b2b561
begin
	imgA="https://user-images.githubusercontent.com/20276764/111042787-12377e00-840d-11eb-8ddb-64cc1cfd57fd.png"
	imgB="https://user-images.githubusercontent.com/20276764/97648227-970b9780-1a2a-11eb-81c4-65ec2c87efc6.png"
	md"""# MITgcm_workflow.jl

	### 


	Here we setup, run and plot MITgcm interactively via **`MITgcmTools.jl`** to generate something like this:
	
	$(Resource(imgA, :width => 240))
	
	### 
	
	$(Resource(imgB, :width => 120))
	"""
end

# ╔═╡ 98b6621c-85ab-11eb-29d1-af0433598c6a
md"""## Select Model Configuration

_Note: changing this top level parameter should update multiple-choice menus and results below_
"""

# ╔═╡ a28f7354-84eb-11eb-1830-1f401bf2db97
@bind myexp Select([exps[i].configuration for i in 1:length(exps)],default="advect_xy")

# ╔═╡ 2ff78cac-868b-11eb-2d56-79ea1f874453
begin
	iexp=findall([exps[i].configuration==myexp for i in 1:length(exps)])[1]
	
	md"""###
	
	name = $(exps[iexp].configuration)
	
	build options = $([exps[iexp].options[i]*", " for i in 1:length(exps[iexp].options)])
	
	run-time options = $(exps[iexp].inputs)
	"""
end

# ╔═╡ ee0ed0a0-8817-11eb-124d-a197f1d4545a
md"""### Where Is `mitgcmuv` located?

The model executable `mitcmuv` is normally found in the `build/` subfolder of the selected experiment.

If `mitcmuv` is not found at this stage then it is assumed that the chosen model configuration has never been compiled -- such that we need to compile and run the model a first time. This might take a lot longer than a normal model run due to the one-time cost of compiling the model.

Once `mitgcmuv` is found, then a `🏁` should appear just below.
"""

# ╔═╡ eca925ba-8816-11eb-1d6d-39bf08bfe979
begin
	filexe=joinpath(MITgcm_path,"verification",exps[iexp].configuration,"build","mitgcmuv")
	!isfile(filexe) ? testreport(exps[iexp]) : nothing
	filout=joinpath(MITgcm_path,"verification",exps[iexp].configuration,"run","output.txt")
	filstat=joinpath(MITgcm_path,"verification",exps[iexp].configuration,"run","onestat.txt")
	🏁
end

# ╔═╡ f051e094-85ab-11eb-22d4-5bd61ac572a1
md"""## Browse Model Parameters

**Once the model has been run for the selected configuration**, then `data` and `PARM01` should be found in the model run directory. If an error message suggests that something has gone wrong, sometimes it helps to rerun `testreport(exps[iexp])` to clean up, recompile, and rerun the chosen model configuration (as shown above). After restarting this notebook, you should be able to call `run(exps[iexp])` to rerun the already compiled model with modified parameters (as done below).
"""

# ╔═╡ f93bde1a-8811-11eb-35f5-e325bd730161
@bind reload_nml Button("Refresh Parameters")

# ╔═╡ d7f2c656-8512-11eb-2fdf-47a3e57a55e6
begin
	pth=joinpath(MITgcm_path,"verification",exps[iexp].configuration,"run")
	function list_namelist_files(pth)
		tmpA=readdir(pth)
		tmpA=tmpA[findall([length(tmpA[i])>3 for i in 1:length(tmpA)])]
		tmpA=tmpA[findall([tmpA[i][1:4]=="data" for i in 1:length(tmpA)])]
	end
	dats=list_namelist_files(pth)
	try
		@bind mydats Select([dats[i] for i in 1:length(dats)])
	catch e
		"Error: could not find any namelist in $(pth)"
	end
end

# ╔═╡ c7670d00-868c-11eb-1889-4d3ffe621dd2
md"""## Modify Parameter File

In the code cell below, we can change the run duration for the **$(exps[iexp].configuration)** configuration.

_Note: some MITgcm experiments use `nTimeSteps` while others use `endTime`. Trying to use both in the same run generates an error message (conflicting specifications)._
"""

# ╔═╡ dff9a4c8-880c-11eb-37e1-439de05c5166
@bind update_file Select(["allset" => "Use Previous Parameters", "update" => "Update Parameter File"])

# ╔═╡ 4b62b282-86bd-11eb-2fed-bbbe8ef2d4af
md"""## Run Modified Model

Click on button when ready to run the model **$(exps[iexp].configuration)**
"""

# ╔═╡ 6f618b2c-86bd-11eb-1607-a179a349378e
@bind do_run2 Button("Launch Model")

# ╔═╡ 0f920f90-86e9-11eb-3f6d-2d530bd2e9db
md"""## Plot Model Result

Here we show average temperature in **$(exps[iexp].configuration)**
"""

# ╔═╡ af176e6c-8695-11eb-3e34-91fbdb9c52fa
md"""### Appendices"""

# ╔═╡ 348c692e-84fe-11eb-3288-dd0a1dedce90
begin	
	update_file
	reload_nml
	fil=joinpath(MITgcm_path,"verification",exps[iexp].configuration,"run",mydats)
	nml=read(fil,MITgcm_namelist())
	🏁
end

# ╔═╡ ca7bb004-8510-11eb-379f-632c3b40723d
try
	@bind nmlgroup Select(String.(nml.groups))
catch e
	"Error: could not find any namelist in $(pth)"
end

# ╔═╡ be7d5ee2-86cb-11eb-2ef3-bd7757133661
md"""Selected model : **$(exps[iexp].configuration)**; namelist file : **$mydats**; parameter group : **$nmlgroup**
"""

# ╔═╡ 15746ef0-8617-11eb-1160-5f48a95d94d0
begin
	update_file
	
	tmplist=deepcopy(nml)
	i1=findall((nml.groups.==:PARM03))[1]
	
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
		rm(fil)
		write(fil,tmplist)
	end

	do_run1="🐎"
end

# ╔═╡ 96492c18-86bd-11eb-35ca-dff79e6e7818
begin
	do_run1
	do_run2
	launch(exps[iexp])
	refresh_plot=true
	🏁
end

# ╔═╡ d0bbb668-86e0-11eb-1a9b-8f2b0175f7c1
begin
	refresh_plot
	run(pipeline(`grep dynstat_theta_mean $(filout)`,filstat))
	
	tmp0 = read(filstat,String)
	tmp0 = split(tmp0,"\n")
	Tmean=[parse(Float64,split(tmp0[i],"=")[2]) for i in 1:length(tmp0)-1]
	plot(Tmean)	
end

# ╔═╡ 52d7c7a2-8693-11eb-016f-4fc3eb516d44
begin
        inml=findall(nml.groups.==Symbol(nmlgroup))[1]
        🏁
end

# ╔═╡ 385bd57a-8810-11eb-289a-47fcc1ec5d51
nml.params[inml]

# ╔═╡ 734e2b5a-8866-11eb-0025-bd9544f4c30d
begin
	#Read grid (as if rectangular domain for initial test) 
	
	try
		XC=read_mdsio(pth,"XC"); siz=size(XC)

		mread(xx::Array,x::MeshArray) = read(xx,x)	
		function mread(fil::String,x::MeshArray)
			d=dirname(fil)
			b=basename(fil)[1:end-5]
			read(read_mdsio(d,b),x)
		end

		γ=gcmgrid(pth,"PeriodicChannel",1,fill(siz,1), [siz[1] siz[2]], eltype(XC), mread, write)
		Γ=GridLoad(γ)
	catch e
		γ=[]
		Γ=[]
		println("no grid files")
	end
	
	🏁
end

# ╔═╡ Cell order:
# ╟─f588eaba-84ef-11eb-0755-bf1b85b2b561
# ╟─98b6621c-85ab-11eb-29d1-af0433598c6a
# ╟─a28f7354-84eb-11eb-1830-1f401bf2db97
# ╟─2ff78cac-868b-11eb-2d56-79ea1f874453
# ╟─ee0ed0a0-8817-11eb-124d-a197f1d4545a
# ╟─eca925ba-8816-11eb-1d6d-39bf08bfe979
# ╟─f051e094-85ab-11eb-22d4-5bd61ac572a1
# ╟─be7d5ee2-86cb-11eb-2ef3-bd7757133661
# ╟─f93bde1a-8811-11eb-35f5-e325bd730161
# ╟─d7f2c656-8512-11eb-2fdf-47a3e57a55e6
# ╟─ca7bb004-8510-11eb-379f-632c3b40723d
# ╟─385bd57a-8810-11eb-289a-47fcc1ec5d51
# ╟─c7670d00-868c-11eb-1889-4d3ffe621dd2
# ╟─dff9a4c8-880c-11eb-37e1-439de05c5166
# ╟─15746ef0-8617-11eb-1160-5f48a95d94d0
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

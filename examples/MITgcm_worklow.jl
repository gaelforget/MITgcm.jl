### A Pluto.jl notebook ###
# v0.14.8

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
#	import Pkg
#    Pkg.activate(mktempdir())
#    Pkg.add([
#        Pkg.PackageSpec(name="MITgcmTools", version="0.1"),
#        Pkg.PackageSpec(name="ClimateModels", version="0.1"),
#        Pkg.PackageSpec(name="PlutoUI", version="0.7"),
#        Pkg.PackageSpec(name="Plots", version="1.11"),
#    ])
	using MITgcmTools, ClimateModels, PlutoUI, Printf, Plots
	exps=verification_experiments()	
	🏁 = "🏁"
	
	md"""😸"""
end

# ╔═╡ f588eaba-84ef-11eb-0755-bf1b85b2b561
begin
	md"""# Model Interface Demo

	### 

	Here we setup, run and plot the [MIT general circulation model](https://mitgcm.readthedocs.io/en/latest/?badge=latest) interactively via [MITgcmTools.jl](https://gaelforget.github.io/MITgcmTools.jl/dev/), which can generate something like the Atmosphere example shown below. 
	
	The notebook demonstrates the use of the climate model interface (`build`, `setup`, `launch`, etc) defined in [ClimateModels.jl](https://github.com/gaelforget/ClimateModels.jl) and implemented for MITgcm.
	The `log/` subfolder is a byproduct of the climate model interface, which uses `git` to document wokflows as they happen and allow us to reproduce them later. The _Modify Parameters_ section demonstrates the interactive use of this functionality. Adding analysis / processing steps can be done similarly.

	
	![plot](https://user-images.githubusercontent.com/20276764/111042787-12377e00-840d-11eb-8ddb-64cc1cfd57fd.png)
	"""
end

# ╔═╡ 98b6621c-85ab-11eb-29d1-af0433598c6a
md"""## Select Model Configuration

###

_Note:_ 

_changing the top level parameter just below will update multiple-choice menus and results afterwards._ 

_Be aware, however, that selecting a new model configuration typically means recompiling the model._ 

_This can take a lot longer than a normal model run due to the one-time cost of compiling the model (see below for more on this)._
"""

# ╔═╡ a28f7354-84eb-11eb-1830-1f401bf2db97
@bind myexp Select([exps[i].configuration for i in 1:length(exps)],default="advect_xz")

# ╔═╡ 766f87b8-f3d4-4e39-8cae-d91679d9af6f
begin
	iexp=findall([exps[i].configuration==myexp for i in 1:length(exps)])[1]
	exps[iexp]
end

# ╔═╡ ee0ed0a0-8817-11eb-124d-a197f1d4545a
md"""### Where Is `mitgcmuv` compiled?

###

The model executable `mitcmuv` is generally found in the `$(MITgcm_path)/verification/$(configuration)/build/` subfolder of the selected model configuration.



If `mitcmuv` is not found at this stage then it is assumed that the chosen model configuration has never been compiled -- such that we need to do that before we can run the model. It should also be noted that compiling the model can take a lot longer than running it for a few time steps (as done in this notebook's sample model runs).

Once `mitgcmuv` is found, then the executable file name should appear just below.
"""

# ╔═╡ eca925ba-8816-11eb-1d6d-39bf08bfe979
begin
	filexe=joinpath(MITgcm_path[1],"verification",exps[iexp].configuration,"build","mitgcmuv")
	!isfile(filexe) ? build(exps[iexp]) : nothing
	rundir=joinpath(exps[iexp].folder,string(exps[iexp].ID),"run")
	filout=joinpath(rundir,"output.txt")
	filstat=joinpath(rundir,"onestat.txt")
	setup(exps[iexp])
	git_log_prm(exps[iexp])
	filexe
end

# ╔═╡ f051e094-85ab-11eb-22d4-5bd61ac572a1
md"""### Where Is `mitgcmuv` run?

Once the model has been compliled and setup for the selected configuration, the model run directory path should appear just below this comment bloc. At this point, we are ready to call `launch` to run the model. 
"""

# ╔═╡ f7e66980-9ec5-43cf-98b1-37aa6823d64a
rundir

# ╔═╡ 5d775d7b-98d6-4d96-aee2-bae961379fd4
md"""## Run Model"""

# ╔═╡ 4b62b282-86bd-11eb-2fed-bbbe8ef2d4af
md"""

This should also happen automatically once at first, and once after modifying parameters.

You can also click on the `Launch Model` button to trigger a model run manually.
"""

# ╔═╡ 6f618b2c-86bd-11eb-1607-a179a349378e
@bind do_run1 Button("Launch Model")

# ╔═╡ 6404fddf-3c46-4015-9580-b9159c76b30a
md"""### Explore Model Output

###

Below is a list of all files (by default) contained in the `run/` directory. Clicking on a list like this should expand its display.

To narrow the selection, try typing something in the text field 👉 $(@bind search_txt TextField(; default=""))	👈
"""

# ╔═╡ 0f920f90-86e9-11eb-3f6d-2d530bd2e9db
md"""### Plot Model Result

###

Here we show mean temperature in **$(exps[iexp].configuration)** as a function of time. 

_Note: in the absence of air-sea fluxes, for example, this quantity is "conserved" but in other configurarions it can vary with time._
"""

# ╔═╡ e6c10fb5-ee95-41a0-982e-3910a8ce1d00
md"""

If a map appears below (only for some configurations), then it can be animated by clicking on `start`.

$(@bind t_slow Clock(1.0, true))
"""

# ╔═╡ c7670d00-868c-11eb-1889-4d3ffe621dd2
md"""## Modify Parameters

###

Often we want to experiment with parameters e.g. to tune models, generate more output, or generate a model ensemble. The climate model interface provides a simple way to do this interactively and document changes via `git` along the way.

**First, select a model parameter group** (or the default): 
"""

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

# ╔═╡ af176e6c-8695-11eb-3e34-91fbdb9c52fa
md"""### Appendices

###

Code cells below handle Julia packages and help set up the notebook.
"""

# ╔═╡ 348c692e-84fe-11eb-3288-dd0a1dedce90
begin	
	fil=joinpath(rundir,mydats)
	nml=read(fil,MITgcm_namelist())
	md"""😸"""
end

# ╔═╡ ca7bb004-8510-11eb-379f-632c3b40723d
try
	@bind nmlgroup Select(String.(nml.groups))
catch e
	"Error: could not find any namelist in $(rundir)"
end

# ╔═╡ 002171f5-8d6d-4197-8a3c-642f3337a01b
begin
	nmlgroup
	
	md"""
	
	**Then, enter parameter name** (without ":") **and new value:** 
	
	parameter name 👉 $(@bind p_name TextField(; default=""))	👈
	
	new value 👉 $(@bind p_value TextField(; default=""))	👈
	
	**Once ready, click** `Update & Relaunch` **to:**

	- update parameter file
	- rerun the model
	- update the plots 

	$(@bind update_param Button("Update & Relaunch"))
	
	"""
end

# ╔═╡ 52d7c7a2-8693-11eb-016f-4fc3eb516d44
begin
	inml=findall(nml.groups.==Symbol(nmlgroup))[1]
	md"""😸"""
end

# ╔═╡ e658d885-ad25-4b47-b0ee-6c97a204f731
begin
	update_param
	nml.params[inml]
end

# ╔═╡ 15746ef0-8617-11eb-1160-5f48a95d94d0
begin
	update_param
	
	if !isempty(p_value)
		tmptype=typeof(nml.params[inml][Symbol(p_name)])
		nml.params[inml][Symbol(p_name)]=parse(tmptype,p_value)
		
		tmpfil=joinpath(rundir,mydats)
		rm(tmpfil)
		write(tmpfil,nml)

		tmpfil=joinpath("tracked_parameters",mydats)
		git_log_fil(exps[iexp],tmpfil,"update parameter file")
	end
	
	#@bind do_run2 Button("Launch Model")
	do_run2="🐎"
	
	md"""🐎"""
end

# ╔═╡ 96492c18-86bd-11eb-35ca-dff79e6e7818
begin
	do_run1
	do_run2
	isempty(exps[iexp].channel) ? put!(exps[iexp].channel,MITgcm_launch) : nothing
	launch(exps[iexp])
	refresh_plot=true
	md"""Model run for the **$(exps[iexp].configuration)** configuration has completed!
	
	🏇 🏁 🏁 🏁 🎉 🎊 """
end

# ╔═╡ 6edcbba3-8485-44d2-940a-e4f2df019373
begin
	refresh_plot
	list1=readdir(rundir)
	search_txt!=="*" ? list1[findall(occursin.(search_txt,list1))] : list1
end

# ╔═╡ d0bbb668-86e0-11eb-1a9b-8f2b0175f7c1
begin
	refresh_plot

	run(pipeline(`grep dynstat_theta_mean $(filout)`,filstat))

	tmp0 = read(filstat,String)
	tmp0 = split(tmp0,"\n")
	Tmean=[parse(Float64,split(tmp0[i],"=")[2]) for i in 1:length(tmp0)-1]
end

# ╔═╡ 8ad9d646-4eec-45b9-938b-21df34da2d6b
begin
	refresh_plot
	
	if exps[iexp].configuration=="advect_xy"||exps[iexp].configuration=="advect_xz"
		tmp2=readdir(rundir)
		tmp2=tmp2[findall(occursin.("T.0000",tmp2))]
		tmp2=tmp2[findall(occursin.("001.001.data",tmp2))]
		tmp2=[i[1:end-13] for i in tmp2]
	
		i=mod(t_slow,length(tmp2))
		tmp3=read_mdsio(rundir,tmp2[i+1])
		length(size(tmp3))==3 ? tmp3=dropdims(tmp3;dims=2) : nothing
		
		contourf(tmp3,levels=(-0.04:0.02:0.2),leg=:none,c = :terrain)
	else
		plot(Tmean,label="mean temperature")
	end
end

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
	
	md"""😸"""
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
# ╟─5d775d7b-98d6-4d96-aee2-bae961379fd4
# ╟─4b62b282-86bd-11eb-2fed-bbbe8ef2d4af
# ╟─6f618b2c-86bd-11eb-1607-a179a349378e
# ╟─96492c18-86bd-11eb-35ca-dff79e6e7818
# ╟─6404fddf-3c46-4015-9580-b9159c76b30a
# ╟─6edcbba3-8485-44d2-940a-e4f2df019373
# ╟─0f920f90-86e9-11eb-3f6d-2d530bd2e9db
# ╟─d0bbb668-86e0-11eb-1a9b-8f2b0175f7c1
# ╟─e6c10fb5-ee95-41a0-982e-3910a8ce1d00
# ╟─8ad9d646-4eec-45b9-938b-21df34da2d6b
# ╟─c7670d00-868c-11eb-1889-4d3ffe621dd2
# ╟─d7f2c656-8512-11eb-2fdf-47a3e57a55e6
# ╟─ca7bb004-8510-11eb-379f-632c3b40723d
# ╟─e658d885-ad25-4b47-b0ee-6c97a204f731
# ╟─002171f5-8d6d-4197-8a3c-642f3337a01b
# ╟─15746ef0-8617-11eb-1160-5f48a95d94d0
# ╟─af176e6c-8695-11eb-3e34-91fbdb9c52fa
# ╟─8cf4d8ca-84eb-11eb-22d2-255ce7237090
# ╟─348c692e-84fe-11eb-3288-dd0a1dedce90
# ╟─52d7c7a2-8693-11eb-016f-4fc3eb516d44
# ╟─734e2b5a-8866-11eb-0025-bd9544f4c30d
# ╟─901d2844-83be-4767-b169-dfb7701ce15e

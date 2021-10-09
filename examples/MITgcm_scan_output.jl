### A Pluto.jl notebook ###
# v0.16.1

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

# ╔═╡ bb47e9ec-05ce-11ec-265e-85e1b4e90854
begin
	using Pkg; Pkg.activate()
	
	using MITgcmTools, MeshArrays, PlutoUI, CairoMakie
	import Meshes, MeshViz
	"all set with packages"
end

# ╔═╡ f883622e-dada-4acf-9c90-2c3a3373da66
md"""# MITgcm Output And Grids

This notebook demonstrates some of the tools available to scan, read, and display model output.
"""

# ╔═╡ 8586f798-00a3-4ec5-a360-5e709f3c6a72
begin
	Γecco=GridLoad(GridSpec("LatLonCap",MeshArrays.GRID_LLC90))
	"One grid has been read from file."
end

# ╔═╡ 4e669f82-8bbc-4df1-847d-bc61c24884c2
function myviz(G; title="grid points")
	set_theme!(theme_light())
    fig = Figure(resolution = (900,600),markersize=0.1)
    ax = Axis(fig[1,1],xlabel="x",ylabel="y",title=title)

    #col=[:blue,:greenyellow,:magenta,:yellow2,:tomato,:black]
    col=[:red,:green,:blue,:magenta,:cyan,:black]

    for f in 1:length(G.XC)
        tmp=[Meshes.Point2(G.XC[f][i],G.YC[f][i]) for i in eachindex(G.XC[f])]
        MeshViz.viz!(ax,tmp,elementcolor=col[f],markersize=2.0)
    end

    fig
end

# ╔═╡ 98762420-6496-4760-8049-5a3fed396984
md"""## 1. Scan `MITgcm/verification` folder

The `MITgcm/verification` folder contains a series of model configurations. These are compiled and run daily as part of the model stndard testing suite, to help ensure reproducibility (consistency of the code & results through time).

Here we scan subfolders within `MITgcm/verification`. We look for the model standard output file which is typically named something like `MITgcm/verification/advect_cs/run/output.txt`. This pattern is used by the `MITgcm` community for the model testing suite (see list reported below). 

The model standard output file for each `run` subfolder is then scanned for information of the model configuration and its output. If we don't find `output.txt` (or `STDOUT.0000`) in a `run` then it is assumed that this model configuration has not been run yet and should therefore be ignored.
""" 

# ╔═╡ 8ab359c9-7090-4671-8856-e775ee4e7556
begin
	#list of verification experiments
	rep=joinpath(MITgcm_path[1],"verification")
	exps=verification_experiments()

	#
	sc=Vector{Any}(nothing, length(exps))
	for i in 1:length(exps)
		myexp=exps[i].configuration; rundir=joinpath(rep,myexp,"run")
		sc[i]=scan_rundir(rundir)
	end
	
	list_exps=collect(1:length(exps))
	
	with_terminal() do
		println("List of all $(length(exps)) experiments found : \n\n")
		for i in 1:length(exps)
			println(exps[i].configuration)
		end
	end
	
end

# ╔═╡ 5d739d43-e39b-45d5-8a68-87ee85ae0463
begin
	ii=findall((!ismissing).(sc))
	list_mdsio=findall([sc[i].params_files.use_mdsio for i in ii])
	list_mnc=findall([sc[i].params_files.use_mnc for i in ii])
	list_missing=findall((ismissing).(sc))
	a=(length(list_mdsio),length(list_mnc),length(list_missing),length(exps))
	md""" Result of scanning `run` subfolders :
	
	| All `run` folders | $(a[4])   |
	|-------------------|-----------|
	| Empty run folders | $(a[3])   |
	| With binary output| $(a[1])   |
	| With netcdf output| $(a[2])   |
	
	"""
end

# ╔═╡ adf96bf1-b405-4405-a747-e2835b670a25
md"""## 2. Select Configuration

The grid initially selected (ECCO) is downloaded automatically. Click on _verification_ to instead look at the contents of `MITgcm/verification` discussed earlier on in this notebook.
"""

# ╔═╡ 1a6eca66-819c-49a9-b245-1faf3290b65d
@bind tst Radio(["ECCO","verification"], default="ECCO")

# ╔═╡ 44263dfc-68b9-4ed8-8d7a-3548cfecdace
begin
	myaz_slider = @bind myaz NumberField(-1:0.1:1; default=-0.2)
	tick_button = @bind tick Clock(2)
	md"""
	Azimuth for 3d view (if applicable) : 
	
	$(myaz_slider)
	
	####
	
	Click to loop through (if verification) : 
	
	$(tick_button)	
	"""
end

# ╔═╡ 4d7bf60f-e71c-4a90-b10a-312c4545c555
md"""## 3. Visualize Grid

"""

# ╔═╡ 211ab33e-d482-49dd-9448-0f5c6e63a280
begin
	tick; 
	
	if tst=="ECCO"
		i=0
		siz=Γecco.XC.grid.ioSize
		az=myaz*π; el=0.2π
		f=myviz(Γecco; title="ECCO version 4 grid \n grid name = LLC90      size = $siz")
	else
		n=length(list_exps)
		isnothing(tick) ? kk=1 : kk=max(min(tick,n),1)
		i=list_exps[kk]

		#i=list_exps[ii]

		if sc[i].params_files.use_mdsio
			Γ=GridLoad_mdsio(exps[i])
		else
			Γ=GridLoad_mnc(exps[i])
		end

		is2d=sum(sc[i].params_files.ioSize.>1)==2
		siz=sc[i].params_files.ioSize

		tt=exps[i].configuration*" \n exp ID = $i / $n           size = $siz"
		if sc[i].params_grid.usingCartesianGrid&&is2d
			f=myviz(Γ;title=tt)
		elseif sc[i].params_grid.usingCylindricalGrid&&is2d
			f=myviz(Γ;title=tt)
		elseif is2d
			f=myviz(Γ;title=tt)
		else
			f=myviz(Γ;title=tt)
		end
		f
	end
end

# ╔═╡ a444cf7e-cbe1-4e13-981b-184f1a64d3d5
 i>0 ? exps[i] : "ECCO v4 / LLC90 grid"

# ╔═╡ 49e5553c-b316-4c2c-821a-0dd6148006dc
 i>0 ? sc[i].params_grid : typeof(Γecco.XC)

# ╔═╡ 405d7388-074d-400f-bc77-054dd10bc51f
 i>0 ? sc[i].params_files.ioSize : Γecco.XC.grid.ioSize

# ╔═╡ 92b6319a-a56d-483e-a7eb-6f71966364c5
begin
	i>0 ? dd=Float64.(Γ.Depth) : dd=Float64.(Γecco.Depth)
	minimum(dd),maximum(dd)
end

# ╔═╡ Cell order:
# ╟─f883622e-dada-4acf-9c90-2c3a3373da66
# ╟─bb47e9ec-05ce-11ec-265e-85e1b4e90854
# ╟─8586f798-00a3-4ec5-a360-5e709f3c6a72
# ╟─4e669f82-8bbc-4df1-847d-bc61c24884c2
# ╟─98762420-6496-4760-8049-5a3fed396984
# ╟─5d739d43-e39b-45d5-8a68-87ee85ae0463
# ╟─8ab359c9-7090-4671-8856-e775ee4e7556
# ╟─adf96bf1-b405-4405-a747-e2835b670a25
# ╟─1a6eca66-819c-49a9-b245-1faf3290b65d
# ╟─4d7bf60f-e71c-4a90-b10a-312c4545c555
# ╟─44263dfc-68b9-4ed8-8d7a-3548cfecdace
# ╟─211ab33e-d482-49dd-9448-0f5c6e63a280
# ╟─a444cf7e-cbe1-4e13-981b-184f1a64d3d5
# ╟─49e5553c-b316-4c2c-821a-0dd6148006dc
# ╟─405d7388-074d-400f-bc77-054dd10bc51f
# ╟─92b6319a-a56d-483e-a7eb-6f71966364c5

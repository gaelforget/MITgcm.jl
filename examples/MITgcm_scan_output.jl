### A Pluto.jl notebook ###
# v0.15.1

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
	import Pkg
	Pkg.activate() # activate the global environment
	
	using MITgcmTools, MeshArrays, PlutoUI, GLMakie
	
	p=dirname(pathof(MeshArrays))
	include(joinpath(p,"..","examples","Makie.jl"))

	"all set with packages"
end

# ╔═╡ f883622e-dada-4acf-9c90-2c3a3373da66
md"""## 0. Packages And Config Lists
"""

# ╔═╡ 8ab359c9-7090-4671-8856-e775ee4e7556
begin
	rep=joinpath(MITgcm_path[1],"verification")
	exps=verification_experiments()
	
	sc=Vector{Any}(nothing, length(exps))
	for i in 1:length(exps)
		myexp=exps[i].configuration; rundir=joinpath(rep,myexp,"run")
		sc[i]=MITgcmTools.scan_rundir(rundir)
	end
	
	[exps[i].configuration for i in 1:length(exps)]
end

# ╔═╡ 5d739d43-e39b-45d5-8a68-87ee85ae0463
list_mdsio=findall([sc[i].params_files.use_mdsio for i in 1:length(exps)])

# ╔═╡ 40f09d63-a884-41e8-ad07-7a850014ccfa
list_mnc=findall([sc[i].params_files.use_mnc for i in 1:length(exps)])

# ╔═╡ 6c8260cc-35ae-4f6d-a3e8-df6f33ed3e81
(length(list_mdsio),length(list_mnc),length(exps))

# ╔═╡ adf96bf1-b405-4405-a747-e2835b670a25
md"""## 1. Select Config. : binary output cases

_Note: this will update the plot and text display below_
"""

# ╔═╡ 7c37d2bd-2a10-4a42-9029-87d636c3a054
@bind ii NumberField(1:length(list_mdsio); default=1)

# ╔═╡ 211ab33e-d482-49dd-9448-0f5c6e63a280
begin
	i=list_mdsio[ii]
	Γ=MITgcmTools.GridLoad_mdsio(exps[i])
	
	is2d=sum(sc[i].params_files.ioSize.>1)==2
	if sc[i].params_grid.usingCartesianGrid
		 f=plot_as_plane(Γ)
	elseif is2d
		f=plot_as_sphere(Γ)
	else
		f="no display available"
	end
	f
end

# ╔═╡ a444cf7e-cbe1-4e13-981b-184f1a64d3d5
 exps[i]

# ╔═╡ 49e5553c-b316-4c2c-821a-0dd6148006dc
sc[i].params_grid

# ╔═╡ 92b6319a-a56d-483e-a7eb-6f71966364c5
begin
	dd=Float64.(Γ.Depth)
	minimum(dd),maximum(dd)
end

# ╔═╡ d43a7659-9466-449c-9bf1-0c7ee668ce82
md"""## 2. Select Config. : netcdf output cases

_Note: this will update the plot and text display below_
"""

# ╔═╡ 9a6583c8-325d-49e9-95cb-f10a33d16394
@bind jj NumberField(1:length(list_mnc); default=2)

# ╔═╡ ce15c4e2-e6d9-4908-a336-14de39fd3c20
begin
	j=list_mnc[jj]
	rundir=joinpath(rep,exps[j].configuration,"run"); exps[j]
	
	pth=joinpath(rundir,"mnc_test_0001")
	XC=MITgcmTools.read_mnc(pth,"grid","XC")
	s1=(sc[j].params_grid.sNx*sc[j].params_grid.nSx,sc[j].params_grid.sNx*sc[j].params_grid.nSx)
    s2=[s1[1] s1[2]]
	elty=eltype(XC)
	
	γ_mnc=gcmgrid(rundir,"PeriodicDomain",1,fill(s1,1),s2,elty, read, write)

	Γ_mnc=MITgcmTools.GridLoad_mnc(γ_mnc)
	
	plot_as_plane(Γ_mnc)
end

# ╔═╡ 52500893-60c9-4f62-ae38-9e07ee76a34b
 exps[j]

# ╔═╡ 4f9c9197-9d83-4545-8d41-92c904a29c9f
sc[j].params_grid

# ╔═╡ Cell order:
# ╟─f883622e-dada-4acf-9c90-2c3a3373da66
# ╟─bb47e9ec-05ce-11ec-265e-85e1b4e90854
# ╟─8ab359c9-7090-4671-8856-e775ee4e7556
# ╟─5d739d43-e39b-45d5-8a68-87ee85ae0463
# ╟─40f09d63-a884-41e8-ad07-7a850014ccfa
# ╟─6c8260cc-35ae-4f6d-a3e8-df6f33ed3e81
# ╟─adf96bf1-b405-4405-a747-e2835b670a25
# ╟─7c37d2bd-2a10-4a42-9029-87d636c3a054
# ╟─211ab33e-d482-49dd-9448-0f5c6e63a280
# ╟─a444cf7e-cbe1-4e13-981b-184f1a64d3d5
# ╟─49e5553c-b316-4c2c-821a-0dd6148006dc
# ╟─92b6319a-a56d-483e-a7eb-6f71966364c5
# ╟─d43a7659-9466-449c-9bf1-0c7ee668ce82
# ╟─9a6583c8-325d-49e9-95cb-f10a33d16394
# ╟─ce15c4e2-e6d9-4908-a336-14de39fd3c20
# ╟─52500893-60c9-4f62-ae38-9e07ee76a34b
# ╟─4f9c9197-9d83-4545-8d41-92c904a29c9f

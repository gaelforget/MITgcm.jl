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

	tst_XC=Vector{Any}(nothing, length(exps))
	tst_mnc=Vector{Any}(nothing, length(exps))
	vec_ioSize=Vector{Any}(nothing, length(exps))

	for i in 1:length(exps)
		myexp=exps[i].configuration; rundir=joinpath(rep,myexp,"run")
		tst_XC[i] = !isempty(filter(x -> occursin("XC",x), readdir(rundir)))
		tst_mnc[i] = isdir(joinpath(rundir,"mnc_test_0001"))
		if tst_XC[i] 
			myexp=exps[i].configuration; rundir=joinpath(rep,myexp,"run")
			tmp=read_mdsio(rundir,"XC")
			vec_ioSize[i]=size(tmp)
		end
	end
	
	[exps[i].configuration for i in 1:length(exps)]
end

# ╔═╡ c221da29-f3b1-4f09-a85d-7e4330eeddf9
findall(tst_XC)

# ╔═╡ e571794a-2355-4e65-9437-7318ba3f1ef4
findall(tst_mnc)

# ╔═╡ 6c8260cc-35ae-4f6d-a3e8-df6f33ed3e81
(sum(tst_XC),sum(tst_mnc),length(exps))

# ╔═╡ adf96bf1-b405-4405-a747-e2835b670a25
md"""## 1. Select Config. : binary output cases

_Note: this will update the plot and text display below_
"""

# ╔═╡ 7c37d2bd-2a10-4a42-9029-87d636c3a054
@bind ii NumberField(1:length(findall(tst_XC)); default=1)

# ╔═╡ 0c1e7a7e-6165-43a3-ab06-2ab82d533205
i=findall(tst_XC)[ii]

# ╔═╡ 211ab33e-d482-49dd-9448-0f5c6e63a280
begin
	Γ=MITgcmTools.GridLoad_mdsio(exps[i])
	
	is2d=sum(vec_ioSize[i].>1)==2
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
@bind jj NumberField(1:length(findall(tst_mnc)); default=1)

# ╔═╡ 91cf76da-1137-4f12-9a1f-b9b44a920911
j=findall(tst_mnc)[jj]

# ╔═╡ 4e6a4410-9823-48fb-a9db-5e6296fa8d32
myexp=exps[j].configuration; rundir=joinpath(rep,myexp,"run"); exps[j]

# ╔═╡ ce15c4e2-e6d9-4908-a336-14de39fd3c20
begin
	pth=joinpath(rundir,"mnc_test_0001")
	XC=MITgcmTools.read_mnc(pth,"grid","XC")
	s1=(sc[j].params_grid.sNx*sc[j].params_grid.nSx,sc[j].params_grid.sNx*sc[j].params_grid.nSx)
    s2=[s1[1] s1[2]]
	elty=eltype(XC)
	
	γ_mnc=gcmgrid(rundir,"PeriodicDomain",1,fill(s1,1),s2,elty, read, write)

	Γ_mnc=MITgcmTools.GridLoad_mnc(γ_mnc)
	
	plot_as_plane(Γ_mnc)
end

# ╔═╡ 4f9c9197-9d83-4545-8d41-92c904a29c9f
sc[j].params_grid

# ╔═╡ Cell order:
# ╟─f883622e-dada-4acf-9c90-2c3a3373da66
# ╟─bb47e9ec-05ce-11ec-265e-85e1b4e90854
# ╟─8ab359c9-7090-4671-8856-e775ee4e7556
# ╟─c221da29-f3b1-4f09-a85d-7e4330eeddf9
# ╟─e571794a-2355-4e65-9437-7318ba3f1ef4
# ╟─6c8260cc-35ae-4f6d-a3e8-df6f33ed3e81
# ╟─adf96bf1-b405-4405-a747-e2835b670a25
# ╟─7c37d2bd-2a10-4a42-9029-87d636c3a054
# ╟─211ab33e-d482-49dd-9448-0f5c6e63a280
# ╟─0c1e7a7e-6165-43a3-ab06-2ab82d533205
# ╟─a444cf7e-cbe1-4e13-981b-184f1a64d3d5
# ╟─49e5553c-b316-4c2c-821a-0dd6148006dc
# ╟─92b6319a-a56d-483e-a7eb-6f71966364c5
# ╟─d43a7659-9466-449c-9bf1-0c7ee668ce82
# ╟─9a6583c8-325d-49e9-95cb-f10a33d16394
# ╠═ce15c4e2-e6d9-4908-a336-14de39fd3c20
# ╟─91cf76da-1137-4f12-9a1f-b9b44a920911
# ╟─4e6a4410-9823-48fb-a9db-5e6296fa8d32
# ╟─4f9c9197-9d83-4545-8d41-92c904a29c9f

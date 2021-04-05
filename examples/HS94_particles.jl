### A Pluto.jl notebook ###
# v0.14.0

using Markdown
using InteractiveUtils

# ╔═╡ 02005854-3442-4c65-910b-e8a000805d17
begin
	md"""# HS94_particles.jl

	### 


	Here we setup, run and plot MITgcm output via **`IndividualDisplacements.jl`** to generate something like this:
	
	![plot](https://user-images.githubusercontent.com/20276764/111042787-12377e00-840d-11eb-8ddb-64cc1cfd57fd.png)
	"""
end

# ╔═╡ a6ebb38e-2dcd-4126-bfa1-220df3180b94
begin
	#Path to model output
	pth="run_HS94/"
	
	md""" ### Julia Packages And Model Files
	
	Model grid and output files are expected to be found in _$(pth)_.
	First, a list of grid variables should appear just below if files are indeed found as expected.
	"""
end

# ╔═╡ bf462d7b-28af-4fc5-9952-e631051df4cd
begin
	#packages for Lagrangian particle advection etc	
	using MITgcmTools, MeshArrays, IndividualDisplacements, OrdinaryDiffEq
	p=dirname(pathof(IndividualDisplacements))
	include(joinpath(p,"../examples/recipes_plots.jl"))

	#Read and write functions for cube-sphere grid as configured in this model run
	readcube(xx::Array,x::MeshArray) = read(cube2compact(xx),x)
	readcube(fil::String,x::MeshArray) = read(fil::String,x::MeshArray)
	writecube(x::MeshArray) = compact2cube(write(x))
	writecube(fil::String,x::MeshArray) = write(fil::String,x::MeshArray)
	
	#Load grid files to memory
	γ=gcmgrid(pth,"CubeSphere",6,fill((32, 32),6), [192 32], Float64, readcube, writecube)
	Γ = GridLoad(γ)
	keys(Γ)
end

# ╔═╡ 05cbf6ff-032f-4090-8981-da931f9e1521
md""" ### Functions And Data Structures

Individuals 𝐼 are those that will be advected by the flow fields using the `∫!` function. 
This can be done in a time loop with `update_FlowFields!` used to load flow fields in a 
sequence as shown below.
"""		

# ╔═╡ e032b6d4-959d-11eb-28df-2112db1b1e4e
begin
		
	#Parameters: level, time, U/V files, grid factors
	k=3
	dt=7200.0
	t00=43200.0*600
	
	tmp=readdir(pth)
	Ul=tmp[findall(occursin.("U.0000",tmp).*occursin.(".data",tmp))]
	Vl=tmp[findall(occursin.("V.0000",tmp).*occursin.(".data",tmp))]
	ndt=length(Ul)-1
	
	𝐷=(pth=pth,k=k,dt=dt,t00=t00,Γ=Γ,
	    U=Ul,V=Vl,tmp=MeshArray(γ,Float64,5),    
	    XC=exchange(Γ["XC"]),YC=exchange(Γ["YC"]),
	    iDXC=1. ./Γ["DXC"], iDYC=1. ./Γ["DYC"])
	
	update_loc=(u -> IndividualDisplacements.update_location_cs!(u,𝐷))
	tmp = IndividualDisplacements.dict_to_nt(
	    IndividualDisplacements.NeighborTileIndices_cs(Γ))
	𝐷 = merge(𝐷 , tmp)
	
	#FlowFields data structure
	𝑃=𝐹_MeshArray2D{Float64}(MeshArray(γ,Float64),MeshArray(γ,Float64),
	    MeshArray(γ,Float64),MeshArray(γ,Float64),[t00-dt,t00],update_loc)    
	
	#Individuals data structure
	n=100; x=24 .+ randn(n); y=24 .+ randn(n); f=fill(1,n);
	custom∫(prob) = solve(prob,Tsit5(),reltol=1e-5,abstol=1e-5,save_everystep=false)
	𝐼=Individuals(𝑃,x,y,f,(;∫=custom∫))
	
	#Function that read velocity fields from 𝐼.𝑃.𝑇[2] and at level=𝐷.k
	function update_FlowFields!(𝐼::Individuals,𝐷::NamedTuple)
	    m0=Int(floor((𝐼.𝑃.𝑇[2]-𝐷.t00)/𝐷.dt))+1
	    m1=m0+1
	    t0=m0*𝐷.dt-𝐷.dt+𝐷.t00
	    t1=m1*𝐷.dt-𝐷.dt+𝐷.t00
	    #println(m0)
	
	    u0=read(𝐷.pth*𝐷.U[m0],𝐷.tmp)[:,𝐷.k]
	    v0=read(𝐷.pth*𝐷.V[m0],𝐷.tmp)[:,𝐷.k]
	    u0=u0.*𝐷.iDXC; v0=v0.*𝐷.iDYC; #normalize to grid units
	    (u0,v0)=exchange(u0,v0,1) #add 1 point at each edge for u and v
	
	    u1=read(𝐷.pth*𝐷.U[m1],𝐷.tmp)[:,𝐷.k]
	    v1=read(𝐷.pth*𝐷.V[m1],𝐷.tmp)[:,𝐷.k]
	    u1=u1.*𝐷.iDXC; v1=v1.*𝐷.iDYC; #normalize to grid units
	    (u1,v1)=exchange(u1,v1,1) #add 1 point at each edge for u and v
	
	    𝑃.u0[:]=u0[:]
	    𝑃.u1[:]=u1[:]
	    𝑃.v0[:]=v0[:]
	    𝑃.v1[:]=v1[:]
	    𝑃.𝑇[:]=[t0,t1]
	end
	
	𝐼
end

# ╔═╡ 5a7af9d3-b800-44fd-9ffe-0580b51ee70a
md""" ### Trajectory Computation

The particle trajectory output is in tabular format. To save results to file, a simple yet general method is :

```
using Dates, JLD2
M=(timestamp=now(),author="me",run="HS94")
I=(metadata=M, ID=𝐼.🆔, record=𝐼.🔴, position=𝐼.📌)
@save "HS94.jld2" I
```
"""

# ╔═╡ 7cc13ee0-ad61-4274-9c94-cbfa3974046b
begin
	#Read velocity fields and compute trajectories
	update_FlowFields!(𝐼,𝐷)
	∫!(𝐼)
	
	#Plot initial and final locations
	f1=plot(𝐼)
		
	#Carry on for several time intervals (𝐼.𝑃.𝑇 .+ 𝐷.dt and so on)
	for tt=1:ndt-1
	    update_FlowFields!(𝐼,𝐷)
	    ∫!(𝐼)
	end
	f2=plot(𝐼)
	
	#Add longitude and latitude
	add_lonlat!(𝐼.🔴,𝐷.XC,𝐷.YC)
end

# ╔═╡ 5129ac14-0861-40b5-b97e-0d0edd37849f
md""" ### Plots

The plots below show:

- 1. initial positions (red) and after 1 day (blue; `x,y`)
- 2. initial and final positions ($ndt days; `x,y`)
- 3. initial and final coordinates ($ndt days; `lon,lat`)

To save a plot, add: 

```
savefig(tempdir()*"/"*"hs94.cs.particles.png")
```
"""

# ╔═╡ a34adf36-81de-42c2-8407-d408041193b6
f1

# ╔═╡ a1fdaf62-1b80-481e-a5c7-45e82c540368
f2

# ╔═╡ c473fb90-8901-466e-a8e3-c7efd189e1ba
	f3=plot(𝐼)

# ╔═╡ Cell order:
# ╠═02005854-3442-4c65-910b-e8a000805d17
# ╟─a6ebb38e-2dcd-4126-bfa1-220df3180b94
# ╟─bf462d7b-28af-4fc5-9952-e631051df4cd
# ╟─05cbf6ff-032f-4090-8981-da931f9e1521
# ╟─e032b6d4-959d-11eb-28df-2112db1b1e4e
# ╟─5a7af9d3-b800-44fd-9ffe-0580b51ee70a
# ╟─7cc13ee0-ad61-4274-9c94-cbfa3974046b
# ╟─5129ac14-0861-40b5-b97e-0d0edd37849f
# ╟─a34adf36-81de-42c2-8407-d408041193b6
# ╟─a1fdaf62-1b80-481e-a5c7-45e82c540368
# ╟─c473fb90-8901-466e-a8e3-c7efd189e1ba

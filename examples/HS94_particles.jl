### A Pluto.jl notebook ###
# v0.14.0

using Markdown
using InteractiveUtils

# ╔═╡ e032b6d4-959d-11eb-28df-2112db1b1e4e
begin
	
	## Lagrangian particle advection
	
	using MITgcmTools, MeshArrays, IndividualDisplacements, OrdinaryDiffEq
	
	#Model grid, which should be made provided in folder `pth` below
	pth="run_HS94/"
	
	readcube(xx::Array,x::MeshArray) = read(cube2compact(xx),x)
	readcube(fil::String,x::MeshArray) = read(fil::String,x::MeshArray)
	writecube(x::MeshArray) = compact2cube(write(x))
	writecube(fil::String,x::MeshArray) = write(fil::String,x::MeshArray)
	
	γ=gcmgrid(pth,"CubeSphere",6,fill((32, 32),6), [192 32], Float64, readcube, writecube)
	Γ = GridLoad(γ)
	
	#parameters: level, time, U/V files, grid factors
	k=3
	dt=7200.0
	t00=43200.0*600
	
	tmp=readdir(pth)
	Ul=tmp[findall(occursin.("U.0000",tmp).*occursin.(".data",tmp))]
	Vl=tmp[findall(occursin.("V.0000",tmp).*occursin.(".data",tmp))]
	
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
	
	#Read velocity fields and compute trajectories
	update_FlowFields!(𝐼,𝐷)
	∫!(𝐼)
	
	#Plot initial and final locations
	p=dirname(pathof(IndividualDisplacements))
	include(joinpath(p,"../examples/recipes_plots.jl"))
	f1=plot(𝐼)
	
	#To save this plot, e.g. : 
	#```
	#savefig(tempdir()*"/"*"hs94.cs.particles.png")
	#```
	
	#Carry on for several time intervals (𝐼.𝑃.𝑇 .+ 𝐷.dt and so on)
	for tt=1:length(𝐷.U)-2
	    update_FlowFields!(𝐼,𝐷)
	    ∫!(𝐼)
	end
	f2=plot(𝐼)
	
	#Add longitude and latitude
	add_lonlat!(𝐼.🔴,𝐷.XC,𝐷.YC)
	
	#To save results, e.g. :
	#```
	#using Dates, JLD2
	#M=(timestamp=now(),author="me",run="HS94")
	#I=(metadata=M, ID=𝐼.🆔, record=𝐼.🔴, position=𝐼.📌)
	#@save "HS94.jld2" I
	#```
	
end

# ╔═╡ a34adf36-81de-42c2-8407-d408041193b6
f1

# ╔═╡ a1fdaf62-1b80-481e-a5c7-45e82c540368
f2

# ╔═╡ Cell order:
# ╟─e032b6d4-959d-11eb-28df-2112db1b1e4e
# ╟─a34adf36-81de-42c2-8407-d408041193b6
# ╟─a1fdaf62-1b80-481e-a5c7-45e82c540368

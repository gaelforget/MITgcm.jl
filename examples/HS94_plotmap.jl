### A Pluto.jl notebook ###
# v0.14.0

using Markdown
using InteractiveUtils

# ╔═╡ 3668f786-9597-11eb-01a1-87d34b49eef9
begin
	
	using MITgcmTools, MeshArrays, Plots
	
	pth="run_HS94/"
	
	readcube(xx::Array,x::MeshArray) = read(cube2compact(xx),x)
	readcube(fil::String,x::MeshArray) = read(fil::String,x::MeshArray)
	writecube(x::MeshArray) = compact2cube(write(x))
	writecube(fil::String,x::MeshArray) = write(fil::String,x::MeshArray)
	
	γ=gcmgrid(pth,"CubeSphere",6,fill((32, 32),6), [192 32], Float64, readcube, writecube)
	Γ = GridLoad(γ)
	
	## Interpolation setup for plotting
	
	lon=[i for i=-179.5:1.0:179.5, j=-89.5:1.0:89.5]
	lat=[j for i=-179.5:1.0:179.5, j=-89.5:1.0:89.5]
	(f,i,j,w,_,_,_)=InterpolationFactors(Γ,vec(lon),vec(lat))
	IntFac=(f,i,j,w)
	
	##
	
	ff=readdir(pth); fil="T.0000"
	ff=ff[findall(occursin.(fil,ff).*occursin.(".data",ff))]
	
	nt=length(ff)
	
	function myplot(fil)
	    T=read(pth*fil,MeshArray(γ,Float64))
	    TT=Interpolate(T,IntFac...)
	    contourf(vec(lon[:,1]),vec(lat[1,:]),TT,clims=(260.,320.))
	end
	
	##
	
	#dt=Int(ceil(nt/100))
	dt=6
	
	#g1 = @gif for i ∈ 1:dt:nt
	#    myplot(ff[i])
	#end
	
	myplot(ff[end])
	
	#```
	#anim = @animate for i ∈ 1:dt:nt
	#    myplot(ff[i])
	#end
	#pp=tempdir()*"/"
	#gif(anim,pp*"hs94.cs.gif", fps = 8)
	#```
	
end

# ╔═╡ Cell order:
# ╟─3668f786-9597-11eb-01a1-87d34b49eef9

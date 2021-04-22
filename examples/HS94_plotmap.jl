### A Pluto.jl notebook ###
# v0.14.2

using Markdown
using InteractiveUtils

# ╔═╡ 3668f786-9597-11eb-01a1-87d34b49eef9
begin
	
	#packages for I/O, interpolation, etc	
	using MITgcmTools, MeshArrays, Plots
	
	#Path to model output
	pth="run_HS94/"

	
	#Read and write functions for cube-sphere grid as configured in this model run	
	readcube(xx::Array,x::MeshArray) = read(cube2compact(xx),x)
	readcube(fil::String,x::MeshArray) = read(fil::String,x::MeshArray)
	writecube(x::MeshArray) = compact2cube(write(x))
	writecube(fil::String,x::MeshArray) = write(fil::String,x::MeshArray)
	
	#Load grid files to memory
	γ=gcmgrid(pth,"CubeSphere",6,fill((32, 32),6), [192 32], Float64, readcube, writecube)
	Γ = GridLoad(γ)
	
	## Interpolation setup for plotting
	lon=[i for i=-179.5:1.0:179.5, j=-89.5:1.0:89.5]
	lat=[j for i=-179.5:1.0:179.5, j=-89.5:1.0:89.5]
	(f,i,j,w,_,_,_)=InterpolationFactors(Γ,vec(lon),vec(lat))
	IntFac=(f,i,j,w)
	
	🏁 = "🏁"
end

# ╔═╡ 19095067-33f5-495f-bc4d-ee6dacbf6ca8
begin
	md"""# HS94_plotmap.jl

	### 


	Here we read MITgcm output for temperature and interpolate it via **`MeshArrays.jl`** to generate something like this:
	
	![plot](https://user-images.githubusercontent.com/20276764/113531401-b1780d00-9596-11eb-8e96-990cf9533ada.png)
	"""
end

# ╔═╡ 56a76f42-7d83-4600-a9a2-2b675b6efcaa
begin
	#list of output files (1 per time record)
	ff=readdir(pth); fil="T.0000"
	ff=ff[findall(occursin.(fil,ff).*occursin.(".data",ff))]	
	nt=length(ff)
	
	#function used to plot one time record
	function myplot(fil)
	    T=read(pth*fil,MeshArray(γ,Float64))
	    TT=Interpolate(T,IntFac...)
	    contourf(vec(lon[:,1]),vec(lat[1,:]),TT,clims=(260.,320.))
	end
	
	##
	
	f1=myplot(ff[end])	
	🏁
end

# ╔═╡ ee0e6f28-aa26-48de-8ddd-8bb2d1102ee9
begin
	dt=6
	anim = @animate for i ∈ 1:dt:nt
	    myplot(ff[i])
	end
	pp=tempdir()*"/"
	gif(anim,pp*"hs94.cs.gif", fps = 8)
end

# ╔═╡ Cell order:
# ╟─19095067-33f5-495f-bc4d-ee6dacbf6ca8
# ╟─3668f786-9597-11eb-01a1-87d34b49eef9
# ╟─56a76f42-7d83-4600-a9a2-2b675b6efcaa
# ╟─ee0e6f28-aa26-48de-8ddd-8bb2d1102ee9

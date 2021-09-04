### A Pluto.jl notebook ###
# v0.15.1

using Markdown
using InteractiveUtils

# ╔═╡ 3668f786-9597-11eb-01a1-87d34b49eef9
begin 
	using Pkg

	Pkg.activate(mktempdir())
    Pkg.add([
        Pkg.PackageSpec(name="MITgcmTools", rev="master"),
        Pkg.PackageSpec(name="MeshArrays", rev="master"),
        Pkg.PackageSpec("Plots"),
    ])
	
	#packages for I/O, interpolation, etc	
	using MITgcmTools, MeshArrays, Plots
	PICKUP_hs94_download()
		
	🏁 = "🏁"
	"Downloads and pacakges : complete."
end

# ╔═╡ 19095067-33f5-495f-bc4d-ee6dacbf6ca8
begin
	md"""# HS94_plotmap.jl

	### 


	Here we read MITgcm output for temperature and interpolate it via **`MeshArrays.jl`** to generate something like this:
	
	![plot](https://user-images.githubusercontent.com/20276764/113531401-b1780d00-9596-11eb-8e96-990cf9533ada.png)
	"""
end

# ╔═╡ fa968801-6892-4475-9b27-56472ca611b4
function modify_params_HS94(myexp)
	par_path=joinpath(myexp.folder,string(myexp.ID),"log","tracked_parameters")

	fil=joinpath(par_path,"data")
	nml=read(fil,MITgcm_namelist())

	nml.params[1][:useSingleCpuIO]=true
	
	nml.params[3][:nIter0]=43200
	nml.params[3][:nTimeSteps]=720
	nml.params[3][:monitorFreq]= 21600.0

	write(fil,nml)
	#git_log_fil(myexp,fil,"update parameter file : "*split(fil,"/")[end])

	fil=joinpath(par_path,"data.pkg")
	nml=read(fil,MITgcm_namelist())

	nml.params[1][:useDiagnostics]=false
	nml.params[1][:useMNC]=false

	write(fil,nml)
	#git_log_fil(myexp,fil,"update parameter file : "*split(fil,"/")[end])

end	

# ╔═╡ aad7e042-ba39-4518-8f3e-da59b77c13cb
begin
	myexp=verification_experiments("hs94.cs-32x32x5")
	
	setup(myexp)

	modify_params_HS94(myexp)
	
	pth_run=joinpath(myexp.folder,string(myexp.ID),"run")

	fil1="pickup.0000043200.data"
	!isfile(joinpath(pth_run,fil1)) ? cp(joinpath(PICKUP_hs94_path,fil1),joinpath(pth_run,fil1)) : nothing
	fil2="pickup.0000043200.meta"
	!isfile(joinpath(pth_run,fil2)) ? cp(joinpath(PICKUP_hs94_path,fil2),joinpath(pth_run,fil2)) : nothing

	#readdir(joinpath(myexp.folder,string(myexp.ID),"log"))
	step1=🏁
	
	isfile(joinpath(MITgcm_path[1],"verification",myexp.configuration,"build","mitgcmuv"))
end

# ╔═╡ 207e4c15-7818-4dc3-a048-1dd36ba5a73e
myexp

# ╔═╡ 0aa37844-b4b9-4f58-adf7-15ae9a490993
begin
	step1==🏁
	MITgcm_launch(myexp)
	step2=🏁
	isfile(joinpath(pth_run,"output.txt"))
end

# ╔═╡ b77f7ff2-da7e-41b3-b3f6-3819b09cd33c
begin
	step2==🏁
	
	isfile(joinpath(pth_run,"output.txt")) ? sc=scan_rundir(pth_run) : sc=(completed=false,)


	#copy files to known location for subsequent notebooks (Makie, particles, etc)
	function cp_run_dir()
		p2=joinpath(PICKUP_hs94_path,"run")
		tst = sc.completed&(!isdir(p2))
		tst ? run(`cp -pr $pth_run $p2`) : nothing
		isdir(p2)
	end
	cp_run_dir()

	## read grid variables (for interpolation)
	Γ = GridLoad_mdsio(myexp)
	
	## setup interpolation (for plotting)
	lon=[i for i=-179.5:1.0:179.5, j=-89.5:1.0:89.5]
	lat=[j for i=-179.5:1.0:179.5, j=-89.5:1.0:89.5]
	(f,i,j,w,_,_,_)=InterpolationFactors(Γ,vec(lon),vec(lat))
	IntFac=(f,i,j,w)
	step3=🏁
end

# ╔═╡ 56a76f42-7d83-4600-a9a2-2b675b6efcaa
begin
	step3==🏁


	#list of output files (1 per time record)
	ff=readdir(pth_run); fil="T.0000"
	ff=ff[findall(occursin.(fil,ff).*occursin.(".data",ff))]	
	nt=length(ff)
	γ=Γ.XC.grid
		
	step4=🏁
end

# ╔═╡ 964108cd-4fe3-4bb8-85db-500618e21af7
#function used to plot one time record
function myplot(fil)
	T=read(joinpath(pth_run,fil),MeshArray(γ,Float64))
	TT=Interpolate(T,IntFac...)
	contourf(vec(lon[:,1]),vec(lat[1,:]),TT,clims=(260.,320.))
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

# ╔═╡ 0ca84f4e-f5bf-40d0-bf46-7a0e70b7aded
readdir(pth_run)

# ╔═╡ ca299148-6aa8-4379-88e3-c4500ddc779f
stdout=readlines(joinpath(pth_run,"output.txt"))

# ╔═╡ b1ca8b16-7b63-470b-90d0-6ea41eeb5211
sc

# ╔═╡ 37294d8a-a70e-419a-a60b-11d09930c6b0
readdir(PICKUP_hs94_path)

# ╔═╡ Cell order:
# ╟─19095067-33f5-495f-bc4d-ee6dacbf6ca8
# ╟─207e4c15-7818-4dc3-a048-1dd36ba5a73e
# ╟─ee0e6f28-aa26-48de-8ddd-8bb2d1102ee9
# ╠═aad7e042-ba39-4518-8f3e-da59b77c13cb
# ╠═0aa37844-b4b9-4f58-adf7-15ae9a490993
# ╠═b77f7ff2-da7e-41b3-b3f6-3819b09cd33c
# ╟─56a76f42-7d83-4600-a9a2-2b675b6efcaa
# ╟─3668f786-9597-11eb-01a1-87d34b49eef9
# ╟─964108cd-4fe3-4bb8-85db-500618e21af7
# ╟─fa968801-6892-4475-9b27-56472ca611b4
# ╠═0ca84f4e-f5bf-40d0-bf46-7a0e70b7aded
# ╠═ca299148-6aa8-4379-88e3-c4500ddc779f
# ╠═b1ca8b16-7b63-470b-90d0-6ea41eeb5211
# ╠═37294d8a-a70e-419a-a60b-11d09930c6b0

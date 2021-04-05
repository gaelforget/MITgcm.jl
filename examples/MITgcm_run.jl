### A Pluto.jl notebook ###
# v0.14.0

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
	using MITgcmTools, PlutoUI, Printf
	exps=verification_experiments()
	🏁 = "🏁"
	md"""## Select model configuration:
	
	_Note: this will trigger the `cleanup`, `compile`, `run` sequence below_
	"""
end

# ╔═╡ 6ef93b0e-859f-11eb-1b3b-d76b26d678dc
begin
	imgA="https://user-images.githubusercontent.com/20276764/111042787-12377e00-840d-11eb-8ddb-64cc1cfd57fd.png"
	imgB="https://user-images.githubusercontent.com/20276764/97648227-970b9780-1a2a-11eb-81c4-65ec2c87efc6.png"
	md"""# run_MITgcm.jl

	###

	Here we use [MITgcm](https://mitgcm.readthedocs.io/en/latest/?badge=latest) interactively via [MITgcmTools.jl](https://gaelforget.github.io/MITgcmTools.jl/dev/) to generate something like the result shown below. 
	
	###
	
	This includes compiling and running the model via the simple interface defined in [ClimateModels.jl](https://github.com/gaelforget/ClimateModels.jl).
	
	$(Resource(imgA, :width => 240))
	
	### 
	
	$(Resource(imgB, :width => 120))
	"""
end

# ╔═╡ a28f7354-84eb-11eb-1830-1f401bf2db97
@bind myexp Select([exps[i].configuration for i in 1:length(exps)],default="advect_xy")

# ╔═╡ 7fa8a460-89d4-11eb-19bb-bbacdd32719a
begin
	iexp2=findall([exps[i].configuration==myexp for i in 1:length(exps)])[1]
	exps[iexp2]	
end

# ╔═╡ f91c3396-84ef-11eb-2665-cfa350d38737
begin
	iexp=findall([exps[i].configuration==myexp for i in 1:length(exps)])[1]
	TextField((80, 8), "name = $myexp\n\nbuild  = $(exps[iexp].options) \n\nrun    = $(exps[iexp].inputs)")
end

# ╔═╡ d90039c4-85a1-11eb-0d82-77db4decaa6e
md"""## Trigger individual operations:

_Note: letting each operation complete before triggering another one may be best_

Selected model configuration : **$myexp**
"""

# ╔═╡ 8569269c-859c-11eb-1ab1-2d874dfa741b
@bind do_cleanup Button("Clean up subfolders")

# ╔═╡ f008ccaa-859c-11eb-1188-114843d333e6
let
	do_cleanup
	clean(exps[iexp])
	🏁
end

# ╔═╡ 76291182-86d1-11eb-1524-73dc02ca7b64
@bind do_build Button("Build mitgcmuv")

# ╔═╡ 848241fe-86d1-11eb-3b30-b94aa0b4431d
let
	do_build
	build(exps[iexp])
	🏁
end

# ╔═╡ 11b024ac-86d1-11eb-1db9-47a5e41398e3
@bind do_link Button("Link input files to run/")

# ╔═╡ 31829f08-86d1-11eb-3e26-dfae038b4c01
let
	do_link
	setup(exps[iexp])
	🏁
end

# ╔═╡ 5d826e4c-859d-11eb-133d-859c3abe3ebe
@bind do_run Button("Run mitgcmuv in run/")

# ╔═╡ 550d996a-859d-11eb-34bf-717389fbf809
let
	do_run
	MITgcm_launch(exps[iexp])
	🏁
end

# ╔═╡ a04c1cd6-3b9e-4e69-b986-c863b120bb0b
begin
	rundir=joinpath(exps[iexp].folder,string(exps[iexp].ID),"run")
	readdir(rundir)
end

# ╔═╡ Cell order:
# ╟─6ef93b0e-859f-11eb-1b3b-d76b26d678dc
# ╟─8cf4d8ca-84eb-11eb-22d2-255ce7237090
# ╟─a28f7354-84eb-11eb-1830-1f401bf2db97
# ╟─7fa8a460-89d4-11eb-19bb-bbacdd32719a
# ╟─f91c3396-84ef-11eb-2665-cfa350d38737
# ╟─d90039c4-85a1-11eb-0d82-77db4decaa6e
# ╟─8569269c-859c-11eb-1ab1-2d874dfa741b
# ╟─f008ccaa-859c-11eb-1188-114843d333e6
# ╟─76291182-86d1-11eb-1524-73dc02ca7b64
# ╟─848241fe-86d1-11eb-3b30-b94aa0b4431d
# ╟─11b024ac-86d1-11eb-1db9-47a5e41398e3
# ╟─31829f08-86d1-11eb-3e26-dfae038b4c01
# ╟─5d826e4c-859d-11eb-133d-859c3abe3ebe
# ╟─550d996a-859d-11eb-34bf-717389fbf809
# ╟─a04c1cd6-3b9e-4e69-b986-c863b120bb0b

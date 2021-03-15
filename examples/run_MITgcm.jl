### A Pluto.jl notebook ###
# v0.12.21

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


	Here we use MITgcm interactivetly to generate something like this:
	
	$(Resource(imgA, :width => 240))
	
	### 
	
	$(Resource(imgB, :width => 120))
	"""
end

# ╔═╡ a28f7354-84eb-11eb-1830-1f401bf2db97
@bind myexp Select([exps[i].name for i in 1:length(exps)],default="advect_xy")

# ╔═╡ f91c3396-84ef-11eb-2665-cfa350d38737
begin
	iexp=findall([exps[i].name==myexp for i in 1:length(exps)])[1]
	TextField((80, 8), "name = $(exps[iexp].name)\n\nbuild  = $(exps[iexp].build) \n\nrun    = $(exps[iexp].run)")
end

# ╔═╡ d90039c4-85a1-11eb-0d82-77db4decaa6e
md"""## Trigger individual operations:

_Note: letting each operation complete before triggering another one may be best_
"""

# ╔═╡ 8569269c-859c-11eb-1ab1-2d874dfa741b
@bind do_cleanup Button("Clean up $(exps[iexp].name)   (incl. subfolders)")

# ╔═╡ f008ccaa-859c-11eb-1188-114843d333e6
let
	do_cleanup
	MITgcm_cleanup(exps[iexp].name)
	🏁
end

# ╔═╡ 388d23a2-859d-11eb-0d5b-c728aa6b1aa6
@bind do_compile Button("Compile $(exps[iexp].name)   (build/mitgcmuv)")

# ╔═╡ 3b94cc6a-859d-11eb-2ae5-cbd79424e009
let
	do_compile
	MITgcm_compile(exps[iexp].name)
	🏁
end

# ╔═╡ 5d826e4c-859d-11eb-133d-859c3abe3ebe
@bind do_run Button("Run $(exps[iexp].name)   (all experiments)")

# ╔═╡ 550d996a-859d-11eb-34bf-717389fbf809
let
	do_run
	MITgcm_run(exps[iexp].name)
	🏁
end

# ╔═╡ Cell order:
# ╟─6ef93b0e-859f-11eb-1b3b-d76b26d678dc
# ╟─8cf4d8ca-84eb-11eb-22d2-255ce7237090
# ╟─a28f7354-84eb-11eb-1830-1f401bf2db97
# ╟─f91c3396-84ef-11eb-2665-cfa350d38737
# ╟─d90039c4-85a1-11eb-0d82-77db4decaa6e
# ╟─8569269c-859c-11eb-1ab1-2d874dfa741b
# ╟─f008ccaa-859c-11eb-1188-114843d333e6
# ╟─388d23a2-859d-11eb-0d5b-c728aa6b1aa6
# ╟─3b94cc6a-859d-11eb-2ae5-cbd79424e009
# ╟─5d826e4c-859d-11eb-133d-859c3abe3ebe
# ╟─550d996a-859d-11eb-34bf-717389fbf809

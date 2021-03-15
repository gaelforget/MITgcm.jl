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
end

# ╔═╡ a28f7354-84eb-11eb-1830-1f401bf2db97
@bind myexp Select([exps[i].name for i in 1:length(exps)],default="advect_cs")

# ╔═╡ f91c3396-84ef-11eb-2665-cfa350d38737
begin
	iexp=findall([exps[i].name==myexp for i in 1:length(exps)])[1]
	TextField((80, 8), "name = $(exps[iexp].name)\n\nbuild  = $(exps[iexp].build) \n\nrun    = $(exps[iexp].run)")
end

# ╔═╡ 02a95138-8524-11eb-0bce-1f899f85ee47
begin
	do_cleanup=false
	do_compile=false
	do_run=false
	(exps[iexp].name,do_cleanup,do_compile,do_run)
end

# ╔═╡ df2b2c8e-851d-11eb-0602-2ddd06ab5d72
begin
	do_cleanup ? MITgcm_cleanup(exps[iexp].name) : nothing
	do_compile ? MITgcm_compile(exps[iexp].name) : nothing
	do_run ? MITgcm_run(exps[iexp].name) : nothing
	(exps[iexp].name,do_cleanup,do_compile,do_run)
end

# ╔═╡ Cell order:
# ╟─a28f7354-84eb-11eb-1830-1f401bf2db97
# ╟─f91c3396-84ef-11eb-2665-cfa350d38737
# ╟─02a95138-8524-11eb-0bce-1f899f85ee47
# ╠═df2b2c8e-851d-11eb-0602-2ddd06ab5d72
# ╟─8cf4d8ca-84eb-11eb-22d2-255ce7237090

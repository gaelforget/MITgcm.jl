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

# ╔═╡ f588eaba-84ef-11eb-0755-bf1b85b2b561
begin
	imgA="https://user-images.githubusercontent.com/20276764/111042787-12377e00-840d-11eb-8ddb-64cc1cfd57fd.png"
	imgB="https://user-images.githubusercontent.com/20276764/97648227-970b9780-1a2a-11eb-81c4-65ec2c87efc6.png"
	md"""# monitor_run.jl

	### 


	Here we use scan an MITgcm run folder interactivetly to generate something like this:
	
	$(Resource(imgA, :width => 240))
	
	### 
	
	$(Resource(imgB, :width => 120))
	"""
end

# ╔═╡ 98b6621c-85ab-11eb-29d1-af0433598c6a
	md"""## Select model configuration:
	
	_Note: this will update the multiple-choices menu sequence below_
	"""

# ╔═╡ a28f7354-84eb-11eb-1830-1f401bf2db97
@bind myexp Select([exps[i].name for i in 1:length(exps)],default="advect_xy")

# ╔═╡ f91c3396-84ef-11eb-2665-cfa350d38737
begin
	iexp=findall([exps[i].name==myexp for i in 1:length(exps)])[1]
	TextField((100, 8), "name = $(exps[iexp].name)\n\nbuild  = $(exps[iexp].options) \n\nrun    = $(exps[iexp].parameters)")
end

# ╔═╡ f051e094-85ab-11eb-22d4-5bd61ac572a1
md"""## Select a namelist and parameter group

_Note: `data` and `PARM01`, e.g., should be found in any model run directory,_ **once the model has been run for that configuration**. _To run the model, if needed, please refer to the `run_MITgcm.jl` _notebook._

Model configuration is **$(exps[iexp].name)**; let's take a deeper look into its parameters.
"""

# ╔═╡ d7f2c656-8512-11eb-2fdf-47a3e57a55e6
begin
#    lst=readdir(pth)
#    tmp=[isfile(joinpath(pth,i,"code","packages.conf")) for i in lst]
#    lst=lst[findall(tmp)]
	
	pth=joinpath(MITgcm_path,"verification",exps[iexp].name,"run")
	function list_namelist_files(pth)
		tmpA=readdir(pth)
		tmpA=tmpA[findall([length(tmpA[i])>3 for i in 1:length(tmpA)])]
		tmpA=tmpA[findall([tmpA[i][1:4]=="data" for i in 1:length(tmpA)])]
	end
	dats=list_namelist_files(pth)
	try
		@bind mydats Select([dats[i] for i in 1:length(dats)])
	catch e
		"Error: could not find any namelist in $(pth)"
	end
end

# ╔═╡ f40e76c4-86d5-11eb-15b0-cd55d6cd1e65
md"""### Appendices"""

# ╔═╡ 348c692e-84fe-11eb-3288-dd0a1dedce90
begin
	fil=joinpath(MITgcm_path,"verification",exps[iexp].name,"run",mydats)
	nml=read(fil,MITgcm_namelist())
	🏁
end

# ╔═╡ ca7bb004-8510-11eb-379f-632c3b40723d
try
	@bind nmlgroup Select(String.(nml.groups))
catch e
	"Error: could not find any namelist in $(pth)"
end

# ╔═╡ 9bdb94da-8510-11eb-01a6-c9a1519baa68
begin
	inml=findall(nml.groups.==Symbol(nmlgroup))[1]
	🏁
end

# ╔═╡ e50726aa-86d3-11eb-0418-fff8fb79ef95
nml.params[inml]

# ╔═╡ Cell order:
# ╟─f588eaba-84ef-11eb-0755-bf1b85b2b561
# ╟─98b6621c-85ab-11eb-29d1-af0433598c6a
# ╟─a28f7354-84eb-11eb-1830-1f401bf2db97
# ╟─f91c3396-84ef-11eb-2665-cfa350d38737
# ╟─f051e094-85ab-11eb-22d4-5bd61ac572a1
# ╟─d7f2c656-8512-11eb-2fdf-47a3e57a55e6
# ╟─ca7bb004-8510-11eb-379f-632c3b40723d
# ╟─e50726aa-86d3-11eb-0418-fff8fb79ef95
# ╟─f40e76c4-86d5-11eb-15b0-cd55d6cd1e65
# ╟─8cf4d8ca-84eb-11eb-22d2-255ce7237090
# ╟─9bdb94da-8510-11eb-01a6-c9a1519baa68
# ╟─348c692e-84fe-11eb-3288-dd0a1dedce90

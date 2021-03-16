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
	TextField((100, 8), "name = $(exps[iexp].name)\n\nbuild  = $(exps[iexp].build) \n\nrun    = $(exps[iexp].run)")
end

# ╔═╡ f051e094-85ab-11eb-22d4-5bd61ac572a1
md"""## Select a namelist and parameter group

_Note: `data` and `PARM01`, e.g., should be found in any model run directory,_ **once the model has been run for that configuration**

_Note: one can use e.g. `run MITgcm.jl` notebook or the `MITgcm run()` function to rerun the various model configurations_
"""

# ╔═╡ 6702b58e-8625-11eb-2373-e58eb4e371bd
exps[iexp].name

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

# ╔═╡ 348c692e-84fe-11eb-3288-dd0a1dedce90
begin
	fil=joinpath(MITgcm_path,"verification",exps[iexp].name,"run",mydats)
	namelist=read_namelist(fil)
	🏁
end

# ╔═╡ ca7bb004-8510-11eb-379f-632c3b40723d
try
	@bind mynamelist Select([String(keys(namelist)[i]) for i in 1:length(namelist)])
catch e
	"Error: could not find any namelist in $(pth)"
end

# ╔═╡ 15746ef0-8617-11eb-1160-5f48a95d94d0
begin
	function save_namelist(fil,namelist)
	fid = open(fil, "w")
	for ii in keys(namelist)
		tmpA=namelist[ii] 
		params=(; zip(keys(tmpA),values(tmpA))...)
			
			txt=fill("",length(params))
			for i in 1:length(params)
				x=params[i]
				y=missing
				isa(x,Bool)&&x==true ? y=".TRUE." : nothing
				isa(x,Bool)&&x==false ? y=".FALSE." : nothing
				
				ismissing(y)&&isa(x,SubString)&&(!occursin('*',x)) ? y="'$x'" : nothing
				ismissing(y) ? y="$x" : nothing
				y[end]==',' ? y=y[1:end-1] : nothing
				txt[i]=y
			end
			
		params=[" $(keys(params)[i]) = $(txt[i]),\n" for i in 1:length(params)]

		write(fid," &$(ii)\n")
		[write(fid,params[i]) for i in 1:length(params)]
		write(fid," &\n")
		write(fid," \n")
	end
	close(fid)
	end
	save_namelist("data.dev",namelist)
	#"saved!"
end

# ╔═╡ 9bdb94da-8510-11eb-01a6-c9a1519baa68
begin
	tmpA=namelist[Symbol(mynamelist)]
	params=(; zip(keys(tmpA),values(tmpA))...)	
	🏁
end

# ╔═╡ 345071c4-8611-11eb-1a91-e914c1f315d5
[(keys(params)[i],values(params)[i]) for i in 1:length(params)]

# ╔═╡ Cell order:
# ╟─f588eaba-84ef-11eb-0755-bf1b85b2b561
# ╟─98b6621c-85ab-11eb-29d1-af0433598c6a
# ╟─a28f7354-84eb-11eb-1830-1f401bf2db97
# ╟─f91c3396-84ef-11eb-2665-cfa350d38737
# ╟─f051e094-85ab-11eb-22d4-5bd61ac572a1
# ╠═6702b58e-8625-11eb-2373-e58eb4e371bd
# ╟─d7f2c656-8512-11eb-2fdf-47a3e57a55e6
# ╟─ca7bb004-8510-11eb-379f-632c3b40723d
# ╟─345071c4-8611-11eb-1a91-e914c1f315d5
# ╠═15746ef0-8617-11eb-1160-5f48a95d94d0
# ╠═8cf4d8ca-84eb-11eb-22d2-255ce7237090
# ╠═9bdb94da-8510-11eb-01a6-c9a1519baa68
# ╠═348c692e-84fe-11eb-3288-dd0a1dedce90

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
("inspect","monitor","plot")

# ╔═╡ a28f7354-84eb-11eb-1830-1f401bf2db97
@bind myexp Select([exps[i].name for i in 1:length(exps)],default="advect_cs")

# ╔═╡ f91c3396-84ef-11eb-2665-cfa350d38737
begin
	iexp=findall([exps[i].name==myexp for i in 1:length(exps)])[1]
	TextField((100, 8), "name = $(exps[iexp].name)\n\nbuild  = $(exps[iexp].build) \n\nrun    = $(exps[iexp].run)")
end

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
	@bind mydats Select([dats[i] for i in 1:length(dats)])
end

# ╔═╡ b57666da-84fd-11eb-11a4-5161e1b5beb6
begin
"""
    read_namelist(fil)

Read a `MITgcm` namelist file, parse it, and return as a NamedTuple
"""
function read_namelist(fil)

    meta = read(fil,String)
    meta = split(meta,"\n")
    meta = meta[findall((!isempty).(meta))]
    meta = meta[findall(first.(meta).!=='#')]
    groups = meta[findall(occursin.('&',meta))]
	groups = [Symbol(groups[1+2*(i-1)][3:end]) for i in 1:Int(length(groups)/2)]
	params = fill(Dict(),length(groups))
		
	for i in 1:length(groups)
		ii=1+findall(occursin.(String(groups[i]),meta))[1]
		i1=ii
		tmp0=Dict()
		while !occursin('&',meta[ii])
			if occursin('=',meta[ii])
				tmp1=split(meta[ii],'=')
				tmp2=split(tmp1[2],',')
				tmp0[Symbol(strip(tmp1[1]))]=strip(tmp2[1])
			else
				println("ignoring line -- likely part of an array ...")
			end
			ii += 1
		end
		params[i]=tmp0			
	end
		
#	params=(; zip(Symbol.(groups),params)...)
#    return meta,groups,params
	return (; zip(Symbol.(groups),params)...),groups,meta
end
	🏁
end

# ╔═╡ 348c692e-84fe-11eb-3288-dd0a1dedce90
begin
	fil=joinpath(MITgcm_path,"verification",exps[iexp].name,"run",mydats)
	namelist,groups,lines=read_namelist(fil)
	🏁
end

# ╔═╡ ca7bb004-8510-11eb-379f-632c3b40723d
@bind mynamelist Select([String(keys(namelist)[i]) for i in 1:length(namelist)])

# ╔═╡ 9bdb94da-8510-11eb-01a6-c9a1519baa68
begin
	tmpA=namelist[Symbol(mynamelist)]
	params=(; zip(keys(tmpA),values(tmpA))...)
	
	tmpB=["$(keys(params)[i]) = $(params[i]) \n" for i in 1:length(params)]
	params_txt=""
	[params_txt=params_txt*tmpB[i] for i in 1:length(tmpB)]	
	
	🏁
end

# ╔═╡ a3392068-8514-11eb-14ab-4b807c5325d3
TextField((40, length(params)+2),params_txt)

# ╔═╡ Cell order:
# ╟─f588eaba-84ef-11eb-0755-bf1b85b2b561
# ╟─a28f7354-84eb-11eb-1830-1f401bf2db97
# ╟─f91c3396-84ef-11eb-2665-cfa350d38737
# ╟─d7f2c656-8512-11eb-2fdf-47a3e57a55e6
# ╟─ca7bb004-8510-11eb-379f-632c3b40723d
# ╟─a3392068-8514-11eb-14ab-4b807c5325d3
# ╟─8cf4d8ca-84eb-11eb-22d2-255ce7237090
# ╟─9bdb94da-8510-11eb-01a6-c9a1519baa68
# ╟─348c692e-84fe-11eb-3288-dd0a1dedce90
# ╟─b57666da-84fd-11eb-11a4-5161e1b5beb6

module MITgcmTools

using Dates, DataFrames, NetCDF, Printf, MeshArrays, SparseArrays, Pkg.Artifacts

include("ReadFiles.jl")
include("FormatConversions.jl")
include("PhysicalOceanography.jl")

export MITgcm_path, MITgcm_cleanup, MITgcm_compile, MITgcm_run
export verification_experiments, testreport, read_namelist, save_namelist
export read_mdsio, read_meta, read_available_diagnostics
export read_bin, read_flt, read_nctiles, findtiles, parse_param
export cube2compact, compact2cube, convert2array, convert2gcmfaces
export SeaWaterDensity, MixedLayerDepth

p=dirname(pathof(MITgcmTools))
artifact_toml = joinpath(p, "../Artifacts.toml")
MITgcm_hash = artifact_hash("MITgcm", artifact_toml)
MITgcm_path = joinpath(artifact_path(MITgcm_hash)*"/","MITgcm-checkpoint67s/")

"""
    testreport(nam::String,ext="")

```
testreport("front_relax");
```
"""
function testreport(nm::String,ext="")
    cd(tempdir())
    c=`$(MITgcm_path)/verification/testreport -t $(MITgcm_path)/verification/$(nm) $ext`
    isempty(ext) ? c=`$(MITgcm_path)/verification/testreport -t $(MITgcm_path)/verification/$(nm)` : nothing
    run(c)
end

"""
    MITgcm_cleanup(nam::String)
"""
MITgcm_cleanup(nam::String) = testreport(nam,"-clean")

"""
    MITgcm_compile(nam::String)
"""
MITgcm_compile(nam::String) = testreport(nam,"-norun")

"""
    MITgcm_run(nam::String)
"""
MITgcm_run(nam::String) = testreport(nam,"-runonly")

"""
    verification_experiments()

```
exps=verification_experiments()
```
"""
function verification_experiments()
    pth=joinpath(MITgcm_path,"verification")
    lst=readdir(pth)
    tmp=[isfile(joinpath(pth,i,"code","packages.conf")) for i in lst]
    tmp2=[isfile(joinpath(pth,i,"code","SIZE.h")) for i in lst]
    lst=lst[findall(tmp.|tmp2)]

    pkg_build=fill(String[],size(lst))
    pkg_run=fill(String[],size(lst))
    for i in 1:length(lst)
        fil=joinpath(pth,lst[i],"code","packages.conf")
        if isfile(fil)
            tmp1=read(fil,String)
            tmp1=split(tmp1,"\n")
            tmp1=tmp1[findall((!isempty).(tmp1))]
            pkg_build[i]=tmp1[findall(first.(tmp1).!=='#')]
        end

        fil=joinpath(pth,lst[i],"input","data.pkg")
        tmp1=read(fil,String)
        tmp1=split(tmp1,"\n")
        tmp1=tmp1[findall((!isempty).(tmp1))]
        tmp1=tmp1[findall(first.(tmp1).!=='#')]
        pkg_run[i]=tmp1[findall([!occursin("&",i) for i in tmp1])]
    end

    [(name=lst[i],build=pkg_build[i],run=pkg_run[i]) for i in 1:length(lst)]
end

"""
    save_namelist(fil)

Save a `MITgcm` namelist file. In the example below, one is read from file, modified, and then saved to a new file using save_namelist.

```
using MITgcmTools
testreport("advect_xy")
fil=joinpath(MITgcm_path,"verification","advect_xy","run","data")
namelist=read_namelist(fil)
save_namelist(fil*"_new",namelist)
```
"""
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
				if isa(x,Array)
                    tmpy=[""]
                    [tmpy[1]*=x[ii]*"," for ii in 1:length(x)]
                    y=tmpy[1]
                end
				ismissing(y)&&isa(x,AbstractString)&&(!occursin('*',x)) ? y="'$x'" : nothing
				ismissing(y) ? y="$x" : nothing
				y[end]==',' ? y=y[1:end-1] : nothing
				txt[i]=y
			end
			
		txtparams=[" $(keys(params)[i]) = $(txt[i]),\n" for i in 1:length(params)]

		write(fid," &$(ii)\n")
		[write(fid,txtparams[i]) for i in 1:length(txtparams)]
		write(fid," &\n")
		write(fid," \n")
	end
	close(fid)
end

#more:
#
#using Plots; include("recipes_plots.jl"); export qwckplot

#deprecated:
#
#include("deprecated.jl"); export prep_MTRX, read_SPM

end # module

# file for checking if all types of mass are conserved 

using CSV
using DataFrames
using DelimitedFiles
using Plots



using Markdown
using InteractiveUtils
using MITgcmTools, ClimateModels, PlutoUI, Printf, Plots
using UUIDs
using NCDatasets
using Pkg

# # setting up python...
# ENV["PYTHON"] = "/opt/anaconda3/envs/eaps-env/bin/python"
# Pkg.build("PyCall")
# using PyCall
# @pyimport xarray

# let
#     import Pkg
#     Pkg.activate(".")
# 	# CHANGE ME - put correct path in 
#     Pkg.add(path="/Users/birdy/Documents/eaps_research/julia stuff/MITgcmTools.jl")
# end

num_nutrients = 20
config_id = "conservation-test-nutrients.$num_nutrients"
needs_setup = true 

if needs_setup
    begin
        using Markdown
        using InteractiveUtils
        using MITgcmTools, ClimateModels, PlutoUI, Printf, Plots
        using UUIDs
    end
    
    MITgcm_path[1] = "/Users/birdy/Documents/eaps_research/darwin3" # CHANGE ME 
    
    begin
        # create config
        config_name = "darwin-single-box"
        # config_id = "conservation-test-" * string(UUIDs.uuid4())
        
        #folder = "/Users/birdy/Documents/eaps_research/darwin3/verification/darwin-single-box/run"
        folder = joinpath(MITgcm_path[1], "verification", config_name, "run")
        config_obj = MITgcm_config(configuration=config_name, ID=config_id, folder=folder)
    
        # setup 
        filexe=joinpath(MITgcm_path[1],"verification",config_name,"build","mitgcmuv")
    
        # TODO: build model here instead of using a pre-built exec
        # check for mitgcmuv executable, if not there, build it
        # ... the `make -j 4` command in ModelSteps>build fails :'(  
        # even though it works fine when i run the commands from the CL
        # WORKAROUND: run the folling from the build dir
        # ../../../tools/genmake2 -mods=../code
        # make depend
        # make -j 4
        if !isfile(filexe)
            println("building...")
            build(config_obj)
            println("done with build")
        end
    
        # rundir=joinpath("/Users/birdy/Documents/eaps_research/darwin3","verification",config_name,"run", string(config_id), "run")
        # filout=joinpath(rundir,"output.txt")
        # filstat=joinpath(rundir,"onestat.txt")
        println("MITgcm_path[1]: ", MITgcm_path[1])
        println("running setup...")
        setup(config_obj)
        println("done with setup")
        # ClimateModels.git_log_prm(config_name)
        # NOTE: creates a new folder each time it runs, so CLEAN OUT EVENTUALLY
    end
    
    println(config_obj)
    println("********************************************************")
    println("* Config ID: ", config_id, " ***** copy this into darwin-run.jl *****")
    println("********************************************************")    
end

##################
# helpers
##################
# change a parameter
function update_param(file_name, group_name, param_name, new_param_value)
    # read the contents of the data file into a namelist 
    data_file = file_name
    fil = joinpath(rundir, data_file)
    nml = read(fil, MITgcm_namelist())

    # which param group do you want to modify?
    nmlgroup = group_name
    group_idx =findall(nml.groups.==Symbol(nmlgroup))[1]
    parms = nml.params[group_idx]

    # what parameter do you want to modify?
    p_name = param_name
    p_value = new_param_value

    # write changed parameter
    # tmptype= haskey(nml.params[group_idx], Symbol(p_name)) ? typeof(nml.params[group_idx][Symbol(p_name)]) : typeof(p_value)
    #nml.params[group_idx][Symbol(p_name)]=parse(tmptype,p_value)
    nml.params[group_idx][Symbol(p_name)] = p_value
    tmpfil=joinpath(rundir,data_file)
    rm(tmpfil)
    write(tmpfil,nml)
    tmpfil=joinpath("tracked_parameters",data_file)
    ClimateModels.git_log_fil(config_obj,tmpfil,"updated $(p_name) parameter file in $(data_file) to $(p_value)")
end
##################
# END helpers
##################


##################
# TODO: copy and paste in the correct config_id
# (from the output of darwin-setup)
##################
MITgcm_path[1] = "/Users/birdy/Documents/eaps_research/darwin3" # CHANGE ME 
# config_id = "conservation-test-2.5" # CHANGE ME

# reload the config 
config_name = "darwin-single-box"
folder = joinpath(MITgcm_path[1], "verification/darwin-single-box/run")
config_obj = MITgcm_config(configuration=config_name, ID=config_id, folder=folder)
rundir = joinpath(folder, config_id, "run")

##################
# Modify runtime parameters here
# file > group > parameter
##################

# timing 
update_param("data", "PARM03", "nenditer", 2880) # end after 1 years

# NOTE: values taken from large Darwin model run (lat=20.5, lon=202.5)
# load up seed nc file 
seed_file = "/Users/birdy/Documents/eaps_research/gcm_analysis/gcm_data/jan_7_2022/3d.0000000000.nc"
ds = Dataset(seed_file)

# selection using indices
x = 203
y = 121
z = 1
#t = 21 # APRIL 
t = 50 # OCTOBER

# # NO BIOLOGY
# # we want the first 19 tracers

# # DIC
# tracer_name = "TRAC01"
# new_value = ds[tracer_name][x, y, z, t]
# update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,1)", new_value)

# # NO3
# tracer_name = "TRAC02"
# new_value = ds[tracer_name][x, y, z, t]
# update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,2)", new_value)

for i = 1:num_nutrients
    tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
    tracer_name = "TRAC"*tracer_id 
    new_value = ds[tracer_name][x, y, z, t]
    update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,$i)", new_value)
end
# # set biology to 0
# for i = 21:70
#     tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
#     tracer_name = "TRAC"*tracer_id 
#     new_value = 0
#     update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,$i)", new_value)
# end
# WITH PRO and SYN
# we want the first 21 tracers
# for i = 1:20
#     tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
#     tracer_name = "TRAC"*tracer_id 
#     new_value = ds[tracer_name][x, y, z, t]*10
#     update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,$i)", new_value)
# end

# for i = 21:22
#     tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
#     tracer_name = "TRAC"*tracer_id 
#     new_value = ds[tracer_name][x, y, z, t]
#     update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,$i)", new_value)
# end

# # with pro's predator 
# pro_pred = ds["TRAC53"][x, y, z, t]
# update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,53)", pro_pred/10)

# # with syns's predator 
# pro_pred = ds["TRAC54"][x, y, z, t]
# update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,54)", pro_pred/10)

# # with hetero bact
# het = ds["TRAC69"][x, y, z, t]
# update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,69)", het)

# # WITH EVERYTHING except diazotrophs 
# for i = 1:29
#     tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
#     tracer_name = "TRAC"*tracer_id 
#     new_value = ds[tracer_name][x, y, z, t]
#     update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,$i)", new_value)
# end
# for i = 35:70
#     tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
#     tracer_name = "TRAC"*tracer_id 
#     new_value = ds[tracer_name][x, y, z, t]
#     update_param("data.ptracers", "PTRACERS_PARM01", "PTRACERS_ref( :,$i)", new_value)
# end

# temperature = 22.65576 # TODO: not actually taken from darwin data
# salinity = 33.23 # TODO: not actually taken from darwin data
# update_param("data", "PARM01", "tRef", temperature)
# update_param("data", "PARM01", "sRef", salinity)

# TODO: save file to output dir with info about runtime params


##################
# run model
##################
println("launching...")
t = @elapsed begin
    MITgcm_launch(config_obj)
end
println("run completed")
println("time elapse: ", t, " seconds")
println()
println("Output in directory $rundir, most recent ecco folder ")
# TODO: print out the subfolder (i.e. "ecco_gud_DATE_0001")




# run the conscheck script on each darwin_cons file 
# example: python /Users/birdy/Documents/eaps_research/darwin3/tools/darwin/conscheck darwin_cons_A.txt > c_conscheck_output.txt

darwin_dir = "/Users/birdy/Documents/eaps_research/darwin3"
# config_id = "conservation-test-2.5"
cd("/Users/birdy/Documents/eaps_research/darwin3/verification/darwin-single-box/run/$config_id/run")

elts = ["A", "C", "Fe", "N", "O", "P", "Si"]
for elt in elts
    try
        run(pipeline(`python /Users/birdy/Documents/eaps_research/darwin3/tools/darwin/conscheck darwin_cons_$elt.txt`,
        "conscheck_output_$elt.txt"))
        println(elt, " done")
    catch e
        println(elt, " failed with error $e")
    end
end 

# Plot the output of conscheck 

savefigs = false
# place to save plots to 
outdir = dirname(Base.source_path())*"/conservation_graphs/"
folder = "/Users/birdy/Documents/eaps_research/darwin3/verification/darwin-single-box/run"
rundir = joinpath(folder, config_id, "run")


dfs = Vector{DataFrame}()
df_elts = Vector{String}()
for elt in elts
    fil = "conscheck_output_$elt.txt"
    path = joinpath(rundir, fil)
    #df = CSV.read(joinpath(rundir, fil), DataFrame; delim="\t")
    if elt == "P" || elt == "Si"
        println("skipped $elt")
        continue
    end
    data, header = readdlm(path, header=true)
    println(size(data))
    trim_header = header[2:end]
    trim_data = data[:, 1:5] #TODO, should be end-1
    df = DataFrame(trim_data, vec(trim_header))
    append!(dfs, [df])
    append!(df_elts, [elt])
    println(elt, " done")
end

for (df, elt) in collect(zip(dfs, df_elts))
    p = plot(df.tot, title="total $elt over time with first $num_nutrients tracers")
    display(p)
end
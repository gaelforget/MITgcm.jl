# let
#     import Pkg
#     Pkg.activate(".")
# 	# CHANGE ME - put correct path in 
#     Pkg.add(path="/Users/birdy/Documents/eaps_research/julia stuff/MITgcmTools.jl")
# end

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
	config_id = "conservation-test-" * string(UUIDs.uuid4())
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
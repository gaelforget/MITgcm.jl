# let
#     import Pkg
#     Pkg.activate(".")
#     Pkg.add(path="/Users/birdy/Documents/eaps_research/julia stuff/MITgcmTools.jl")
# end

begin
	using Markdown
	using InteractiveUtils
	using MITgcmTools, ClimateModels, PlutoUI, Printf, Plots
	using UUIDs
end

begin
	# create config
	config_name = "darwin-single-box"
	config_id = UUIDs.uuid4()
    # TODO: update folder path
	folder = "/Users/birdy/Documents/eaps_research/darwin3/verification/darwin-single-box/run"
	config_obj = MITgcm_config(configuration=config_name, ID=config_id, folder=folder)

	# setup 
	filexe=joinpath(MITgcm_path[1],"verification",config_name,"build","mitgcmuv")
	# TODO: build model here instead of using a pre-built exec
	#build(config_obj)
    rundir=joinpath("/Users/birdy/Documents/eaps_research/darwin3","verification",config_name,"run", string(config_id), "run")
	filout=joinpath(rundir,"output.txt")
	filstat=joinpath(rundir,"onestat.txt")
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
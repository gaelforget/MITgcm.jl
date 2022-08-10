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
#config_id = "conservation-test-nutrients.$num_nutrients"
config_id = "no_scav_20yrs_less_nutrients"
needs_setup = false 
needs_run = false

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

if needs_run
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
end



# run the conscheck script on each darwin_cons file 
# example: python /Users/birdy/Documents/eaps_research/darwin3/tools/darwin/conscheck darwin_cons_A.txt > c_conscheck_output.txt

darwin_dir = "/Users/birdy/Documents/eaps_research/darwin3"
# config_id = "conservation-test-2.5"
cd("/Users/birdy/Documents/eaps_research/darwin3/verification/darwin-single-box/run/$config_id/run")

elts = ["P", "A", "C", "Fe", "N", "O", "Si"]
for elt in elts
    try
        run(pipeline(`python /Users/birdy/Documents/eaps_research/darwin3/tools/darwin/conscheck darwin_cons_$elt.txt 0`,
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

# load up conscheck data files 
conscheck_dfs = Vector{DataFrame}()
df_elts = Vector{String}()
for elt in elts
    fil = "conscheck_output_$elt.txt"
    path = joinpath(rundir, fil)
    data, header = readdlm(path, header=true)
    println(size(data))
    trim_header = header[2:end]
    trim_data = data[:, 1:5] #TODO, should be end-1
    df = DataFrame(trim_data, vec(trim_header))
    append!(conscheck_dfs, [df])
    append!(df_elts, [elt])
    println(elt, " conscheck done loading")
end

# plot data from conscheck for each element 
plots = []
for (df, elt) in collect(zip(conscheck_dfs, df_elts))
    # p = plot(df.tot, title="total $elt over time with first $num_nutrients tracers")
    p = plot(df.tot, title="total $elt", legend=false, yaxis=(formatter=y->string(round(y; digits=3))))
    #display(p)
    append!(plots, [p])
    tot_max = maximum(df.tot)
    tot_min = minimum(df.tot)
    tot_diff = tot_max - tot_min
    println("Conscheck total for $elt ranges from $tot_min to $tot_max, a difference of $tot_diff")
    # p2 = plot(df.tot - df.diff, title="total-diff $elt over time with first $num_nutrients tracers")
    # display(p2)
end


layout1big2small = @layout[
        a [
            grid(1,1)
            b  ]
    ]

conscheck_total_layout_7 = @layout [
                a   
                [grid(3,2)
                            ]
            ]
# display(plot(plots, layout=layout1big2small))

display(plot(plots..., layout=conscheck_total_layout_7, size=(600,800), plot_title="Conscheck Output"))



############################ Plot just P ############################
elt = "P"
# load up darwin_cons
fil = "darwin_cons_$elt.txt"
path = joinpath(rundir, fil)
data, header = readdlm(path, header=true)
println(size(data))
trim_header = header[2:end]
trim_data = data[:, 1:6] #TODO, should be end-1
p_cons_df = DataFrame(trim_data, vec(trim_header))
println(elt, " darwin_cons done loading")

p_max = maximum(p_cons_df.tot)
p_min = minimum(p_cons_df.tot)
p_diff = p_max - p_min
print("P ranges from $p_min to $p_max, a difference of $p_diff")

# plot p total and losses 
p1 = plot(p_cons_df.tot, title="total $elt")
p2 = plot(p_cons_df.botsnk, title="$elt bottom sinking")
p3 = plot(p_cons_df.EvPrRn, title="$elt EvPrRn")
l = @layout [
           a [grid(1,1)
                    b  ]
       ]
p_plots = plot(p1, p2, p3, layout=l)
display(p_plots)


############################ Plot just A ############################
elt = "A"
# load up darwin_cons
fil = "darwin_cons_$elt.txt"
path = joinpath(rundir, fil)
data, header = readdlm(path, header=true)
println(size(data))
trim_header = header[2:end]
trim_data = data[:, 1:size(data)[2]-1] #TODO, should be end-1
a_cons_df = DataFrame(trim_data, vec(trim_header))
println(elt, " darwin_cons done loading")

a_max = maximum(a_cons_df.tot)
a_min = minimum(a_cons_df.tot)
a_diff = a_max - a_min
print("$elt ranges from $a_min to $a_max, a difference of $a_diff")

# plot p total and losses 
p1 = plot(a_cons_df.tot, title="total $elt")
p2 = plot(a_cons_df.sfcflx, title="$elt surface flux")
p3 = plot(a_cons_df.EvPrRn, title="$elt EvPrRn")
# TODO: alksrc only has a value for every 3rd row (stage 0) 
p4 = plot(a_cons_df.AlkSrc, title="$elt AlkSrc")

l = @layout [
           a [grid(2,1)
                    b  ]
       ]
p_plots = plot(p1, p2, p3, p4, layout=l)
display(p_plots)

############################ Plot just C ############################
elt = "C"
# load up darwin_cons
fil = "darwin_cons_$elt.txt"
path = joinpath(rundir, fil)
data, header = readdlm(path, header=true)
println(size(data))
trim_header = header[2:end]
trim_data = data[:, 1:size(data)[2]-1] #TODO, should be end-1
c_cons_df = DataFrame(trim_data, vec(trim_header))
println(elt, " darwin_cons done loading")

a_max = maximum(c_cons_df.tot)
a_min = minimum(c_cons_df.tot)
a_diff = a_max - a_min
print("$elt ranges from $a_min to $a_max, a difference of $a_diff")

p1 = plot(c_cons_df.tot, title="total $elt")
p2 = plot(c_cons_df.sfcflx, title="$elt sfcflx")
p3 = plot(c_cons_df.EvPrRn, title="$elt EvPrRn")
p4 = plot(c_cons_df.botsnk, title="$elt botsnk")
p5 = plot(c_cons_df.virtflx, title="$elt virtflx")

l = @layout [
                  a [grid(2,2)
                             ]
              ]
p_plots = plot(p1, p2, p3, p4, p5, layout=l)
display(p_plots)

c_stage0_df = filter(:stage => s -> s==0, c_cons_df)
plot(c_stage0_df.sfcflx)
# plot(c_stage0_df.tot - cumsum(c_stage0_df.sfcflx))


############################ Plot just Fe ############################
elt = "Fe"
# load up darwin_cons
fil = "darwin_cons_$elt.txt"
path = joinpath(rundir, fil)
data, header = readdlm(path, header=true)
println(size(data))
trim_header = header[2:end]
trim_data = data[:, 1:size(data)[2]-1] #TODO, should be end-1
fe_cons_df = DataFrame(trim_data, vec(trim_header))
println(elt, " darwin_cons done loading")

fe_stage0_df = filter(:stage => s -> s==0, fe_cons_df)

fe_p0  = plot(fe_stage0_df.tot, title="total $elt stage 0", legend=false)
fe_p2 = plot(fe_stage0_df.minFeLoss, title="$elt minFeLoss stage 0", legend=false)
fe_p1 = plot(fe_stage0_df.scav, title="$elt scav stage 0", legend=false)
fe_p3 = plot(fe_stage0_df.sfcflx, title="$elt sfcflx stage 0", legend=false)
fe_p4 = plot(fe_stage0_df.sedflx, title="$elt sedflx stage 0", legend=false)
fe_p5 = plot(fe_stage0_df.botsnk, title="$elt botsnk stage 0", legend=false)
fe_p6 = plot(fe_stage0_df.EvPrRn, title="$elt EvPrRn stage 0", legend=false)

fe_layout = @layout [
    a{0.3h}
    [
        c b 
        grid(2,2)
     
     ]
]
fe_all = plot(fe_p0, fe_p1, fe_p2, fe_p3, fe_p4, fe_p5, fe_p6, layout=fe_layout, size=(600,900))
display(fe_all)

fe_max = maximum(fe_stage0_df.tot)
fe_min = minimum(fe_stage0_df.tot)
fe_diff = fe_max - fe_min
print("$elt ranges from $fe_min to $fe_max, a difference of $fe_diff")


############################ Plot all elts ############################

# for elt in elts
#     fil = "darwin_cons_$elt.txt"
#     println("working on $elt...")
#     path = joinpath(rundir, fil)
#     data, header = readdlm(path, header=true)
#     println(size(data))
#     trim_header = header[2:end]
#     trim_data = data[:, 1:size(data)[2]-1] #TODO, should be end-1
#     p_cons_df = DataFrame(trim_data, vec(trim_header))
#     println(elt, " darwin_cons done loading")

#     p_max = maximum(p_cons_df.tot)
#     p_min = minimum(p_cons_df.tot)
#     p_diff = p_max - p_min
#     print("$elt ranges from $p_min to $p_max, a difference of $p_diff")

#     # plot p total and losses 
#     p1 = plot(p_cons_df.tot, title="total $elt")
#     p2 = plot(p_cons_df.botsnk, title="$elt bottom sinking")
#     p3 = plot(p_cons_df.EvPrRn, title="$elt EvPrRn")
#     l = @layout [
#             a [grid(1,1)
#                         b  ]
#         ]
#     p_plots = plot(p1, p2, p3, layout=l)
#     display(p_plots)
# end
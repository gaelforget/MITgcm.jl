# file for checking if all types of mass are conserved 

using CSV
using DataFrames
using DelimitedFiles
using Plots

# run the conscheck script on each darwin_cons file 
# example: python /Users/birdy/Documents/eaps_research/darwin3/tools/darwin/conscheck darwin_cons_A.txt > c_conscheck_output.txt

darwin_dir = "/Users/birdy/Documents/eaps_research/darwin3"
cd("/Users/birdy/Documents/eaps_research/darwin3/verification/darwin-single-box/run/conservation-test-7306e661-116f-40ff-8930-9cfeb434081c/run")

elts = ["A", "C", "Fe", "N", "O", "P", "Si"]
# for elt in elts
#     try
#         run(pipeline(`python /Users/birdy/Documents/eaps_research/darwin3/tools/darwin/conscheck darwin_cons_$elt.txt`,
#         "conscheck_output_$elt.txt"))
#         println(elt, " done")
#     catch e
#         println(elt, " failed with error $e")
#     end
# end 

# Plot the output of conscheck 

config_id = "conservation-test-7306e661-116f-40ff-8930-9cfeb434081c" # CHANGE ME
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
    p = plot(df.tot, title="total $elt over time")
    display(p)
end
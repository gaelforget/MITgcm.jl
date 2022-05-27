using Glob # for getting all files
using NCDatasets
# using ClimateBase
using Plots
using DimensionalData


##################
# copy and paste in the correct config_id
# (from the output of darwin-setup)
##################
config_id = "70887403-8813-4702-b2d1-22bd6253d00d" # CHANGE ME
data_folder = "ecco_gud_20220527_0001" # CHANGE ME

# load nc file into ds 
folder = "/Users/birdy/Documents/eaps_research/darwin3/verification/darwin-single-box/run"
rundir = joinpath(folder, config_id, "run")
glob_dir = joinpath(rundir, data_folder)
alldata = glob("3d*.nc", glob_dir)
ds = Dataset(alldata)

##################
# plot results
##################

# nutrients 
# DIC
dic = ds["TRAC01"]
p1 = plot(dic[1, 1, 1, :], title=dic.attrib["description"], legend=false, titlefontsize=12)
# NO3
no3 = ds["TRAC02"]
p2 = plot(no3[1, 1, 1, :], title=no3.attrib["description"], legend=false, titlefontsize=12)
# NO2
no2 = ds["TRAC03"]
p3 = plot(no2[1, 1, 1, :], title=no2.attrib["description"], legend=false, titlefontsize=12)
# PO4
po4 = ds["TRAC05"]
p4 = plot(po4[1, 1, 1, :], title=po4.attrib["description"], legend=false, titlefontsize=12)
# FeT
feT = ds["TRAC06"]
p5 = plot(feT[1, 1, 1, :], title=feT.attrib["description"], legend=false, titlefontsize=12)
# DOC 
doc = ds["TRAC08"]
p6 = plot(doc[1, 1, 1, :], title=doc.attrib["description"], legend=false, titlefontsize=12)

display(plot(p1, p2, p3, p4, p5, p6, plot_title="Nutrients", layout=(3,2), legend=false))


# pro 
pro = ds["TRAC21"]
display(plot(pro[1,1,1,:], title="Prochlorococcus", legend=false))

# sunlight

# temp (sanity check)
temperature_data = glob("tave*.nc", glob_dir)
temp_ds = Dataset(temperature_data)
display(plot(temp_ds["Ttave"][1,1,1,:], title = "Temperature"))
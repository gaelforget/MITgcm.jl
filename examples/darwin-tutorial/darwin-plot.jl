using Glob # for getting all files
using NCDatasets
# using ClimateBase
using Plots
using DimensionalData
using Dates

##################
# CBIOMES 2022 Config ids
##################
just_nutrients = "0c5b1795-f056-4588-8f83-37e13f476634"
with_pro = "1209b496-a5c5-465f-93e7-f68205fa011d"
pro_and_syn = "ad870f4d-600f-4e64-a77d-93c50214db78"
pred_1 = "95cddc5c-16c1-4e3c-84bb-367819dbec6b"
pred_2 = "f9135074-dd36-46e8-a46c-d047b127b9b1"

##################
# copy and paste in the correct config_id
# (from the output of darwin-setup)
##################
config_id = "conservation-test-pro-syn-preds-no-scav" # CHANGE ME
data_folders = glob("ecco_gud*")
data_folder = "ecco_gud_20220802_0001" # CHANGE ME
savefigs = false
# place to save plots to 
outdir = dirname(Base.source_path())*"/poster_graphs/"

# load nc file into ds 
folder = "/Users/birdy/Documents/eaps_research/darwin3/verification/darwin-single-box/run"
rundir = joinpath(folder, config_id, "run")
glob_dir = joinpath(rundir, data_folder)
alldata = glob("3d*.nc", glob_dir)
ds = Dataset(alldata)

##################
# plot results
##################

# DIC and alkalinity
dic = ds["TRAC01"]
dic_plot = plot(dic[1, 1, 1, :], title=dic.attrib["description"], legend=false, titlefontsize=12, ylabel=dic.attrib["units"])
alk = ds["TRAC18"]
alk_plot = plot(alk[1, 1, 1, :], title=alk.attrib["description"], legend=false, titlefontsize=12, ylabel=alk.attrib["units"])
sanity_plot = plot(dic_plot, alk_plot, layout=(2,1))
display(sanity_plot)

# nutrients 

# NH4
nh4 = ds["TRAC04"]
nh4_plot = plot(nh4[1, 1, 1, :], title=nh4.attrib["description"], legend=false, titlefontsize=12, ylabel=nh4.attrib["units"])
# NO3
no3 = ds["TRAC02"]
no3_plot = plot(no3[1, 1, 1, :], title=no3.attrib["description"], legend=false, titlefontsize=12, ylabel=no3.attrib["units"])
# NO2
no2 = ds["TRAC03"]
no2_plot = plot(no2[1, 1, 1, :], title=no2.attrib["description"], legend=false, titlefontsize=12, ylabel=no2.attrib["units"])
# PO4
po4 = ds["TRAC05"]
po4_plot = plot(po4[1, 1, 1, :], title=po4.attrib["description"], legend=false, titlefontsize=12, ylabel=po4.attrib["units"], xlabel="weeks")
# FeT
feT = ds["TRAC06"]
feT_plot = plot(feT[1, 1, 1, :], title=feT.attrib["description"], legend=false, titlefontsize=12, ylabel=feT.attrib["units"])
# DOC 
doc = ds["TRAC08"]
doc_plot = plot(doc[1, 1, 1, :], title=doc.attrib["description"], legend=false, titlefontsize=12, ylabel=doc.attrib["units"])
# DOFe
doFe = ds["TRAC11"]
doFe_plot = plot(doFe[1, 1, 1, :], title=doFe.attrib["description"], legend=false, titlefontsize=12, ylabel=doFe.attrib["units"])
# POFe
poFe = ds["TRAC15"]
poFe_plot = plot(poFe[1, 1, 1, :], title=poFe.attrib["description"], legend=false, titlefontsize=12, xlabel="weeks", ylabel=poFe.attrib["units"])
nutrients_plot = plot(nh4_plot, doc_plot, no3_plot, feT_plot, no2_plot, doFe_plot, po4_plot, poFe_plot, plot_title="Nutrients",
                    layout=(4,2), legend=false, size=(500, 1000))
display(nutrients_plot)

# pro 
pro = ds["TRAC21"]
pro_plot = plot(pro[1,1,1,:], title="Prochlorococcus", legend=false, xlabel="weeks", ylabel=pro.attrib["units"])
plot!(pro_plot, size=(500,500))
display(pro_plot)

# pro pred
pro_pred = ds["TRAC53"]
pro_pred_plot = plot(pro_pred[1,1,1,:], title="Pro Predator", legend=false, xlabel="weeks", ylabel=pro_pred.attrib["units"])
display(pro_pred_plot)

# syn 
syn = ds["TRAC22"]
syn_plot = plot(syn[1,1,1,:], title="Syn", legend=false, xlabel="weeks", ylabel=syn.attrib["units"])
display(syn_plot)

# syn pred
syn_pred = ds["TRAC54"]
syn_pred_plot = plot(syn_pred[1,1,1,:], title="Syn Predator", legend=false, xlabel="weeks", ylabel=syn_pred.attrib["units"])
display(syn_pred_plot)

# syn pred
het = ds["TRAC69"]
het_plot = plot(het[1,1,1,:], title="het", legend=false, xlabel="weeks", ylabel=syn_pred.attrib["units"])
display(het_plot)

# plot all biomass counts (by functional group)

# sum of all nitrogren 
# NO3, NO2, NH4, DON, PON, and biomass
# TODO: add new biomass nitrogren (redfield ratio)
bio_n = ds["TRAC20"] * (16/106)
#bio_n = pro * (16/106)
for i = 21:70 # all biomass creatures
    tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
    tracer_name = "TRAC"*tracer_id 
    global bio_n = bio_n + ds[tracer_name]*(16/106)
end
don = ds["TRAC09"]
pon = ds["TRAC13"]
total_nitrogen = no3 + no2 + nh4 + don + pon + bio_n
n_plot = plot(total_nitrogen[1, 1, 1, :], title="Total Nitrogen", legend=false, xlabel="weeks", ylabel=no2.attrib["units"])
#display(n_plot)

# sum of all iron 
# FeT, DOFe, POFe
# TODO: add biomass iron
doFe = ds["TRAC11"]
poFe = ds["TRAC15"]
total_iron = feT + doFe + poFe
fe_plot = plot(total_iron[1, 1, 1, :], title="Total Iron", legend=false, xlabel="weeks", ylabel=feT.attrib["units"])


# phosphorus 
# PO4 + POP + DOP + biomass*r_pc
r_pc = 0.008333333333333333 # phosphorus carbon ratio 
#bio_p = pro * r_pc
bio_p = ds["TRAC20"] * r_pc
for i = 21:70 # all biomass creatures 
    tracer_id = length(string(i)) < 2 ? "0"*string(i) : string(i)
    tracer_name = "TRAC"*tracer_id 
    global bio_n = bio_n + ds[tracer_name]*r_pc
end
dop = ds["TRAC10"]
pop = ds["TRAC14"]
total_phosphorous = po4 + pop + dop + bio_p
p_plot = plot(total_phosphorous[1, 1, 1, :], title="Total Phosphorus", legend=false, xlabel="weeks", ylabel=po4.attrib["units"])

conservation_plot = plot(n_plot, p_plot, fe_plot, layout=(3,1))
display(conservation_plot)



if savefigs
    savefig(sanity_plot, outdir*"dic-and-alk-"*string(Dates.format(Dates.now(),"YYYYMMddHHMMSS"))*".png")
    savefig(nutrients_plot, outdir*"nutrients-"*string(Dates.format(Dates.now(),"YYYYMMddHHMMSS"))*".png")
    # savefig(n_plot, outdir*"/total-nitrogen-"*string(Dates.format(Dates.now(),"YYYYMMddHHMMSS"))*".png")
    savefig(pro_plot, outdir*"pro-"*string(Dates.format(Dates.now(),"YYYYMMddHHMMSS"))*".png")
    savefig(syn_plot, outdir*"syn-"*string(Dates.format(Dates.now(),"YYYYMMddHHMMSS"))*".png")
    savefig(pro_pred_plot, outdir*"pred-pro-"*string(Dates.format(Dates.now(),"YYYYMMddHHMMSS"))*".png")
    savefig(syn_pred_plot, outdir*"pred-syn-"*string(Dates.format(Dates.now(),"YYYYMMddHHMMSS"))*".png")
    savefig(conservation_plot, outdir*"conservation-"*string(Dates.format(Dates.now(),"YYYYMMddHHMMSS"))*".png")
    savefig(het_plot, outdir*"het-"*string(Dates.format(Dates.now(),"YYYYMMddHHMMSS"))*".png")

end

# sunlight
# load nc file into ds 
folder = "/Users/birdy/Documents/eaps_research/darwin3/verification/darwin-single-box/run"
rundir = joinpath(folder, config_id, "run")
glob_dir = joinpath(rundir, data_folder)
alldata = glob("par*.nc", glob_dir)
par_ds = Dataset(alldata)

plots = Vector{Plots.plot}
p = plot()
p2 = plot()
for variable in collect(keys(par_ds))
    if occursin("PAR", variable)
        #plot on same plot 
        plot!(p, par_ds[variable][1,1,1,:], label=variable, title="PARs")
        #append!(plots, p)
    elseif occursin("E", variable)
        plot!(p2, par_ds[variable][1,1,1,:], label=variable, title="radtran settings")
    end
end
# display(p)
# display(p2)


# growth rate 
folder = "/Users/birdy/Documents/eaps_research/darwin3/verification/darwin-single-box/run"
rundir = joinpath(folder, config_id, "run")
glob_dir = joinpath(rundir, data_folder)
alldata = glob("PC*.nc", glob_dir)
pc_ds = Dataset(alldata)
pc_plot = plot(pc_ds["PC0001"][1, 1, 1, :], title=pc_ds["PC0001"].attrib["description"], legend=false, xlabel="weeks", ylabel=pc_ds["PC0001"].attrib["units"])
# temp (sanity check)
# temperature_data = glob("tave*.nc", glob_dir)
# temp_ds = Dataset(temperature_data)
# display(plot(temp_ds["Ttave"][1,1,1,:], title = "Temperature"))


# plotting conservation 
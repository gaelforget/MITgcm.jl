using Documenter, MITgcm, NetCDF
import PlutoSliderServer
using Plots, CairoMakie

MITgcm.set_environment_variables_to_default()

makedocs(;
    modules=[MITgcm,Base.get_extension(MITgcm, :MITgcmNetCDFExt)],
    format=Documenter.HTML(),
    pages=[
        "Contents" => "index.md",
        "Manual" => ["functionalities.md","functionalities_interface.md",
        "functionalities_configurations.md","functionalities_read.md","functionalities_more.md"],
        "Examples" => "examples.md",
        "Contribute" => "contributing.md",
        "API" => "API.md",
    ],
    warnonly = [:cross_references,:missing_docs],
    repo=Remotes.GitHub("gaelforget", "MITgcm.jl"),
    sitename="MITgcm.jl",
    authors="gaelforget <gforget@mit.edu>",
)

pth = joinpath(@__DIR__, "build","examples")
lst=("HS94_animation.jl","HS94_particles.jl","MITgcm_configurations.jl","MITgcm_run.jl","MITgcm_worklow.jl","HS94_Makie.jl","MITgcm_scan_output.jl")
#lst=[]
for i in lst
    fil_in=joinpath(@__DIR__,"..","examples",i)
    fil_out=joinpath(pth,i[1:end-2]*"html")
    PlutoSliderServer.export_notebook(fil_in)
    mv(fil_in[1:end-2]*"html",fil_out)
    #cp(fil_in[1:end-2]*"html",fil_out)
end

deploydocs(;
    repo="github.com/gaelforget/MITgcm.jl",
)

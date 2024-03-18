using Documenter, MITgcm
import PlutoSliderServer
using Plots, CairoMakie

makedocs(;
    modules=[MITgcm],
    format=Documenter.HTML(),
    pages=[
        "Home" => "index.md",
        "Manual" => "functionalities.md",
        "Examples" => "examples.md",
        "API Reference" => "functionalities_more.md",
#        "Examples" => Any[
#            "Guide " => "examples.md",
#            "Listing" => [map(s -> "generated/$(s[1:end-2])md",lst)...],
#            ],
    ],
    warnonly = [:cross_references,:missing_docs],
    repo="https://github.com/gaelforget/MITgcm.jl/blob/{commit}{path}#L{line}",
    sitename="MITgcm.jl",
    authors="gaelforget <gforget@mit.edu>",
)

pth = joinpath(@__DIR__, "build","examples")
lst=("HS94_animation.jl","HS94_particles.jl","MITgcm_configurations.jl","MITgcm_run.jl","MITgcm_worklow.jl","HS94_Makie.jl","MITgcm_scan_output.jl")
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

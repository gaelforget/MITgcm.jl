using Documenter, MITgcmTools
import PlutoSliderServer

makedocs(;
    modules=[MITgcmTools],
    format=Documenter.HTML(),
    pages=[
        "Home" => "index.md",
        "Manual" => "functionalities.md",
        "Examples" => "examples.md",
#        "Examples" => Any[
#            "Guide " => "examples.md",
#            "Listing" => [map(s -> "generated/$(s[1:end-2])md",lst)...],
#            ],
    ],
    repo="https://github.com/gaelforget/MITgcmTools.jl/blob/{commit}{path}#L{line}",
    sitename="MITgcmTools.jl",
    authors="gaelforget <gforget@mit.edu>",
)

pth = joinpath(@__DIR__, "build","examples")
lst=("HS94_plotmap.jl","HS94_particles.jl","HS94_Makie.jl","MITgcm_configurations.jl","MITgcm_run.jl","MITgcm_worklow.jl","MITgcm_scan_output.jl")
for i in lst
    fil_in=joinpath(@__DIR__,"..","examples",i)
    fil_out=joinpath(pth,i[1:end-2]*"html")
    PlutoSliderServer.export_notebook(fil_in)
    mv(fil_in[1:end-2]*"html",fil_out)
    #cp(fil_in[1:end-2]*"html",fil_out)
end

deploydocs(;
    repo="github.com/gaelforget/MITgcmTools.jl",
)

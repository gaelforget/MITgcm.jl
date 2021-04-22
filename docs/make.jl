using Documenter, MITgcmTools

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
    assets=String[],
)

deploydocs(;
    repo="github.com/gaelforget/MITgcmTools.jl",
)

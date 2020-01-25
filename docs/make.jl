using Documenter, MITgcmTools

makedocs(;
    modules=[MITgcmTools],
    format=Documenter.HTML(),
    pages=[
        "Home" => "index.md",
    ],
    repo="https://github.com/gaelforget/MITgcmTools.jl/blob/{commit}{path}#L{line}",
    sitename="MITgcmTools.jl",
    authors="gaelforget <gforget@mit.edu>",
    assets=String[],
)

deploydocs(;
    repo="github.com/gaelforget/MITgcmTools.jl",
)

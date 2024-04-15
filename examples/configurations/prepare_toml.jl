
using MITgcm, TOML, OrderedCollections

params=read_all_namelists("input"); nam="ECCO4"
#params=read_all_namelists("run"); nam="OCCA2"

P=OrderedDict()
P[:main]=OrderedDict(
    :category=>"custom",
    :name=>nam,
    :version=>"main")

push!(params,(:setup => P))

open(nam*".toml", "w") do io
    TOML.print(io, params)
end


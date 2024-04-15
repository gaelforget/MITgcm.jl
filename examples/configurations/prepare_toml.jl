
using MITgcm, TOML, OrderedCollections

params=MITgcm.read_namelist_files("input"); nam="ECCO4"
#params=MITgcm.read_namelist_files("run"); nam="OCCA2"

P=OrderedDict()
P[:main]=OrderedDict(
    :category=>"custom",
    :name=>nam,
    :version=>"main")

push!(params,(:setup => P))

open(nam*".toml", "w") do io
    TOML.print(io, params)
end


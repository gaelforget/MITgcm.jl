
"""
verification_experiments()

Get list of all `most-standard` configurations of `MITgcm` and return as an Array of `MITgcm_config`

```
exps=verification_experiments()
```
"""
function verification_experiments()
MITgcm_download()
pth=joinpath(MITgcm_path[1],"verification")
lst=readdir(pth)
tmp=[isfile(joinpath(pth,i,"code","packages.conf")) for i in lst]
tmp2=[isfile(joinpath(pth,i,"code","SIZE.h")) for i in lst]
lst=lst[findall(tmp.|tmp2)]

pkg_build=fill(String[],size(lst))
pkg_run=fill(String[],size(lst))
for i in 1:length(lst)
    fil=joinpath(pth,lst[i],"code","packages.conf")
    if isfile(fil)
        tmp1=read(fil,String)
        tmp1=split(tmp1,"\n")
        tmp1=tmp1[findall((!isempty).(tmp1))]
        pkg_build[i]=tmp1[findall(first.(tmp1).!=='#')]
    end

    fil=joinpath(pth,lst[i],"input","data.pkg")
    tmp1=read(fil,String)
    tmp1=split(tmp1,"\n")
    tmp1=tmp1[findall((!isempty).(tmp1))]
    tmp1=tmp1[findall(first.(tmp1).!=='#')]
    pkg_run[i]=tmp1[findall([!occursin("&",i) for i in tmp1])]
end

exps=fill(MITgcm_config(),length(lst))
for i in 1:length(lst)
    ID = UUIDs.uuid4()
    exps[i]=MITgcm_config(configuration=lst[i],ID=ID)
end

return exps
end

function verification_experiments(nam::String)
    exps=verification_experiments()
    iexp=findall([exps[i].configuration==nam for i in 1:length(exps)])[1]
    exps[iexp]
end

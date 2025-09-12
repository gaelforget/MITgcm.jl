
## ECCO reference solutions

"""
    setup_ECCO4!(config::MITgcm_config)

Setup method for ECCO4 and OCCA2 solutions.

```
using MITgcm

params=read_toml(:ECCO4)
folder=joinpath(pwd(),"tmp1")

MC=MITgcm_config(inputs=params,folder=folder)

#providing executable (optional)
#push!(MC.inputs[:setup][:main],(:exe => joinpath(pwd(),"mitgcmuv")))

#providing input folder (optional)
#push!(MC.inputs[:setup][:main],(:input_folder => joinpath(pwd(),"input_folder")))

#modifying run time options (optional)
#MC.inputs[:pkg][:PACKAGES][:useECCO]=false

setup(MC)

#modifying build options (optional)
#MC.inputs[:setup][:build][:options]=MITgcm.build_options_pleiades

build(MC)

launch(MC)
```
"""
function setup_ECCO4!(config::MITgcm_config)
    if !haskey(config.inputs[:setup][:main],:exe)
        println("downloading MITgcm ... ")
        u0="https://github.com/MITgcm/MITgcm"; p0=joinpath(config,"MITgcm")
        @suppress run(`$(git()) clone --depth 1 --branch checkpoint68o $(u0) $(p0)`)
        println("downloading code folder ... ")
        u0="https://github.com/gaelforget/ECCOv4"; p0=joinpath(config,"ECCOv4")
        @suppress run(`$(git()) clone $(u0) $(p0)`)
        p1=joinpath(config,"MITgcm","mysetups")
        p2=joinpath(p1,"ECCOv4")
        mkdir(p1); mv(p0,p2)
        p3=joinpath(p2,"build")
        n3="mitgcmuv"
    else
        p3=dirname(config.inputs[:setup][:main][:exe])
        n3=basename(config.inputs[:setup][:main][:exe])
    end
    P=OrderedDict(:path=>p3,:options=>"-mods=../code -mpi",:rebuild=>false,:exe=>n3)
    push!(config.inputs[:setup],(:build => P))
    #push!(config.inputs[:setup][:main],(:command => "mpirun -np 96 mitgcmuv"))
    push!(config.inputs[:setup][:main],(:command => "qsub submit.csh"))
    println("creating job submission script ...")
    p=joinpath(pathof(config),"run")
    f=joinpath(p,"submit.csh")
    create_script(p,f)

    if haskey(config.inputs[:setup][:main],:input_folder)
        p=config.inputs[:setup][:main][:input_folder]
    else
        p=ECCO4_inputs.download_input_folder(config)
    end
    if isdir(p)
        f=readdir(p)
        pth_run=joinpath(pathof(config),"run")
        [symlink(joinpath(p,f[i]),joinpath(pth_run,f[i])) for i in 1:length(f)]
    end

    return true
end

## ECCO-related functionalities

module ECCO4_inputs

using Dataverse, ClimateModels.DataFrames, ClimateModels.CSV
import MITgcm: MITgcm_config
export get_list, get_files, download_input_folder

##

list0=[
    "doi:10.7910/DVN/PICCRE,documentation,",
    "doi:10.7910/DVN/9WYSZF,surface forcing fields,forcing_baseline2",
    "doi:10.7910/DVN/7XYXSF,model initialization,",
    "doi:10.7910/DVN/GNOREE,in situ T-S profiles,",
    "doi:10.7910/DVN/MEDQWY,sea level anomaly,",
    "doi:10.7910/DVN/L3OQT0,sea surface temperature,",
    "doi:10.7910/DVN/DKXQHO,ice cover fraction,",
    "doi:10.7910/DVN/F8BCRF,surface wind stress,",
    "doi:10.7910/DVN/SYZMUX,bottom pressure,",
    "doi:10.7910/DVN/H2Q1ND,miscellaneous,"];

fil0=joinpath(tempdir(),"Dataverse_list.csv")

##

"""
    get_list(; write_file=false)

Create a list of Dataverse folders for ECCOv4r2. If `write_file=true` then write to file `joinpath(tempdir(),"Dataverse_list.csv")`/
"""
function get_list(; write_file=false)
    df=DataFrame(doi=String[],name=String[],folder=String[])
    for i in list0
        tmp1=split(i,",")
        push!(df,(doi=tmp1[1],name=tmp1[2],folder=tmp1[3]))
    end
    write_file ? CSV.write(fil0, df) : nothing
    df
end

##

"""
    get_list(list1::DataFrame,name::String)

Create a list of Dataverse files from folder with specified `name`.

```
list1=ECCO4_inputs.get_list()
nam1="surface forcing fields"
list2=ECCO4_inputs.get_list(list1,nam1)
```
"""
function get_list(list1::DataFrame,name::String)
    try
        doi=list1[list1.name.==name,:].doi[1]
        Dataverse.file_list(doi)
    catch
        ""
    end
end

"""
    get_files(list1::DataFrame,nam1::String,path1::String)

Create a list of Dataverse files from folder with specified `name`.

```
list1=ECCO4_inputs.get_list()
nam1="model initialization"
ECCO4_inputs.get_files(list1,nam1,tempname())
```
"""
function get_files(list1::DataFrame,nam1::String,path1::String; filenames=[] , dry_run=false)
    !isdir(path1) ? mkdir(path1) : nothing
    list3=get_list(list1,nam1)
    path3=joinpath(path1,list1[list1.name.==nam1,:].folder[1])
    !isdir(path3) ? mkdir(path3) : nothing
    println("Download started ...")
    println("  See : $(path3)")
    to_do_list=setdiff(list3.filename,readdir(path3))
    !isempty(filenames) ? to_do_list=intersect(to_do_list,filenames) : nothing
    #show(to_do_list)
    dry_run ? nothing : [Dataverse.file_download(list3,n,path3) for n in to_do_list]
    println("and now completed!")
    path3
end

"""
    download_input_folder(config::MITgcm_config)
"""
function download_input_folder(config::MITgcm_config; dry_run=false)
    p=joinpath(config,"input_folder")
    mkdir(p)
    list1=ECCO4_inputs.get_list()
    nam1="model initialization"
    ECCO4_inputs.get_files(list1,nam1,p,dry_run=dry_run)
    nam1="surface forcing fields"
    ECCO4_inputs.get_files(list1,nam1,p,dry_run=dry_run)
    if config.inputs[:pkg][:PACKAGES][:useECCO]
        list2=["sea level anomaly","sea surface temperature",
        "ice cover fraction","surface wind stress","bottom pressure"]
        [ECCO4_inputs.get_files(list1,nam2,p,dry_run=dry_run) for nam2 in list2]
    end
    if config.inputs[:pkg][:PACKAGES][:useProfiles]
        nam1="in situ T-S profiles"
        ECCO4_inputs.get_files(list1,nam1,p,dry_run=dry_run)
    end
    p
end

end

module ECCO4_testreport

using Glob, MeshArrays, ClimateModels.DataFrames
using Distributed, SharedArrays, Statistics
import MITgcm: read_mdsio

alt_names=false

"""
    compute(pth0)

```
@everywhere begin
 using MITgcm
 using ECCO4_testreport.SharedArrays
end

report=ECCO4_testreport.compute("run")
```
"""
function compute(pth0; dry_run=false)

 alt_names ? lst=list_diags_files_alt(pth0) : lst=list_diags_files(pth0)
 nt=length(lst.state_2d_set1)

 tave=193:228
 ntave=length(tave)
 
 pth00=(dry_run ? MeshArrays.GRID_LLC90 : pth0)
 RAC=RAC_masked(pth00)
 vol=vol_masked(pth00)
 G,LC=load_llc90_grid(pth00)

 ##

 if !isempty(glob("costfun*",pth0))
   fil0=glob("costfun*",pth0)[1]
   fc=parse_fc(fil0)
   println("Done with fc")
 else
   fc=DataFrame()
 end

 ##

 mH = SharedArray{Float64}(nt)
 mT = SharedArray{Float64}(nt)
 mS = SharedArray{Float64}(nt)

 @sync @distributed for i = 1:nt
  mH[i] = calc_mH(lst.state_2d_set1[i],RAC)
  mT[i] = calc_mT(lst.state_3d_set1[i],:THETA,vol)
  mS[i] = calc_mT(lst.state_3d_set1[i],:SALT,vol)
 end

 println("done with monthly")

 ##

 if nt<maximum(tave)
   tV,tT,tS=[],[],[]
 else

 tV_m = SharedArray{Float64}(179,ntave)
 tT_m = SharedArray{Float64}(179,ntave)
 tS_m = SharedArray{Float64}(179,ntave)

 @sync @distributed for i in tave
  j=i-tave[1]+1
  tV_m[:,j] .= calc_tV(lst.trsp_3d_set1[i],G,LC)
  tmp = calc_tT(lst.trsp_3d_set2[i],G,LC)
  tT_m[:,j] .= tmp[1]
  tS_m[:,j] .= tmp[2]
 end

 tV=mean(tV_m,dims=2)
 tS=mean(tS_m,dims=2)
 tT=mean(tT_m,dims=2)

 println("done with transport")
 end

 ##

 return assemble(fc,mH,mT,mS,tV,tT,tS)

#return mH,mT,mS

end

##

function assemble(fc,mH,mT,mS,tV,tT,tS)

table=DataFrame(name=String[],index=Int[],value=Float64[])

#

if (!isempty(fc)) && (sum(occursin.("argo_feb2016_set3",fc.name))>0)

ii=findall(occursin.("argo_feb2016_set3",fc.name))[1]
append!(table,DataFrame(name="jT",index=0,value=fc.cost[ii]/fc.nb[ii]))
ii=findall(occursin.("argo_feb2016_set3",fc.name))[2]
append!(table,DataFrame(name="jS",index=0,value=fc.cost[ii]/fc.nb[ii]))

ii=findall(occursin.("sshv4-lsc",fc.name))[1]
append!(table,DataFrame(name="jHa",index=0,value=fc.cost[ii]/fc.nb[ii]))
ii=findall(occursin.("sshv4-gmsl",fc.name))[1]
append!(table,DataFrame(name="jHg",index=0,value=fc.cost[ii]/fc.nb[ii]))
ii=findall(occursin.("sshv4-mdt",fc.name))[1]
append!(table,DataFrame(name="jHm",index=0,value=fc.cost[ii]/fc.nb[ii]))

ii=findall(occursin.("sst-reynolds",fc.name))[1]
append!(table,DataFrame(name="jTs",index=0,value=fc.cost[ii]/fc.nb[ii]))
ii=findall(occursin.("sss_repeat",fc.name))[1]
append!(table,DataFrame(name="jSs",index=0,value=fc.cost[ii]/fc.nb[ii]))
ii=findall(occursin.("siv4-conc",fc.name))[1]
append!(table,DataFrame(name="jIs",index=0,value=fc.cost[ii]/fc.nb[ii]))

end

[append!(table,DataFrame(name="tV",index=k,value=tV[k])) for k in 1:length(tV)]
[append!(table,DataFrame(name="tT",index=k,value=tT[k])) for k in 1:length(tT)]
[append!(table,DataFrame(name="tS",index=k,value=tS[k])) for k in 1:length(tS)]
[append!(table,DataFrame(name="mH",index=k,value=mH[k])) for k in 1:length(mH)]
[append!(table,DataFrame(name="mT",index=k,value=mT[k])) for k in 1:length(mT)]
[append!(table,DataFrame(name="mS",index=k,value=mS[k])) for k in 1:length(mS)]

return table
end

"""
    compare(A::DataFrame,B::DataFrame)

```
using ClimateModels.DataFrames, ClimateModels.CSV
ref_file="test/testreport_baseline2.csv"
ref=CSV.read(ref_file,DataFrame)

using MITgcm
ECCO4_testreport.compare(report,ref)
```
"""
function compare(A::DataFrame,B::DataFrame)
 println("Error report:")
 for v in intersect(unique(A.name),unique(B.name))
  x=compare(A,B,v)
  if x > 0.01
    y=Int(round(100*x))
    w=rpad(v,4)
    println("$w : +- $y % large")
  else
    y=Int(floor(log10(x)))
    w=rpad(v,4)
    println("$w : +- 10^$y")
  end
 end
end

"""
    compare(A::DataFrame,B::DataFrame,v::AbstractString)
"""
function compare(A::DataFrame,B::DataFrame,v::AbstractString)
 a=sort(A[A.name.==v,:],:index)
 b=sort(B[B.name.==v,:],:index)
 nv=min(length(a.index),length(b.index))
 #nv=6
 if nv==1 
   abs(a.value[1]-b.value[1])/abs(a.value[1])
 else
   sqrt(mean((a.value[1:nv]-b.value[1:nv]).^2))/std(a.value[1:nv])
end
end

##

function parse_fc(fil)
    tmp1=readlines(fil)

    fc=(name=String[],cost=Float64[],nb=Int[])

    for i in tmp1
        (tmp2,tmp3)=split(i,"=")
        push!(fc.name,tmp2)
        tmp3=replace(tmp3,"E" => "e")
        tmp3=replace(tmp3,"D" => "e")
        (tmp4,tmp5)=split(tmp3)
        push!(fc.cost,parse(Float64,tmp4))
        push!(fc.nb,Int(parse(Float64,tmp5)))
    end

    fc
end

function list_diags_files(pth0)
  if isdir(joinpath(pth0,"diags","STATE"))
      (STATE,TRSP)=("STATE","TRSP")
  else
      (STATE,TRSP)=("","")
  end
  state_2d_set1=glob("state_2d_set1*data",joinpath(pth0,"diags",STATE))
  state_3d_set1=glob("state_3d_set1*data",joinpath(pth0,"diags",STATE))
  trsp_3d_set1=glob("trsp_3d_set1*data",joinpath(pth0,"diags",TRSP))
  trsp_3d_set2=glob("trsp_3d_set2*data",joinpath(pth0,"diags",TRSP))
  (state_2d_set1=state_2d_set1,state_3d_set1=state_3d_set1,
    trsp_3d_set1=trsp_3d_set1,trsp_3d_set2=trsp_3d_set2)
end

function list_diags_files_alt(pth0)
  state_2d_set1=glob("ETAN_mon_mean*data",joinpath(pth0,"ETAN_mon_mean"))
  state_3d_set1=glob("THETA_mon_mean*data",joinpath(pth0,"THETA_mon_mean"))
  trsp_3d_set1=glob("UVELMASS_mon_mean*data",joinpath(pth0,"UVELMASS_mon_mean"))
  trsp_3d_set2=glob("ADVx_TH_mon_mean*data",joinpath(pth0,"ADVx_TH_mon_mean"))
  (state_2d_set1=state_2d_set1,state_3d_set1=state_3d_set1,
    trsp_3d_set1=trsp_3d_set1,trsp_3d_set2=trsp_3d_set2)
end

function read_mdsio_mH(fil,nam)
 alt_names ? fil2=replace(fil,"ETAN" => nam) : fil2=fil
 read_mdsio(fil2,nam)
end

function calc_mH(fil,RAC)
  ETAN=read_mdsio_mH(fil,:ETAN)
  sIceLoad=read_mdsio_mH(fil,:sIceLoad)
  tmp=RAC.*(ETAN+sIceLoad./1029)
  sum(tmp)/sum(RAC)
end

function RAC_masked(pth0)
  hFacC=read_mdsio(joinpath(pth0,"hFacC.data"))
  RAC=read_mdsio(joinpath(pth0,"RAC.data"))
  #rac=write(G.RAC)
  for i in eachindex(hFacC[:,:,1])
    hFacC[i]==0 ? RAC[i]=0 : nothing
  end
  return Float64.(RAC)
end

function read_mdsio_mT(fil,nam)
 alt_names ? fil2=replace(fil,"THETA" => nam) : fil2=fil
 read_mdsio(fil2,nam)
end

#function calc_mT(fil,nam,vol)
function calc_mT(fil,nam::Symbol,vol)
    tmp=vol.*read_mdsio_mT(fil,nam)
    #[sum(tmp[:,:,k])/sum(vol[:,:,k]) for k in 1:size(tmp,3)]
    sum(tmp)/sum(vol)
end
  
function vol_masked(pth0)
    hFacC=read_mdsio(joinpath(pth0,"hFacC.data"))
    RAC=read_mdsio(joinpath(pth0,"RAC.data"))
    DRF=read_mdsio(joinpath(pth0,"DRF.data"))
    for i in eachindex(IndexCartesian(),hFacC)
      hFacC[i]=hFacC[i]*RAC[i[1],i[2]]*DRF[i[3]]
    end
    return Float64.(hFacC)
end

function load_llc90_grid(pth=MeshArrays.GRID_LLC90)
    pth==MeshArrays.GRID_LLC90 ? γ=GridSpec("LatLonCap",pth) : γ=gcmgrid(pth, "LatLonCap", 5,
      [(90, 270), (90, 270), (90, 90), (270, 90), (270, 90)], [90 1170], Float32, read, write)
    G=GridLoad(γ;option="full")
    LC=LatitudeCircles(-89.0:89.0,G)
    G,LC
end

function read_mdsio_tV(fil,nam)
 alt_names ? fil2=replace(fil,"UVELMASS" => nam) : fil2=fil
 read_mdsio(fil2,nam)
end

function calc_tV(fil,Γ,LC)
    u=read(read_mdsio_tV(fil,:UVELMASS),Γ.hFacW)  
    v=read(read_mdsio_tV(fil,:VVELMASS),Γ.hFacS)
    (Utr,Vtr)=UVtoTransport(u,v,Γ)

    #integrate across latitude circles and depth
    nz=size(Γ.hFacC,2); nt=12; nl=length(LC)
    MT=fill(0.0,nl)
    for z=1:nz
        UV=Dict("U"=>Utr[:,z],"V"=>Vtr[:,z],"dimensions"=>["x","y"])
        [MT[l]=MT[l]+ThroughFlow(UV,LC[l],Γ) for l=1:nl]
    end

    1e-6*MT
end

function read_mdsio_tT(fil,nam)
 alt_names ? fil2=replace(fil,"ADVx_TH" => nam) : fil2=fil
 read_mdsio(fil2,nam)
end

function calc_tT(fil,Γ,LC)
    TRx_T=read(read_mdsio_tT(fil,:ADVx_TH)+read_mdsio_tT(fil,:DFxE_TH),Γ.hFacW)  
    TRy_T=read(read_mdsio_tT(fil,:ADVy_TH)+read_mdsio_tT(fil,:DFyE_TH),Γ.hFacS)  
    TRx_S=read(read_mdsio_tT(fil,:ADVx_SLT)+read_mdsio_tT(fil,:DFxE_SLT),Γ.hFacW)  
    TRy_S=read(read_mdsio_tT(fil,:ADVy_SLT)+read_mdsio_tT(fil,:DFyE_SLT),Γ.hFacS)  

    #integrate across latitude circles and depth
    nz=size(Γ.hFacC,2); nt=12; nl=length(LC)
    MT=fill(0.0,nl)
    MS=fill(0.0,nl)
    for z=1:nz
        UV=Dict("U"=>TRx_T[:,z],"V"=>TRy_T[:,z],"dimensions"=>["x","y"])
        [MT[l]=MT[l]+ThroughFlow(UV,LC[l],Γ) for l=1:nl]
        UV=Dict("U"=>TRx_S[:,z],"V"=>TRy_S[:,z],"dimensions"=>["x","y"])
        [MS[l]=MS[l]+ThroughFlow(UV,LC[l],Γ) for l=1:nl]
    end

    return 1e-15*4e6*MT,1e-6*MS
end

end #module ECCO4_testreport

## Darwin3

"""
    setup_darwin3!(config::MITgcm_config)

Setup method for Darwin3 one dimensional examples.
"""
function setup_darwin3!(config::MITgcm_config)
    p0=joinpath(config,"MITgcm")
    cp(MITgcm.getdata("darwin3code"),p0)

    p1=joinpath(config,"MITgcm","mysetups")
    p2=joinpath(p1,config.configuration)
    mkdir(p1); mkdir(p2); mkdir(joinpath(p2,"build"))

    Darwin3_1D_configs_download()

    p3=joinpath(MITgcm.getdata("darwin3oneD"),"code_"*config.configuration)
    p4=joinpath(p2,"code_"*config.configuration)
    cp(p3,p4)

    p3=joinpath(MITgcm.getdata("darwin3oneD"),"input_"*config.configuration)
    p4=joinpath(p2,"input_"*config.configuration)
    cp(p3,p4)

    params=read_all_namelists(p4)
    params[:main][:PARM03][:nTimeSteps]=64

    P=OrderedDict()
    P[:main]=OrderedDict(
        :category=>"darwin3",
        :name=>config.configuration,
        :version=>"main")
    P[:build]=OrderedDict(
        :path=>joinpath(p2,"build"),
        :options=>"-mods=../code_"*config.configuration,
        :rebuild=>false,
        :exe=>"mitgcmuv",
        )
    push!(params,(:setup => P))

    push!(config.inputs,params...)

    #nTimeSteps=48
end

## verification experiments

"""
verification_experiments()

Get list of all `most-standard` configurations of `MITgcm` and return as an Array of `MITgcm_config`

```
exps=verification_experiments()
```
"""
function verification_experiments()
default_path()
pth=MITgcm_path[2]
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

"""
    verification_experiments(nam::String)

Get one configurations of `MITgcm` and return as a `MITgcm_config`

```
advect_xy=verification_experiments("advect_xy")
```
"""
verification_experiments(nam::String) = MITgcm_config(configuration=nam)

"""
    setup_verification!(config::MITgcm_config)

Setup method for verification experiments.
"""
function setup_verification!(config::MITgcm_config)
    pth_run=joinpath(config,"run")

    do_mpi=(haskey(config.inputs,:mpi) ? config.inputs[:mpi] : false)
    do_adj=(haskey(config.inputs,:adj) ? config.inputs[:adj] : false)
#    println("do_mpi=$(do_mpi)")
#    println("do_adj=$(do_adj)")
    input_folder=(haskey(config.inputs,:input_folder) ? config.inputs[:input_folder] : "")

    exe=(do_adj ? "mitgcmuv_ad" : "mitgcmuv")
    
    inp=(do_adj ? "input_ad" : "input")
    inp=(isempty(input_folder) ? inp : input_folder)

    p=joinpath(MITgcm_path[2],config.configuration,inp)

    tmpA=readdir(p)
    f=tmpA[findall([!isfile(joinpath(pth_run,tmpA[i])) for i in 1:length(tmpA)])]
    [(!ispath(joinpath(pth_run,f[i])) ? symlink(joinpath(p,f[i]),joinpath(pth_run,f[i])) : nothing ) for i in 1:length(f)]

    #replace relative paths with absolutes then exe prepare_run
    if isfile(joinpath(pth_run,"prepare_run"))
        try
            pth=pwd()
        catch e
            cd()
        end
        pth=pwd()
        cd(pth_run)
        #
        fil="prepare_run"
        meta = read(fil,String)
        meta = split(meta,"\n")
        ii=findall(occursin.("../../",meta))
        for i in ii
            meta[i]=replace(meta[i],"../../" => MITgcm_path[2]*"/")
        end
        ii=findall(occursin.("../",meta))
        for i in ii
            meta[i]=replace(meta[i],"../" => joinpath(MITgcm_path[2],config.configuration)*"/")
        end
        #rm old file from run dir
        rm(fil)
        #write new file in run dir
        txt=["$(meta[i])\n" for i in 1:length(meta)]
        fid = open(fil, "w")
        [write(fid,txt[i]) for i in 1:length(txt)]
        close(fid)
        #execute prepare_run
        chmod(fil,0o777)
        @suppress run(`./$(fil)`)
        #
        cd(pth)
    end

    params=read_all_namelists(pth_run)

    rootdir=MITgcm_path[1]
    optfile=if Sys.isapple()&&(Sys.ARCH==:aarch64)
#        build_options_default[2] #does not work, cause "../../../tools" v "../tools"        
#        build_options_default[1]*" -optfile="*rootdir*"/tools/build_options/darwin_arm64_gfortran"
        code=(do_adj ? "../code_ad" : "../code")
        "-mods=$(code) -optfile="*rootdir*"/tools/build_options/darwin_arm64_gfortran"
    elseif do_adj
        build_options_default_adj[1]
    else
        build_options_default[1]
    end

    path_new=joinpath(config,"MITgcm")
    if !isdir(path_new)
        mkdir(path_new); mkdir(joinpath(path_new,"verification"))
        path_source=MITgcm_path[1]
        path_verif=MITgcm_path[2]
        
        dirs=["eesupp","model","pkg","tools"]
        [symlink(joinpath(path_source,d),joinpath(path_new,d)) for d in dirs]
        cp(joinpath(path_source,"verification","testreport"),joinpath(path_new,"verification","testreport"))
    end

    conf=config.configuration
    path_conf=joinpath(path_new,"verification",conf)
    !isdir(path_conf) ? cp(joinpath(path_verif,conf),path_conf) : nothing
    builddir=joinpath(path_new,"verification",conf,"build")

    if do_mpi
        optfile=optfile*" -mpi"
        p=joinpath(pathof(config),"MITgcm","verification",config.configuration)
        SIZE_in=joinpath(p,"code","SIZE.h_MPI")
        SIZE_out=joinpath(p,"build","SIZE.h")
        !ispath(SIZE_out) ? cp(SIZE_in,SIZE_out) : nothing
    end

    if do_mpi&&do_adj
        main_command="mpirun -np 2 ./mitgcmuv_ad"
    elseif do_mpi
        main_command="mpirun -np 2 ./mitgcmuv"
    elseif do_adj
        main_command="./mitgcmuv_ad > output.txt"
    else
        main_command="./mitgcmuv > output.txt"
    end

    P=OrderedDict()
    P[:main]=OrderedDict(
        :category=>"verification",
        :name=>config.configuration,
        :version=>"main",
        :command=>main_command,
        :mpi=>do_mpi,
        :adj=>do_adj,
        :input_folder=>inp,
        )
    P[:build]=OrderedDict(
        :path=>builddir,
        :rootdir=>rootdir,
        :options=>optfile,
        :rebuild=>false,
        :exe=>exe,
        )
    push!(params,(:setup => P))

    push!(config.inputs,params...)

    return true
end

"""
    scan_verification(; path=MITgcm_path[2])

Scan the verification folder for 
    
- experiments (list_main)
- adjoint experiments (list_adj)
- input folders (list_inp)
- reference results (list_out)

```
list_main,list_adj,list_inp,list_out=MITgcm.scan_verification()

config="exp4"
ii=findall(list_main.=="exp4")[1]
println.(list_inp[ii]);

inputs=Dict(:input_folder=>"input.with_flt")
MC=MITgcm_config(configuration=config,inputs=inputs)
```
"""
scan_verification(; path=MITgcm_path[2]) = begin
    lst=readdir(path)
    lst=lst[findall((!occursin).("._",lst))]
    lst_not=["verification_parser.py","testreport","README.md"]
    lst_main=lst[findall((!in).(lst,Ref(lst_not)))]

    lst_adj=String[]
    lst_inp=[]
    lst_out=[]
    for e in lst_main
        p=joinpath(path,e)
        #experiments with adj
        ispath(joinpath(p,"code_ad")) ? push!(lst_adj,e) : nothing
        #subexperiments for each exp
        lst=readdir(p)
        lst=lst[findall((!occursin).("._",lst))]
        lst=lst[findall([length(t)>=5&&t[1:5]=="input" for t in lst])]
        push!(lst_inp,lst)
        #result files
        if ispath(joinpath(p,"results"))
            lst=readdir(joinpath(p,"results"))
            lst=lst[findall([length(t)>=6&&t[1:6]=="output" for t in lst])]
        else
            lst=[]
        end
        push!(lst_out,lst)
    end
    lst_main,lst_adj,lst_inp,lst_out
end 

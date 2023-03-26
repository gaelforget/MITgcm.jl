
using Glob, MITgcmTools, MeshArrays

"""
    testreport_ecco(pth0)

Parse cost function file etc

```
pth0="run_checkpoint67z_fixed"
testreport_ecco(pth0)
mH,T,z,tV,tT,tS=ans

using GR
inline("iterm")
plot(mH,color=:red)
```
"""
function testreport_ecco(pth0)

fil0=glob("costfun*",pth0)[1]
fc=parse_fc(fil0)

lst=list_diags_files(pth0)
state_2d_set1=read_mdsio(lst.state_2d_set1[1])

RAC=RAC_masked(pth0)
mH=[calc_mH(i,RAC) for i in lst.state_2d_set1]

vol=vol_masked(pth0)
fil=lst.state_3d_set1[1]
nam="THETA"
T=calc_mT(fil,nam,vol)

z=read_mdsio(joinpath(pth0,"RC.data"))

G,LC=load_llc90_grid()
tV=calc_tV(lst.trsp_3d_set1[1],G,LC)
tT,tS=calc_tT(lst.trsp_3d_set2[1],G,LC)

return mH,T,z,tV,tT,tS

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
  state_2d_set1=glob("state_2d_set1*data",joinpath(pth0,"diags"))
  state_3d_set1=glob("state_3d_set1*data",joinpath(pth0,"diags"))
  trsp_3d_set1=glob("trsp_3d_set1*data",joinpath(pth0,"diags"))
  trsp_3d_set2=glob("trsp_3d_set2*data",joinpath(pth0,"diags"))
  (state_2d_set1=state_2d_set1,state_3d_set1=state_3d_set1,
    trsp_3d_set1=trsp_3d_set1,trsp_3d_set2=trsp_3d_set2)
end

function calc_mH(fil,RAC)
  meta=read_meta(fil)
  i1=findall(meta.fldList[:].=="ETAN")[1]
  i2=findall(meta.fldList[:].=="sIceLoad")[1]
  state_2d_set1=read_mdsio(fil)
  tmp=RAC.*(state_2d_set1[:,:,i1]+state_2d_set1[:,:,i2]./1029)
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

function calc_mH(fil,RAC)
  meta=read_meta(fil)
  i1=findall(meta.fldList[:].=="ETAN")[1]
  i2=findall(meta.fldList[:].=="sIceLoad")[1]
  state_2d_set1=read_mdsio(fil)
  tmp=RAC.*(state_2d_set1[:,:,i1]+state_2d_set1[:,:,i2]./1029)
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

#function calc_mT(fil,nam,vol)
function calc_mT(fil,nam::String,vol)
    tmp=vol.*read_mdsio(fil,Symbol(nam))
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

function load_llc90_grid()
    pth=MeshArrays.GRID_LLC90
    γ=GridSpec("LatLonCap",pth)
    G=GridLoad(γ;option="full")
    LC=LatitudeCircles(-89.0:89.0,G)
    G,LC
end

function calc_tV(fil,Γ,LC)
    u=read(read_mdsio(fil,"UVELMASS"),Γ.hFacW)  
    v=read(read_mdsio(fil,"VVELMASS"),Γ.hFacS)
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

function calc_tT(fil,Γ,LC)
    TRx_T=read(read_mdsio(fil,"ADVx_TH")+read_mdsio(fil,"DFxE_TH"),Γ.hFacW)  
    TRy_T=read(read_mdsio(fil,"ADVy_TH")+read_mdsio(fil,"DFyE_TH"),Γ.hFacS)  
    TRx_S=read(read_mdsio(fil,"ADVx_SLT")+read_mdsio(fil,"DFxE_SLT"),Γ.hFacW)  
    TRy_S=read(read_mdsio(fil,"ADVy_SLT")+read_mdsio(fil,"DFyE_SLT"),Γ.hFacS)  

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


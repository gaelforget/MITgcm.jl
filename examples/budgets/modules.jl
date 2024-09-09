
##

module GRID
  using MeshArrays, JLD2

  γ=GridSpec("LatLonCap",MeshArrays.GRID_LLC90)

  τ=Tiles(γ,30,30)

  import Base: zeros
  function zeros(g::gcmgrid)
    tmp=MeshArray(γ)
    tmp.=0
    tmp
  end

  lon=GridLoadVar("XC",γ)
  lat=GridLoadVar("YC",γ)

  hFacC=GridLoadVar("hFacC",γ)
  M=GridLoadVar("hFacC",γ)
  [M[:,k]=land_mask(M[:,k]) for k in 1:50]

  dep=-GridLoadVar("RC",γ)
  dep_RF=-GridLoadVar("RF",γ)
  area=GridLoadVar("RAC",γ)

  DXG=GridLoadVar("DXG",γ)
  DYG=GridLoadVar("DYG",γ)

  DRF=GridLoadVar("DRF",γ)
  RAC=GridLoadVar("RAC",γ)
  Depth=GridLoadVar("Depth",γ)

  nr=length(dep)

  dlat=2
  lats=(-90+dlat/2:dlat:90-dlat/2)
  nl=length(lats)

### basins

basins=demo.ocean_basins()

list_Atl=["Atlantic","Gulf of Mexico","Hudson Bay","Mediterranean Sea","North Sea","Baffin Bay","GIN Seas"]
list_Pac=["Pacific","Bering Sea","Okhotsk Sea","Japan Sea","East China Sea"]
list_Ind=["indian","South China Sea","Java Sea","Timor Sea","Red Sea","Gulf"]
list_Arc=["Arctic","Barents Sea"]

function larger_basins(list_Atl)
  map_Atl=0*basins.map
  for i in list_Atl
   #println(i)
   jj=findall(basins.name.==i)[1]
   map_Atl.=map_Atl+1.0*(basins.map.==jj)
  end
  map_Atl
end

map_Atl=larger_basins(list_Atl)
map_Pac=larger_basins(list_Pac)
map_Ind=larger_basins(list_Ind)
map_Arc=larger_basins(list_Arc)

end

##

module BUDG

using MeshArrays, Statistics, Glob, JLD2, MITgcm

import Main.GRID: M, γ, area, dep_RF, nl, nr, hFacC, Depth, DRF
import Main.GRID: DXG, DYG, lats

import MITgcm: read_mdsio
read_mdsio(γ::gcmgrid,args...;kwargs...) = read(read_mdsio(args...;kwargs...),γ)

function parms()
yearFirst=1992; #first year covered by model integration
yearLast =2011; #last year covered by model integration
yearInAve=[yearFirst yearLast]; #period for time averages and variance computations
timeStep =3600; #model time step for tracers
iceModel =1;#0=use freezing point   1=use pkg/seaice   2=use pkg/thsice (not implemented)
useRFWF  =1;#1=real fresh water flux 0=virtual salt flux
useNLFS  =2;#2=rstar 1=nlfs 0=linear free surface
rhoconst =1029; #sea water density
rcp      =3994*rhoconst; # sea water rho X heat capacity
rhoi     = 910; #sea ice density
rhosn    = 330; #snow density
flami    = 3.34e05; # latent heat of fusion of ice/snow (J/kg)
flamb    = 2.50e06; # latent heat of evaporation (J/kg)
SIsal0   =4;
return (yearFirst=yearFirst, yearLast, timeStep=timeStep,
  iceModel=iceModel, useRFWF=useRFWF, useNLFS=useNLFS,
  rhoconst=rhoconst, rcp=rcp, rhoi=rhoi, rhon=rhosn,
  flami=flami, flamb=flamb,SIsal0)
end

P=parms()

###

fil_geothermal=joinpath("data","geothermalFlux.bin")
geoThermal=read_bin(fil_geothermal,Float32,γ)
geoThermal.=geoThermal/P.rcp

###

function files_list(diags_path="")
 files=glob("budg*.0000000732.meta",diags_path)
 isempty(files) ? files=glob("budg*.0000000024.meta",diags_path) : nothing
 if isempty(files)
  println("no budget files found")
  variables=[]
  files=[]
  times=[]
 else
  variables=[read_meta(f).fldList for f in files]
  files=[basename(split(f,".")[1]) for f in files]
  times=glob("$(files[1])*data",diags_path)
  times=parse.(Float64,[split(f,".")[end-1] for f in times])
 end
 files,variables,times
end

###

function add_one_field!( x,variable,record; fac=1.0,
         diags_path="" , fileroot="",
         fileroots=String[], variables=[])
  if isempty(fileroot)&&!isempty(fileroots)
    a=[findall(in(variable,vs)) for vs in variables]
    b=findall((!isempty).(a))
    if length(b)==1
      fr=fileroots[b[1]]
    elseif length(b)>1
      println("need to specify fileroot from : $(fileroots[b])")
    else
      error("variable not found")
    end
  elseif !isempty(fileroot)
    fr=fileroot
  else
    error("fileroot not found")
  end
  #println("fr=$(fr) diags_path=$(diags_path)")
  file=glob("$(fr)*data",diags_path)[record]
  #println(file*" -- "*variable)
  x.+=fac*read_mdsio(γ,file,Symbol(variable))
end

###

function init_arrays()
  tend=MeshArray(γ)
  forc=MeshArray(γ)
  adv=MeshArray(γ)
  dif=MeshArray(γ)
  (tend,forc,adv,dif)
end

###

function still_to_process(output_path="",times=[])
  nt=length(times)
  test=fill(false,nt)
  for t in 2:nt-1
    tt="$(Int(times[t]))"
    tt=repeat("0",10-length(tt))*tt
    test[t]=!isfile(joinpath(output_path,"dT_budget_$(tt).jld2"))
  end
  findall(test)
end

###

import Base: zeros
zeros(g::gcmgrid,n::Int)=read(zeros(g.ioSize...,n),g)

"""
    layer_heat_budget(t,times,diags_path="")

Compute heat budget from MITgcm output at time t, from level k=kbu to k=50

- Units are in K*m/s
- multiply by P.rcp to convert to W/m2
"""
function layer_heat_budget(t,times,diags_path="")
t_m_1=t-1

fr="budg3d_snap_set2"
tmp1=zeros(γ,nr)
add_one_field!(tmp1,"THETA",t_m_1,diags_path=diags_path,fileroot=fr)
tmp2=zeros(γ,nr)
add_one_field!(tmp2,"THETA",t,diags_path=diags_path,fileroot=fr)

fr="budg2d_snap_set1"
tmp01=zeros(γ)
add_one_field!(tmp01,"ETAN",t_m_1,diags_path=diags_path,fileroot=fr)
tmp02=zeros(γ)
add_one_field!(tmp02,"ETAN",t,diags_path=diags_path,fileroot=fr)

for k in 1:nr
    tmp00=DRF[k]*hFacC[:,k]*(1 .+tmp01/Depth)
    tmp1[:,k]=tmp00*tmp1[:,k]
    tmp00=DRF[k]*hFacC[:,k]*(1 .+tmp02/Depth)
    tmp2[:,k]=tmp00*tmp2[:,k]
end

dt=3600*(times[t]-times[t_m_1])
tend=(tmp2-tmp1)/dt

forc=zeros(γ,nr)
adv=zeros(γ,nr)
dif=zeros(γ,nr)

fr="budg3d_zflux_set3"
add_one_field!(adv,"ADVr_TH",t,fac=-1,diags_path=diags_path,fileroot=fr)
add_one_field!(dif,"DFrE_TH",t,fac=-1,diags_path=diags_path,fileroot=fr)
add_one_field!(dif,"DFrI_TH",t,fac=-1,diags_path=diags_path,fileroot=fr)

# vertical divergence
for k in 1:nr-1
    adv[:,k]=adv[:,k]-adv[:,k+1]
    dif[:,k]=dif[:,k]-dif[:,k+1]
end

TFLUX=zeros(γ)
oceQsw=zeros(γ)
fr="budg2d_zflux_set1"
add_one_field!(TFLUX,"TFLUX",t,fac=1/P.rcp,diags_path=diags_path,fileroot=fr)
add_one_field!(oceQsw,"oceQsw",t,fac=1/P.rcp,diags_path=diags_path,fileroot=fr)

for k in 1:nr
  dd=dep_RF[k]
  dd > 200 ? swfrac=0.0 : swfrac=0.62*exp(-dd/0.6)+(1-0.62)*exp(-dd/20)
  forc[:,k] = (k==1 ? TFLUX : swfrac*oceQsw)*(hFacC[:,k].>0)
end

# vertical divergence
for k in 1:nr-1
    forc[:,k]=forc[:,k]-forc[:,k+1]
end

for k in 1:nr
  forc[:,k] = forc[:,k] + geoThermal *
      (hFacC[:,k].>0) * (k<nr ? hFacC[:,k+1].==0 : 1)
end

#fr=(kbu==1 ? "budg2d_hflux_set2" : "budg2d_hflux_set3_$(kbu)")
fr="budg3d_hflux_set2"
tmpU=zeros(γ,nr)
add_one_field!(tmpU,"ADVx_TH",t,diags_path=diags_path,fileroot=fr)
difU=zeros(γ,nr)
add_one_field!(difU,"DFxE_TH",t,diags_path=diags_path,fileroot=fr)

#fr=(kbu==1 ? "budg2d_hflux_set2" : "budg2d_hflux_set3_$(kbu)")
fr="budg3d_hflux_set2"
tmpV=zeros(γ,nr)
add_one_field!(tmpV,"ADVy_TH",t,diags_path=diags_path,fileroot=fr)
difV=zeros(γ,nr)
add_one_field!(difV,"DFyE_TH",t,diags_path=diags_path,fileroot=fr)

for k in 1:nr
  adv[:,k]=(adv[:,k]+convergence(tmpU[:,k],tmpV[:,k]))/area
  dif[:,k]=(dif[:,k]+convergence(difU[:,k],difV[:,k]))/area
end

return (tend=M*tend,forc=M*forc,adv=M*adv,dif=M*dif)
end

##

"""
    layer_mass_budget(t,times=[],diags_path="")

Compute mass budget from MITgcm output at time t, from level k=kbu to k=50

- Units are in m/s
- multiply by P.rhoconst to convert to kg/m^2/s
"""
function layer_mass_budget(t,times=[],diags_path="")
t_m_1=t-1

tmp1=1 .+zeros(γ,nr)
tmp2=1 .+zeros(γ,nr)

fr="budg2d_snap_set1"
tmp01=zeros(γ)
add_one_field!(tmp01,"ETAN",t_m_1,diags_path=diags_path,fileroot=fr)
tmp02=zeros(γ)
add_one_field!(tmp02,"ETAN",t,diags_path=diags_path,fileroot=fr)

for k in 1:nr
    tmp00=DRF[k]*hFacC[:,k]*(1 .+tmp01/Depth)
    tmp1[:,k]=tmp00*tmp1[:,k]
    tmp00=DRF[k]*hFacC[:,k]*(1 .+tmp02/Depth)
    tmp2[:,k]=tmp00*tmp2[:,k]
end

dt=3600*(times[t]-times[t_m_1])
tend=(tmp2-tmp1)/dt

adv=zeros(γ,nr)
dif=zeros(γ,nr)
fr="budg3d_zflux_set3"
add_one_field!(adv,"WVELMASS",t,fac=-1,diags_path=diags_path,fileroot=fr)

# vertical divergence
for k in 1:nr-1
    adv[:,k]=(k>1 ? adv[:,k]-adv[:,k+1] : -adv[:,k+1]) 
end

forc=zeros(γ,nr)
oceFWflx=zeros(γ)
fr="budg2d_zflux_set1"
add_one_field!(oceFWflx,"oceFWflx",t,fac=1/P.rhoconst,diags_path=diags_path,fileroot=fr)
forc[:,1]=oceFWflx

fr="budg3d_hflux_set2"
tmpU=zeros(γ,nr)
add_one_field!(tmpU,"UVELMASS",t,diags_path=diags_path,fileroot=fr)

fr="budg3d_hflux_set2"
tmpV=zeros(γ,nr)
add_one_field!(tmpV,"VVELMASS",t,diags_path=diags_path,fileroot=fr)

for k in 1:nr
  tmpU[:,k]=tmpU[:,k]*DRF[k]
  tmpV[:,k]=tmpV[:,k]*DRF[k]
  adv[:,k]=adv[:,k]+convergence(tmpU[:,k]*DYG,tmpV[:,k]*DXG)/area
end

return (tend=M*tend,forc=M*forc,adv=M*adv,dif=M*dif)
end

###

"""
    layer_heat_snapshot(t,diags_path="")

Read snapshot from MITgcm output at time t, integrated from level k=kbu to k=50

- Units are in K*m
- multiply by P.rcp to convert to J/m^2
"""
function layer_heat_snapshot(t,diags_path="")
t_m_1=t-1

fr="budg3d_snap_set2"
tmp1=zeros(γ,nr)
add_one_field!(tmp1,"THETA",t_m_1,diags_path=diags_path,fileroot=fr)

fr="budg2d_snap_set1"
tmp01=zeros(γ)
add_one_field!(tmp01,"ETAN",t_m_1,diags_path=diags_path,fileroot=fr)

for k in 1:nr
    tmp00=DRF[k]*hFacC[:,k]*(1 .+tmp01/Depth)
    tmp1[:,k]=tmp00*tmp1[:,k]
end

return M*tmp1
end

"""
    layer_mass_snapshot(t,diags_path="")

Read snapshot from MITgcm output at time t, integrated from level k=kbu to k=50

- Units are in m
- multiply by P.rhoconst to convert to kg/m^2
"""
function layer_mass_snapshot(t,diags_path="")
t_m_1=t-1

tmp1=1 .+zeros(γ,nr)

fr="budg2d_snap_set1"
tmp01=zeros(γ)
add_one_field!(tmp01,"ETAN",t_m_1,diags_path=diags_path,fileroot=fr)

for k in 1:nr
    tmp00=DRF[k]*hFacC[:,k]*(1 .+tmp01/Depth)
    tmp1[:,k]=tmp00*tmp1[:,k]
end

return M*tmp1
end

function layer_snapshots(t,diags_path="")
  H_1=layer_heat_snapshot(t,diags_path)
  M_1=layer_mass_snapshot(t,diags_path)
  return H_1,M_1
end

##

"""
    temperature_equation(t,times; input_path="", output_path="")

```
for t in 1:36
(dT1,dT0)=temperature_equation(t)
end
```
"""
function temperature_equation(t,times; input_path="", output_path="")
  println(t)

  A=layer_heat_budget(t,times,input_path)
  dH=(tend=A.tend,forc=A.forc, adv=A.adv, dif=A.dif)

  A=layer_mass_budget(t,times,input_path)
  dV=(tend=A.tend,forc=A.forc, adv=A.adv, dif=A.dif)

  (H,V)=layer_snapshots(t,input_path)
  (Hp1,Vp1)=layer_snapshots(t+1,input_path)
  T=H/V
  Tp1=Hp1/Vp1

  dt=3600*(times[t]-times[t-1])
  dT1=(dH.tend-Tp1*dV.tend)/V*dt
  dT0=Tp1-T

  if !isempty(output_path)
    tt="$(Int(times[t]))"
    tt=repeat("0",10-length(tt))*tt
    dT1adv=(dH.adv-Tp1*dV.adv)/V*dt
    dT1dif=(dH.dif-Tp1*dV.dif)/V*dt
    dT1forc=(dH.forc-Tp1*dV.forc)/V*dt
    jldsave(joinpath(output_path,"dT_budget_$(tt).jld2"); T0=T, T1=Tp1,
	    t0=3600*times[t-1],t1=3600*times[t],
	    tend=dT1, adv=dT1adv, dif=dT1dif, forc=dT1forc)
  end

  return (dT1,dT0)
end

end

##

module PLOTS

using CairoMakie, MeshArrays, JLD2, Statistics
MeshArraysMakieExt = Base.get_extension(MeshArrays, :MeshArraysMakieExt)

import Main.BUDG: P, init_arrays
import Main.GRID: M

using Shapefile, Proj
projmap=MeshArraysMakieExt.projmap

fil_interp=joinpath("data","interp_coeffs_halfdeg.jld2")
λ=interpolation_setup(fil_interp)

hm(γ::gcmgrid,X::MeshArray;kwargs...) =  heatmap(X;interpolation=λ,kwargs...)
hm!(a...;ka...)=MeshArraysMakieExt.heatmap_interpolation!(a...;ka...)

function map_two(x1,x2,cr;titles=[], output_file="")
  f=Figure(size=(900,900))
  if !isempty(titles)
    tt1=titles[1]
    tt2=titles[2]
  else
    tt1="plot 1"
    tt2="plot 2"
  end
  ax1=Axis(f[1,1],title=tt1)
  hm1=hm!(ax1,x1,λ,colorrange=cr,colormap=:turbo)
  ax2=Axis(f[2,1],title=tt2)
  hm!(ax2,x2,λ,colorrange=cr,colormap=:turbo)
  Colorbar(f[1:2,2], hm1, height = Relative(0.65))
  !isempty(output_file) ? save(output_file,f) : nothing
  f
end

function zsum(x,k=1)
z=write(x)
z[findall(isnan.(z))].=0
read(sum(z[:,:,k:50],dims=3),x.grid)
end

function check_budget_1(A; k=1, output_file="")
  x1=A.tend; tt1="lhs"
  x2=A.adv+A.dif+A.forc-A.tend; tt2="rhs-lhs"
  cr=200 .*(-1,1)./P.rcp
  f=map_two(zsum(x1,k),zsum(x2,k),cr,titles=[tt1,tt2])
  !isempty(output_file) ? save(output_file,f) : nothing
  f
end

###

fil=joinpath("data","polygons","ne_110m_admin_0_countries.shp")
pol=MeshArrays.read_polygons(fil)

function projmap_format(v,ttl,cr,pol)
  lon0=-160.0
  trans=Proj.Transformation(MA_preset=3,lon0=lon0)
  ll=Makie.coordinates.(split(pol,lon0))
  meta=(colorrange=cr,cmap=:turbo,ttl=ttl,lon0=lon0)
  vv=Interpolate(v,λ.f,λ.i,λ.j,λ.w)
  vv=reshape(vv,size(λ.lon))
  (lon=λ.lon,lat=λ.lat,var=vv,meta=meta,polygons=ll,trans=trans)
end

add_polygons!(pol) = (!isempty(pol) ?
  [lines!(l1,color = :black, linewidth = 0.5) for l1 in pol] :
  nothing)

function map_one(ax1,x1,cr,λ,polygons=[])
  if isa(x1,MeshArray)
    hm1=hm!(ax1,x1,λ,colorrange=cr,colormap=:turbo)
    add_polygons!(polygons); ylims!(-75,75)
  else
    hm1=projmap(x1,x1.trans,skip_colorbar=true,skip_axis=true)
  end
  xlims!(-180,180); ylims!(-80,85)
  hm1
end

end


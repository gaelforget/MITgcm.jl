
"""
SeaWaterDensity(Θ,Σ,Π,Π0)

Compute potential density (ρP), in situ density (ρI), and density
referenced to PREF (Π0 in decibars) from potential temperature (Θ in °C),
salinity (Σ in psu) and pressure (Π in decibars) according to the
UNESCO / Jackett & McDougall 1994 equation of state.

Credits: code based on a Matlab implementation by B. Ferron
Reference: https://www.jodc.go.jp/info/ioc_doc/UNESCO_tech/059832eb.pdf
Check value: ρI = `1041.83267kg/m^3` for Θ=`3°Celcius`, Σ=`35psu`, Π=`3000dbar`
```
(ρP,ρI,ρR) = SeaWaterDensity(3.,35.5,3000.)
isapprox(ρI,1041.83267, rtol=1e-6)
```
"""
function SeaWaterDensity(Θ,Σ,Π,Π0=missing)

   #square root salinity
   sqrtΣ= sqrt.(Σ)
   #compute density pure water at atm pressure
   ZR1= ((((6.536332E-9*Θ .-1.120083E-6).*Θ .+1.001685E-4).*Θ
   .-9.095290E-3).*Θ .+6.793952E-2).*Θ .+999.842594
   #seawater density atm pressure
   ZR2= (((5.3875E-9*Θ .-8.2467E-7).*Θ .+7.6438E-5).*Θ
   .-4.0899E-3).*Θ .+0.824493
   ZR3= (-1.6546E-6*Θ .+1.0227E-4).*Θ .-5.72466E-3
   ZR4= 4.8314E-4

   #potential density (referenced to the surface)
   ρP= (ZR4*Σ + ZR3.*sqrtΣ + ZR2).*Σ + ZR1

   #add the compression terms
   ZE = (-3.508914E-8*Θ .-1.248266E-8).*Θ .-2.595994E-6
   ZBW= ( 1.296821E-6*Θ .-5.782165E-9).*Θ .+1.045941E-4
   ZB = ZBW + ZE .* Σ

   ZD = -2.042967E-2
   ZC = (-7.267926E-5*Θ .+2.598241E-3).*Θ .+0.1571896
   ZAW= ((5.939910E-6*Θ .+2.512549E-3).*Θ .-0.1028859).*Θ .-4.721788
   ZA = ( ZD*sqrtΣ + ZC).*Σ + ZAW

   ZB1= (-0.1909078*Θ .+7.390729).*Θ .-55.87545
   ZA1= ((2.326469E-3*Θ .+1.553190).*Θ .-65.00517).*Θ .+1044.077
   ZKW= (((-1.361629E-4*Θ .-1.852732E-2).*Θ .-30.41638).*Θ
   .+2098.925).*Θ .+190925.6
   ZK0= (ZB1.*sqrtΣ + ZA1).*Σ + ZKW

   #in situ density
   ρI = ρP ./ (1.0 .-Π./(ZK0-Π.*(ZA-Π.*ZB)))

   #density referenced to level Π0
   if !ismissing(Π0)
      ρR = ρP ./ (1.0 .-Π0./(ZK0-Π0.*(ZA-Π0.*ZB)))
   else
      ρR = ρP
   end

   return ρP,ρI,ρR
end

"""
MixedLayerDepth(Θ,Σ,Δ,mthd)

Compute mixed layer depth from potential temperature (Θ in °C),
salinity (Σ in psu) and depth (Δ in method) according to various
formulas (mthd == "BM", "Suga", "Kara"). Inputs must be dense
vectors without any missing value (or NaN, etc).

```
D=collect(0.0:1.0:500.0); tmp=(1.0.-tanh.(5*(-1 .+ 2/D[end]*D)));
T=2.0 .+ 8.0*tmp; S=34.0 .+ 0.5*tmp;
(ρP,ρI,ρR) = SeaWaterDensity(T,S,D);

mld=MixedLayerDepth(T,S,D,"BM"); isapprox(mld,134.0)

using Plots
plot(ρP,-D,w=2,label="Potential Density",ylabel="Depth")
plot!(vec([ρP[1] ρP[end]]),-fill(mld,2),label="Mixed Layer Depth",w=2,c="black",s=:dash)
```
"""
function MixedLayerDepth(Θ::Array{T,1},Σ::Array{T,1},Δ::Array{T,1},mthd::String) where T

   (ρP,ρI,ρR) = SeaWaterDensity(Θ,Σ,Δ) #to be more precise Δ should be converted to pressure

   dΘ=1e-4
   (rP,rI,rR) = SeaWaterDensity(Θ[1]+dΘ,Σ[1],Δ[1])
   α=(rP-ρP[1])/dΘ #could also probably be more precise
   dρKara=α*0.8

   nk=length(Δ)
   d=NaN
   for k=1:nk
      ρP[k]-ρP[1]>0.03&&isnan(d)&&(mthd=="BM") ? d=Δ[k] : nothing
      ρP[k]-ρP[1]>0.125&&isnan(d)&&(mthd=="Suga") ? d=Δ[k] : nothing
      ρP[k]-ρP[1]>dρKara&&isnan(d)&&(mthd=="Kara") ? d=Δ[k] : nothing
   end

   return d
end


"""
jmd94(θ,S,P,PREF)

Compute potential density (ρP), in situ density (ρI), and density
referenced to PREF (ρR) from potential temperature (θ in °C),
salinity (S in psu) and pressure (P in decibars) according to the
Jackett & McDougall 1994 equation of state.

Credits: code based on a Matlab implementation by B. Ferron

Check value: ρ = `1041.83267kg/m^3` for P=`3000dbar`, T=`3°Celcius`, and S=`35psu`
```
(ρP,ρI,ρR) = jmd94(3.,35.5,3000.)
isapprox(ρI,1041.83267, rtol=1e-6)
```
"""
function jmd94(θ,σ,Π,Π0=missing)

   #square root salinity
   σR= sqrt(σ);
   #compute density pure water at atm pressure
   ZR1= ((((6.536332E-9*θ-1.120083E-6).*θ+1.001685E-4).*θ
   -9.095290E-3).*θ+6.793952E-2).*θ+999.842594
   #seawater density atm pressure
   ZR2= (((5.3875E-9*θ-8.2467E-7).*θ+7.6438E-5).*θ
   -4.0899E-3).*θ+0.824493
   ZR3= (-1.6546E-6*θ+1.0227E-4).*θ-5.72466E-3
   ZR4= 4.8314E-4;

   #potential density (referenced to the surface)
   ρP= (ZR4*σ + ZR3.*σR + ZR2).*σ + ZR1

   #add the compression terms
   ZE = (-3.508914E-8*θ-1.248266E-8).*θ-2.595994E-6
   ZBW= ( 1.296821E-6*θ-5.782165E-9).*θ+1.045941E-4
   ZB = ZBW + ZE .* σ

   ZD = -2.042967E-2
   ZC = (-7.267926E-5*θ+2.598241E-3).*θ+0.1571896
   ZAW= ((5.939910E-6*θ+2.512549E-3).*θ-0.1028859).*θ-4.721788
   ZA = ( ZD*σR + ZC).*σ + ZAW

   ZB1= (-0.1909078*θ+7.390729).*θ-55.87545
   ZA1= ((2.326469E-3*θ+1.553190).*θ-65.00517).*θ+1044.077
   ZKW= (((-1.361629E-4*θ-1.852732E-2).*θ-30.41638).*θ
   +2098.925).*θ+190925.6
   ZK0= (ZB1.*σR + ZA1).*σ + ZKW

   #in situ density
   ρI = ρP ./ (1.0-Π./(ZK0-Π.*(ZA-Π.*ZB)))

   #density referenced to level Π0
   if !ismissing(Π0)
      ρR = ρP ./ (1.0-Π0./(ZK0-Π0.*(ZA-Π0.*ZB)))
   else
      ρR = ρP
   end

   return ρP,ρI,ρR
end

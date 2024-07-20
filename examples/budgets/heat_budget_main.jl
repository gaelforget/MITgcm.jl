using Pkg
using Distributed

# 1. load modules on all workers

@everywhere begin
  using GLMakie, MeshArrays, JLD2

  do_plot=true
  do_save=true

  include("modules.jl")

  todo=BUDG.still_to_process()
  println("left to do : $(length(todo))")

  vlist=["tend","forc","adv","dif"]
end

# 2. testing

t=120

## heat budget

A=BUDG.layer_heat_budget(t,1)
dH=A.tend
A_res=A.tend-A.forc-A.adv-A.dif

do_plot ? fig1=PLOTS.check_budget_1(A,do_save=do_save,k=1) : nothing

## mass budget

C=BUDG.layer_mass_budget(t,1)
dV=C.tend
C_res=C.tend-C.forc-C.adv-C.dif

cr=(-1,1).*2e-8
do_save ? fil3="tmp/budg_dT_maps_check1.png" : fil3=""
do_plot ? fig3=PLOTS.map_two(C.tend,C_res,cr,titles=["tend (mass)","res (mass)"],output_file=fil3) : nothing

## initial and final conditions

(H,V)=BUDG.layer_snapshots(t)
(Hp1,Vp1)=BUDG.layer_snapshots(t+1)
T=H/V
Tp1=Hp1/Vp1

if do_plot
 fig4=Figure(); ax4=Axis(fig4[1,1],title="dT from snapshots")
 hm4=PLOTS.hm!(ax4,Tp1-T,PLOTS.λ,colormap=:turbo)
 Colorbar(fig4[1,2], hm4, height = Relative(0.65))
 do_save ? save("tmp/budg_dT_maps_check2.png",fig4) : nothing
end

## temperature tendency

dt=3600*(IO_CLIM.times[t+1]-IO_CLIM.times[t])
dTa=(dH-Tp1*dV)/V*dt
dTb=Tp1-T

do_save ? fil5="tmp/budg_dT_maps_check3.png" : fil5=""
do_plot ? fig5=PLOTS.map_two(dTa,dTa-dTb,(-1,1).*1,
	titles=["dTa","dTa-dTb"],output_file=fil5) : nothing

# 3. main computation loop

pa="tmp/dT_budget_monthly"
!isdir(pa) ? mkdir(pa) : nothing

@sync @distributed for t in todo
  BUDG.temperature_equation(t,do_save=true,path=pa)
  GC.gc()
end

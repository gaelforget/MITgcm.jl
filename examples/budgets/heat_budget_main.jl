
# 0. file paths 

#jul. -p 2 src/heat_budget_main.jl data/diags_budget_monthly tmp/budget_monthly_heat

if !isempty(ARGS)
  input_path=ARGS[1]
  output_path=ARGS[2]
else
  input_path=joinpath("data","diags_budget_monthly")
  output_path=joinpath("tmp","dT_budget_monthly")
end

!ispath(output_path) ? mkdir(output_path) : nothing

# 1. load modules on all workers

using Pkg
using Distributed

@everywhere begin
  using CairoMakie, MeshArrays, JLD2

  do_plot=true
  do_save=true

  include("modules.jl")

  input_path=$(input_path)
  output_path=$(output_path)
  (files,variables,times)=BUDG.files_list(input_path)
  todo=BUDG.still_to_process(output_path,times)
  println("left to do : $(length(todo))")

  vlist=["tend","forc","adv","dif"]
end

# 2. testing

t=120

## heat budget

A=BUDG.layer_heat_budget(t,times,input_path)
dH=A.tend
A_res=A.tend-A.forc-A.adv-A.dif

do_save ? fil1="tmp/budg_full.png" : fil1=""
do_plot ? fig1=PLOTS.check_budget_1(A,k=1,output_file=fil1) : nothing

## mass budget

C=BUDG.layer_mass_budget(t,times,input_path)
dV=C.tend
C_res=C.tend-C.forc-C.adv-C.dif

cr=(-1,1).*2e-8
do_save ? fil3="tmp/budg_dT_maps_check1.png" : fil3=""
do_plot ? fig3=PLOTS.map_two(C.tend,C_res,cr,titles=["tend (mass)","res (mass)"],output_file=fil3) : nothing

## initial and final conditions

(H,V)=BUDG.layer_snapshots(t,input_path)
(Hp1,Vp1)=BUDG.layer_snapshots(t+1,input_path)
T=H/V
Tp1=Hp1/Vp1

if do_plot
 fig4=Figure(); ax4=Axis(fig4[1,1],title="dT from snapshots")
 hm4=PLOTS.hm!(ax4,Tp1-T,PLOTS.λ,colormap=:turbo)
 Colorbar(fig4[1,2], hm4, height = Relative(0.65))
 do_save ? save("tmp/budg_dT_maps_check2.png",fig4) : nothing
end

## temperature tendency

dt=3600*(times[t+1]-times[t])
dTa=(dH-Tp1*dV)/V*dt
dTb=Tp1-T

do_save ? fil5="tmp/budg_dT_maps_check3.png" : fil5=""
do_plot ? fig5=PLOTS.map_two(dTa,dTa-dTb,(-1,1).*1,
	titles=["dTa","dTa-dTb"],output_file=fil5) : nothing

# 3. main computation loop

!isdir(output_path) ? mkdir(output_path) : nothing

@sync @distributed for t in todo
  BUDG.temperature_equation(t,times,
     input_path=input_path,output_path=output_path)
  GC.gc()
end

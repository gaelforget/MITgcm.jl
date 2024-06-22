
quickrun(;configuration="advect_xy")=begin
  MC=MITgcm_config(configuration=configuration)
  setup(MC)
  MC.inputs[:setup][:build][:quick]=true
  build(MC)
  launch(MC)
  RS=scan_rundir(joinpath(MC,"run"))
end

"""
    system_check(;set_environment_variables_to_default=false)

"""
system_check(;setenv=false)=begin
  setenv ? MITgcm.set_environment_variables_to_default() : nothing
  tests=Dict()
  tst=[false]
  try
    path0=MITgcm.default_path()
    tst[1]=true
  catch
    tst[1]=false
  end
  push!(tests,("download"=>tst[1]))
    
  RS=quickrun(configuration="advect_xy")
  push!(tests,("complete"=>RS[:completed]))

  RS=quickrun(configuration="hs94.cs-32x32x5")
  push!(tests,("netcdf"=>RS[:packages][:mnc]))

  #log(MC)
  #monitor(MC)

  #- download
  #- compile
  #- run completed
  #- netcdf / mnc 
  #- mpi

  tests
end

"""
    set_environment_variables_to_default()

Defines environment variables to default values.

_!!! Warning : it is generally much better to adjust them to your own system !!!_

_!!! Warning : the defaults will likely NOT work on most systems !!!_
"""
set_environment_variables_to_default()=begin
  @static if Sys.islinux()
    ENV_print("DATADEPS_ALWAYS_ACCEPT",true)
    ENV_print("MPI_INC_DIR","/usr/lib/x86_64-linux-gnu/openmpi/include")
    ENV_print("NETCDF_ROOT","/usr")
  elseif Sys.isapple()
    ENV_print("NETCDF_ROOT","/opt/homebrew")
    ENV_print("MPI_INC_DIR","/opt/homebrew/include")
  else
    #generic_thing(a)
  end
end

ENV_print(a,b) = begin
  @warn "setting environment variable $a to $b"
  ENV[a]=b
end
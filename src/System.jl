
test_run(;configuration="advect_xy",rebuild=false)=begin
  MC=MITgcm_config(configuration=configuration)
  setup(MC)
  MC.inputs[:setup][:build][:rebuild]=rebuild
  build(MC)
  launch(MC)
  RS=scan_rundir(joinpath(MC,"run"))
end

"""
    system_check(;setenv=false,rebuild=true)

"""
system_check(;setenv=false,rebuild=true)=begin

  setenv ? set_environment_variables_to_default() : nothing

  test_env_nc=haskey(ENV,"NETCDF_ROOT")
  test_env_mpi=haskey(ENV,"MPI_INC_DIR")

  ##

  tests=[]
  tst=[false]
  try
    path0=MITgcm.default_path()
    tst[1]=true
  catch
    tst[1]=false
  end
  push!(tests,("MITgcm download"=>tst[1]))
    
  RS=test_run(configuration="advect_xy",rebuild=rebuild)
  tst0=(ismissing(RS) ? false : RS[:completed])
  push!(tests,("run complete"=>tst0))

#  RS=test_run(configuration="hs94.cs-32x32x5",rebuild=rebuild)
#  tst0=(ismissing(RS) ? false : RS[:packages][:mnc])
#  push!(tests,("netcdf output"=>tst0))

  push!(tests,("NETCDF_ROOT"=>test_env_nc))
  push!(tests,("MPI_INC_DIR"=>test_env_mpi))

  #- download
  #- compile
  #- run completed
  #- netcdf / mnc 
  #- mpi
  #- env

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
  elseif Sys.isapple()&&(Sys.ARCH==:x86_64)
    ENV_print("NETCDF_ROOT","/usr/local")
    ENV_print("MPI_INC_DIR","/usr/local/include")
  elseif Sys.isapple() #&&(Sys.ARCH==:AArch64)
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
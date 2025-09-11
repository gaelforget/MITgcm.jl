
using StyledStrings

"""
    system_check(;setenv=false,exe="",mpi=false)

Run a suite of diagnostics, including a test run of MITgcm.

```
using MITgcm
SC=MITgcm.system_check()
```
"""
system_check(;setenv=false,exe="",mpi=false,adj=false,opt=" -devel -ds -ieee")=begin

  setenv ? set_environment_variables_to_default() : nothing

  test_env_nc=(haskey(ENV,"NETCDF_ROOT") ? ENV["NETCDF_ROOT"] : "")
  test_env_mpi=(haskey(ENV,"MPI_INC_DIR") ? ENV["MPI_INC_DIR"] : "")

  ##

  tests=ClimateModels.OrderedDict()
  tst=[false]
  try
    path0=MITgcm.default_path()
    tst[1]=true
  catch
    tst[1]=false
  end
  push!(tests,("MITgcm download"=>tst[1]))
    
  config="advect_xy"
  MC=test_run(config,exe=exe,mpi=mpi,adj=adj,opt=opt)

  genmake_log=joinpath(pathof(MC),"MITgcm","verification","advect_xy","build","genmake.log")
  if isfile(genmake_log)
    genmake_log,genmake_state=scan_build_dir(MC)
    push!(tests,("genmake_log"=>genmake_log))
    push!(tests,("genmake_state"=>genmake_state))
  else
    push!(tests,("genmake_log"=>ClimateModels.OrderedDict()))
    push!(tests,("genmake_state"=>ClimateModels.OrderedDict()))
  end

  RS=scan_run_dir(joinpath(MC,"run"))
  tst0=(ismissing(RS) ? false : RS[:completed])
  push!(tests,("run complete"=>tst0))
  push!(tests,("test folder"=>pathof(MC)))

#  RS=test_run("hs94.cs-32x32x5")
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

  for tst in keys(tests)
    if isa(tests[tst],Bool)
      x = (tests[tst] ? "✅" : "❌")
      println(styled"{yellow:$(tst)} $(x)")
    end
  end

    for tst in keys(tests)
    if !isa(tests[tst],Bool)
      x=(isa(tests[tst],String) ? tests[tst] : typeof(tests[tst]))
      println(styled"{pink:$(tst)} {blue:$(x)}")
    end
  end

  println("")

  MITgcm.MITgcm_system_check(
    download=tests["MITgcm download"],
    complete=tests["run complete"],
    name=config,
    folder=tests["test folder"],
    path_MITgcm=MITgcm_path[1],
    path_verification=MITgcm_path[2],
    NETCDF_ROOT=tests["NETCDF_ROOT"],
    MPI_INC_DIR=tests["MPI_INC_DIR"],
    mpi=mpi,
    adj=adj,
    genmake_log=tests["genmake_log"],
    genmake_state=tests["genmake_state"],
  )

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

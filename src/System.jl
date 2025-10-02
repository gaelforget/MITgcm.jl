
using StyledStrings

"""
    system_check(;setenv=false,exe="",mpi=false,adj=false,opt=" -devel -ds -ieee")

Run a suite of diagnostics, including a test run of MITgcm.

Notes : 

- adj=true requires a properly set up TAF license
- fix needed : tutorial_global_oce_biogeo/input_ad/data.gmredi and data.grdchk

```
using MITgcm
SC=system_check()
```
"""
system_check(;setenv=false,exe="",mpi=false,adj=false,config="advect_xy",opt=" -devel -ds -ieee")=begin

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
    
  MC=test_run(config,exe=exe,mpi=mpi,adj=adj,opt=opt)

  genmake_log=joinpath(pathof(MC),"MITgcm","verification",config,"build","genmake.log")
  if isfile(genmake_log)
    genmake_log,genmake_state=scan_build_dir(MC)
    push!(tests,("genmake_log"=>genmake_log))
    push!(tests,("genmake_state"=>genmake_state))
  else
    push!(tests,("genmake_log"=>ClimateModels.OrderedDict()))
    push!(tests,("genmake_state"=>ClimateModels.OrderedDict()))
  end

  RS=scan_run_dir(joinpath(MC,"run"))
  tst0=(ismissing(RS) ? false : RS.completed)
  push!(tests,("run complete"=>tst0))
  push!(tests,("test folder"=>pathof(MC)))

#  RS=test_run("hs94.cs-32x32x5")
#  tst0=(ismissing(RS) ? false : RS[:packages][:mnc])
#  push!(tests,("netcdf output"=>tst0))

  push!(tests,("NETCDF_ROOT"=>test_env_nc))
  push!(tests,("MPI_INC_DIR"=>test_env_mpi))

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
  setenv("DATADEPS_ALWAYS_ACCEPT",true)
  env=default_environment_variables()
  setenv("NETCDF_ROOT",env.NETCDF_ROOT)
  setenv("MPI_INC_DIR",env.MPI_INC_DIR)
end

"""
    default_environment_variables()

Suggested default environment variables depending on OS.
"""
default_environment_variables()=begin
  @static if Sys.islinux()&&(Sys.ARCH==:AArch64)
    (NETCDF_ROOT="/usr",MPI_INC_DIR="/usr/lib/aarch64-linux-gnu/openmpi/include")
  elseif Sys.islinux()
    (NETCDF_ROOT="/usr",MPI_INC_DIR="/usr/lib/x86_64-linux-gnu/openmpi/include")
#  elseif Sys.isapple()&&(Sys.ARCH==:x86_64)
#    (NETCDF_ROOT="/usr/local",MPI_INC_DIR="/usr/local/include")
  elseif Sys.isapple() #&&(Sys.ARCH==:AArch64)
    (NETCDF_ROOT="/opt/homebrew",MPI_INC_DIR="/opt/homebrew/include")
  else
    (NETCDF_ROOT="",MPI_INC_DIR="")
  end
end

"""
    setenv(a,b)

Set environment variable `a` to value `b`. Issue warning.

```
MITgcm.setenv("DATADEPS_ALWAYS_ACCEPT",true)
env=MITgcm.default_environment_variables()
MITgcm.setenv("NETCDF_ROOT",env.NETCDF_ROOT)
MITgcm.setenv("MPI_INC_DIR",env.MPI_INC_DIR)
system_check()
```
"""
setenv(a,b) = begin
  @warn "setting environment variable $a to $b"
  ENV[a]=b
end

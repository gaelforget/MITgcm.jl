
using StyledStrings

test_run(;configuration="advect_xy",exe="",mpi=false)=begin
  MC=MITgcm_config(configuration=configuration)
  setup(MC)
  if isempty(exe)&&mpi
    println("compiling and running test with MPI")
    MC.inputs[:setup][:build][:options]=MC.inputs[:setup][:build][:options]*" -devel -ds -ieee -mpi"
    SIZE_in=joinpath(pathof(MC),"MITgcm","verification","advect_xy","code","SIZE.h_MPI")
    SIZE_out=joinpath(pathof(MC),"MITgcm","verification","advect_xy","build","SIZE.h")
    cp(SIZE_in,SIZE_out)
    push!(MC.inputs[:setup][:main],(:command => "mpirun -np 2 ./mitgcmuv"))
    build(MC)
  elseif isempty(exe)
    println("compiling and running test")
    MC.inputs[:setup][:build][:options]=MC.inputs[:setup][:build][:options]*" -devel -ds -ieee"
    build(MC)
  else
    println("running test with precompiled model")
    pth=joinpath(MC,"run","mitgcmuv")
    rm(pth)
    symlink(exe,pth)
    push!(MC.inputs[:setup][:main],(:exe => exe))
  end  
  launch(MC)
  MC
end

"""
    system_check(;setenv=false,rebuild=true)

Run a suite of diagnostics, including a test run of MITgcm.
"""
system_check(;setenv=false,exe="",mpi=false)=begin

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
  MC=test_run(configuration=config,exe=exe,mpi=mpi)

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

#  RS=test_run(configuration="hs94.cs-32x32x5")
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

  MITgcm.MITgcm_system_check(
    download=tests["MITgcm download"],
    complete=tests["run complete"],
    name=config,
    folder=tests["test folder"],
    NETCDF_ROOT=tests["NETCDF_ROOT"],
    MPI_INC_DIR=tests["MPI_INC_DIR"],
    mpi=mpi,
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

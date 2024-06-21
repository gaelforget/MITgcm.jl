
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

function create_script(rundir=pwd())

## submission script

submission_script="""
#PBS -S /bin/csh
#PBS -l select=1:ncpus=16:mpiprocs=16:model=ivy+4:ncpus=20:mpiprocs=20:model=ivy
#PBS -q long
#PBS -l walltime=24:00:00
#PBS -q long
#PBS -o $(rundir)
#PBS -e $(rundir)
##PBS -m bea
#PBS -m n

#environment variables and libraries
#-----------------------------------

limit stacksize unlimited
module purge
module load comp-intel/2020.4.304 mpi-hpe/mpt
module load hdf4/4.2.12 hdf5/1.8.18_mpt netcdf/4.4.1.1_mpt
module load cuda/11.0
module list

setenv LD_LIBRARY_PATH \${LD_LIBRARY_PATH}:\${HOME}/lib
#setenv MPIP "-k 2"
setenv MPI_IB_TIMEOUT 20
setenv MPI_IB_RAILS 2
setenv MPI_IB_FAILOVER 1
setenv MPI_CONNECTIONS_THRESHOLD  2049

#model run
#---------

cd $(rundir)
mpiexec -np 96 ./mitgcmuv
"""

return submission_script

end

function to_csh(submission_script,fname="submission_script.csh")
  open(fname, "w") do io
    print(io,submission_script)
  end
  fname
end


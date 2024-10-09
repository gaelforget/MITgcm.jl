
"""
    create_script(rundir=pwd(),filename="")

Create submission script -- for NASA pleiades in this example.

```
submission_script=MITgcm.create_script(pwd(),"job.csh")
```    
"""
function create_script(rundir=pwd(),filename="",script=script_for_pleiades)
  submission_script=script(rundir)
  isempty(filename) ? fn=tempname()*".csh" : fn=filename
  to_csh(submission_script,fn)
  return fn
end

function to_csh(submission_script,fname="job.csh")
  open(fname, "w") do io
    print(io,submission_script)
  end
end

script_for_pleiades(rundir)="""
#PBS -S /bin/csh
#PBS -l select=1:ncpus=16:mpiprocs=16:model=ivy+4:ncpus=20:mpiprocs=20:model=ivy
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

script_for_pcluster(rundir)="""
#!/bin/bash
#SBATCH -J ECCO4
#SBATCH --nodes=3
#SBATCH --ntasks-per-node=36
#SBATCH --time=24:00:00
#SBATCH --exclusive
#SBATCH --partition=sealevel-c5n18xl-demand
#SBATCH --mem-per-cpu=1GB
#SBATCH -o ECCO4-%j-out
#SBATCH -e ECCO4-%j-out

# Initialize and set up the environment
#--------------------------------------

umask 022
ulimit -s unlimited
source /etc/profile
source /shared/spack/share/spack/setup-env.sh
source /usr/share/modules/init/sh

module purge
module add openmpi-4.1.1-gcc-9.4.0-jgsdvep
module add netcdf-fortran-4.5.3-gcc-11.1.0-d35hzyr
module list

export FORT_BUFFERED=1
export MPI_BUFS_PER_PROC=128
export MPI_DISPLAY_SETTINGS=""

#model run
#---------

cd $(rundir)
mpiexec -np 96 ./mitgcmuv
"""

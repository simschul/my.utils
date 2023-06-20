#!/bin/sh
########## Begin Slurm header #########
#
# Give job a reasonable name
#SBATCH --job-name {{jobname}}
#
# Request number of nodes and CPU cores per node for job
#SBATCH --nodes={{n_nodes}} 
#SBATCH --cpus-per-task={{n_cores}}
#
# Estimated wallclock time for job in minutes 40:00:00
#SBATCH --time={{time_minutes}}
#
# RAM (per node in mb, max = 96327mb) 62000
#SBATCH --mem={{RAM_per_node}}
#
# Write standard output and errors in same file
#SBATCH --export=ALL,EXECUTABLE=./omp_exe
#
# Send mail when job begins, aborts and ends
#SBATCH --mail-type=ALL
#
########### End SLURM header ##########

#Usually you should set
export KMP_AFFINITY=compact,1,0
#export KMP_AFFINITY=verbose,compact,1,0 prints messages concerning the supported affinity
#KMP_AFFINITY Description: https://software.intel.com/en-us/node/524790#KMP_AFFINITY_ENVIRONMENT_VARIABLE

export OMP_NUM_THREADS=$((${SLURM_JOB_CPUS_PER_NODE}/2))
echo "Executable ${EXECUTABLE} running on ${SLURM_JOB_CPUS_PER_NODE} cores with ${OMP_NUM_THREADS} threads"
startexe=${EXECUTABLE}
echo $startexe
#exec $startexe



# Setup R and Rmpi Environment
module load math/R/3.6.3

R CMD BATCH {{file_to_execute}}



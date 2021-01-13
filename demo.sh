#!/bin/bash --login

#SBATCH --account=scw1056
#SBATCH --job-name=demo
#SBATCH --partition=compute
#SBATCH --nodes=1
#SBATCH --ntasks=40
#SBATCH -o out_%J
#SBATCH -e err_%J

module purge

module load compiler/gnu/8/1.0
module load mpi/openmpi/1.10.7	# not loaded automatically by the R module, but installed for 3.2.3
module load R/3.6.0

R CMD BATCH ./NERVES_HEanalysis.R                # Always use 1 processor here as Rmpi spawns other slaves dynamically


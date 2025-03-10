#!/bin/bash
#SBATCH -n 1
#SBATCH --time 04:00:00
#SBATCH --mem-per-cpu=124G
#SBATCH --array=[1-40]%1
#SBATCH --job-name=gjam_probe1_base
#SBATCH --output=gjam_probe1_base.out
#SBATCH --error=gjam_probe1_base.err
#SBATCH --open-mode=append
#SBATCH --mail-type=END,FAIL

module load stack/2024-06 r/4.4.0 udunits/2.2.28 gdal/3.4.3 proj/9.2.1 sqlite/3.43.2 geos/3.9.1
cd ~/masterthesis				# Change directory

# Define parameter sets
version="full"
subFact=100
SEED=1234
past_reps=10
n_reps=160	# must match --array above and (n_reps+past_reps) <= subFact**2

# run as array by selecting random number
Rscript scripts/masterscripts/run_gjamTime_batch.R "$version" "$subFact" "$n_reps" "$SLURM_ARRAY_TASK_ID"

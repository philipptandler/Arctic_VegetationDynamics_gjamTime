#!/bin/bash
#SBATCH -n 1
#SBATCH --time 06:00:00
#SBATCH --mem-per-cpu=124G
#SBATCH --array=[1-40]%1
#SBATCH --job-name=gjam_probe1
#SBATCH --output=gjam_probe1.out
#SBATCH --error=gjam_probe1.err
#SBATCH --open-mode=append
#SBATCH --mail-type=END,FAIL

module load stack/2024-06 r/4.4.0 udunits/2.2.28 gdal/3.4.3 proj/9.2.1 sqlite/3.43.2 geos/3.9.1
cd ~/masterthesis				# Change directory

# Define calling script:
scrpt="$1"
# scrpt="scripts/project/1_gjamTime/call_probe1_base.R"

# run as array by selecting random number
Rscript scripts/masterscripts/run_gjamTime_batch.R "$scrpt" "$SLURM_ARRAY_TASK_ID"

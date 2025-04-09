#!/bin/bash
#SBATCH -n 1
#SBATCH --time 48:00:00
#SBATCH --mem-per-cpu=124G
#SBATCH --job-name=find_fixpt_MSc
#SBATCH --output=find_fixpt_MSc.out
#SBATCH --error=find_fixpt_MSc.err
#SBATCH --open-mode=append
#SBATCH --mail-type=END,FAIL

module load stack/2024-06 r/4.4.0 udunits/2.2.28 gdal/3.4.3 proj/9.2.1 sqlite/3.43.2 geos/3.9.1
cd ~/masterthesis				# Change directory

# Define calling script:
arg="$1"
# arg="scripts/project/1_gjamTime/call_probe1_base_regular.R"

Rscript scripts/project/2_analysis/fixpt_paramMSc.R

#!/bin/bash
#SBATCH -n 1
#SBATCH --time 12:00:00
#SBATCH --mem-per-cpu=124G
#SBATCH --job-name=write_wrate
#SBATCH --output=write_wrate.out
#SBATCH --error=write_wrate.err
#SBATCH --open-mode=append
#SBATCH --mail-type=END,FAIL

module load stack/2024-06 r/4.4.0 udunits/2.2.28 gdal/3.4.3 proj/9.2.1 sqlite/3.43.2 geos/3.9.1
cd ~/masterthesis				# Change directory

Rscript scripts/project/2_analysis/write_wrate.R

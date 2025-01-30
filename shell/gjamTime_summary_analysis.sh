#!/bin/bash

# Submit the first job (simulation) and capture the Job ID
SIM_JOB_ID=$(sbatch slurm_simulation.sh | awk '{print $4}')
echo "Simulation job submitted with ID: $SIM_JOB_ID"

# Submit the second job (postprocessing) with a dependency on the first
POST_JOB_ID=$(sbatch --dependency=afterok:$SIM_JOB_ID slurm_postprocessing.sh)
echo "Postprocessing job submitted with ID: $POST_JOB_ID (depends on $SIM_JOB_ID)"

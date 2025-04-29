#!/bin/bash

# Define arguments
SETUP_ARGUMENTS=(
        "scripts/project/1_gjamTime/call_probe1_base_nf.R"
	)

for arg in "${SETUP_ARGUMENTS[@]}"; do
    # Submit run_gjamTime_shell.sh
    # jobid1=$(sbatch shell/run_gjamTime_shell.sh "$arg" | awk '{print $4}')
    # echo "Submitted run_gjamTime_shell.sh with Job ID $jobid1 and argument $arg"

    # Submit summarize_gjamTime_shell.sh with dependency on run_gjamTime_shell.sh
    jobid2=$(sbatch --dependency=afterok:30354303 shell/summarize_gjamTime_shell.sh "$arg" | awk '{print $4}')
    echo "Submitted summarize_gjamTime_shell.sh with Job ID $jobid2 and argument $arg"
    
    # Submit find_fixpt_shell.sh with dependency on summarize_gjamTime_shell.sh
    # jobid3=$(sbatch --dependency=afterok:$jobid2 shell/find_fixpt_shell.sh "$arg" | awk '{print $4}')
    jobid3=$(sbatch --dependency=afterok:$jobid2 shell/find_fixpt_shell.sh "$arg" | awk '{print $4}')
    echo "Submitted find_fixpt_shell.sh with Job ID $jobid3 and argument $arg"

    # Submit find_jacobian_direct.sh with dependency on find_fixpt_shelll.sh
    jobid4=$(sbatch --dependency=afterok:$jobid3 shell/find_jacobian_direct.sh | awk '{print $4}')
    echo "Submitted find_jacobian_direct.sh with Job ID $jobid4"
done

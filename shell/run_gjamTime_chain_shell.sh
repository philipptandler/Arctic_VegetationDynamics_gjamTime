#!/bin/bash

# Define arguments
SETUP_ARGUMENTS=(
        "probe1_base"
       	# "probe1_highorder1"
       	# "probe1_highorder2"
	)

for arg in "${SETUP_ARGUMENTS[@]}"; do
    # Submit run_gjamTime_shell.sh
    # jobid1=$(sbatch shell/run_gjamTime_shell.sh "$arg" | awk '{print $4}')
    # echo "Submitted run_gjamTime_shell.sh with Job ID $jobid1 and argument $arg"

    # Submit summarize_gjamTime_shell.sh with dependency on run_gjamTime_shell.sh
    # jobid2=$(sbatch --dependency=afterok:$jobid1 shell/summarize_gjamTime_shell.sh "$arg" | awk '{print $4}')
    # echo "Submitted summarize_gjamTime_shell.sh with Job ID $jobid2 and argument $arg"
    
    # Submit find_fixpt_shell.sh, add dependency in queue for jobid2
    jobid3=$(sbatch shell/find_fixpt_shell.sh "$arg" | awk '{print $4}')
    echo "Submitted run_gjamTime_shell.sh with Job ID $jobid3 and argument $arg"
done

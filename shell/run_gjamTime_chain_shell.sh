#!/bin/bash

# Define setup scripts
SETUP_SCRIPTS=(
	"scripts/project/1_gjamTime/call_probe1_base.R"
       	"scripts/project/1_gjamTime/call_probe1_highorder1.R"
       	"scripts/project/1_gjamTime/call_probe1_highorder2.R"
	)

for scrpt in "${SETUP_SCRIPTS[@]}"; do
    # Submit run_gjamTime_shell.sh
    jobid1=$(sbatch --export=ALL scrpt="$scrpt" shell/run_gjamTime_shell.sh "$scrpt" | awk '{print $4}')
    echo "Submitted run_gjamTime_shell.sh with Job ID $jobid1 and setup script $scrpt"

    # Submit summarize_gjamTime_shell.sh with dependency on run_gjamTime_shell.sh
    jobid2=$(sbatch --dependency=afterok:$jobid1 --export=ALL scrpt="$scrpt" shell/summarize_gjamTime_shell.sh "$scrpt" | awk '{print $4}')
    echo "Submitted summarize_gjamTime_shell.sh with Job ID $jobid2 and setup script $scrpt"
done

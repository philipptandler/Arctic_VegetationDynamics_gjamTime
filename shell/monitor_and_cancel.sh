#!/bin/bash

# Full path to the directory you're monitoring
DIR="/cluster/scratch/tandlerp/analysis/.tmp"

# Target file to trigger cancellation
TARGET="chunk_process_id-100.tif"

# SLURM Job ID to cancel (passed as argument)
JOBID=$1

echo "Monitoring $DIR for $TARGET to cancel job $JOBID..."

while true; do
    if [ -f "$DIR/$TARGET" ]; then
        echo "Found $TARGET — cancelling job $JOBID"
        scancel "$JOBID"
        break
    fi
    sleep 0.1  # Check every x seconds
done


#!/bin/bash

jobid1=$(sbatch script1.sh | awk '{print $4}')
echo "Submitted script1.sh with Job ID $jobid1"

jobid2=$(sbatch --dependency=afterok:$jobid1 script2.sh | awk '{print $4}')
echo "Submitted script2.sh with Job ID $jobid2"

jobid3=$(sbatch --dependency=afterok:$jobid2 script3.sh | awk '{print $4}')
echo "Submitted script3.sh with Job ID $jobid3"

jobid4=$(sbatch --dependency=afterok:$jobid3 script4.sh | awk '{print $4}')
echo "Submitted script4.sh with Job ID $jobid4"


#!/bin/bash -l
#SBATCH --mem-per-cpu=2GB
#SBATCH --job-name=bs_array
#SBATCH --output=output.array.%A.%a
#SBATCH --array=1-267
#SBATCH --time=0-0:20


module load apps/singularity

singularity pull docker://lucascacule/mri.python:v2
singularity exec ./mri.python_v2.sif python run_bstates.py $SLURM_ARRAY_TASK_ID

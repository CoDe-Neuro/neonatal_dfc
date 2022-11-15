#!/bin/bash -l
#SBATCH --mem-per-cpu=4GB
#SBATCH --job-name=bs_array
#SBATCH --output=output.array.%A.%a
#SBATCH --array=1-347


module load apps/singularity

singularity pull docker://lucascacule/mri.python:v2
singularity exec ./mri.python_v2.sif python run_kuramoto.py $SLURM_ARRAY_TASK_ID
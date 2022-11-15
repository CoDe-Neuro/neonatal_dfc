#!/bin/bash -l
#SBATCH --mem-per-cpu=100GB
#SBATCH --job-name=moments_run


module load apps/singularity

singularity pull docker://lucascacule/mri.python:v2
singularity exec ./mri.python_v2.sif python run_bigClus.py 483 moments 0 XX

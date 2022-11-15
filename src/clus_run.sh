#!/bin/bash -l
#SBATCH --mem-per-cpu=60GB
#SBATCH --job-name=cluster


module load apps/singularity

singularity pull docker://lucascacule/mri.python:v2
singularity exec ./mri.python_v2.sif python run_bigClus.py 267 LEiDa 0
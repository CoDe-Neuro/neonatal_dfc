#!/bin/bash -l
#SBATCH --mem-per-cpu=60GB
#SBATCH --job-name=pca_run


module load apps/singularity

singularity pull docker://lucascacule/mri.python:v2
singularity exec ./mri.python_v2.sif python run_pca.py 267 LEiDa 0

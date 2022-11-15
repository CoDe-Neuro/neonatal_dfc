#!/bin/bash -l
#SBATCH --mem-per-cpu=300GB
#SBATCH --job-name=pca_run_surr


module load apps/singularity

singularity pull docker://lucascacule/mri.python:v2
singularity exec ./mri.python_v2.sif python run_pca.py 483 ensembleArray_surr 1 0
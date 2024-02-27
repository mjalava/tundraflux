#!/bin/bash -l
#SBATCH --job-name=r_array
#SBATCH --account=project_2000371
#SBATCH --output=output_%j_%a.txt
#SBATCH --error=errors_%j_%a.txt
#SBATCH --partition=small
#SBATCH --time=4:00:00
#SBATCH --array=1-40
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=5000

# Load r-env-singularity

module load r-env-singularity

# Clean up .Renviron file in home directory

if test -f ~/.Renviron; then
    sed -i '/TMPDIR/d' ~/.Renviron
fi

# Specify a temp folder path

echo "TMPDIR=/scratch/project_2000371/rtmp/" >> ~/.Renviron



# Run the R script

srun singularity_wrapper exec Rscript --no-save 9a_mixture_dists.R $SLURM_ARRAY_TASK_ID

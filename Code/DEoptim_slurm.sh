#!/bin/bash

############################################################################
# Slurm Script to copy input and output directories/files onto compute node
# Run optimization routine
############################################################################

# Running a job managed by Slurm
# -t time limit to batch job
# -a submit job array (32 processors per node to max) ! THIS IS WRONG! splits to different compute nodes
# -N number of nodes
# -n number of tasks, requests number of processor cores per subjob
# -J name of job
# --mail-user email address to send notifications
# --mail-type=ALL send user email at start and end of job
# --exclusive do not share compute node
# -p queue to run job
# --no-requeue do not resubmit job if fails
# -o create an out file of log
# -e create an error file
# -x exclude these compute nodes
# --mem 256000 request bigger node

# These are default settings
#SBATCH -t 70:00:00
#SBATCH -N 1
#SBATCH -n 32

#SBATCH -p priority

# These settings get changed by Subset script
#SBATCH -J Optim
#SBATCH --mail-user katie.renwick@gmail.com
#SBATCH --mail-type=ALL

#SBATCH --mem-per-cpu 2000



###############
# Copy files to /local on compute node
# Create temporary directory

jobdir="/local/job/$SLURM_JOB_ID"
jobname=Optim
glAbrv=grid
scriptsDir=/home/katie.renwick/scripts/Slurm_scripts/C2E
outputDir=/mnt/lustrefs/work/katie.renwick


# Copy Climate, CO2, Soils data and Gridlists to compute node
rsync -avhP $scriptsDir/Data $jobdir/


# Copy LPJGUESS executable
rsync -avhP $scriptsDir/guess /$jobdir 

# Copy code files
for fil in $scriptsDir/OptimDummy.ins $scriptsDir/optimize_parms.R $scriptsDir/runR.sh; do
	rsync -avhP $fil $jobdir/
done

# Make or copy output directories
mkdir $jobdir/Output_$jobname

###############

# Run R script
./runR.sh
wait


############################---------------------------------
outdir="./Output_$jobname/"
mv $jobdir/*.RData $jobdir/Output_$jobname
############################

# Copy output data to work directory
rsync -avhP $jobdir/Output_$jobname $outputDir
#For trouble-shooting:
#rsync -avhP $jobdir $outputDir

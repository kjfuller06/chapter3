#!/bin/bash
#PBS -N ext_fesm7
#PBS -A UWSY0001
#PBS -l select=1:ncpus=1:mem=25GB
#PBS -l walltime=05:00:00
#PBS -q regular
#PBS -j oe
#PBS -k oed
#PBS -r n

### Temp data to scratch
export TMPDIR=/glade/scratch/kjfuller/temp
mkdir -p $TMPDIR

### Load modules
module load R/4.1.2
module unload netcdf
module load geos
module load gdal
module load proj
module load peak_memusage

### Run the executable
peak_memusage.exe Rscript /glade/scratch/kjfuller/scripts/chapter3/ch3_extract_fesm_function.R --args code=701 num=701

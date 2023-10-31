#!/bin/bash
#PBS -N rast_hydro
#PBS -A UWSY0001
#PBS -l select=1:ncpus=6:mem=100GB
#PBS -l walltime=12:00:00
#PBS -q casper@casper-pbs
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
peak_memusage.exe Rscript /glade/scratch/kjfuller/scripts/chapter3/rasterize_hydro_nonpere.R

#!/bin/bash -l
#$ -V
#$ -l h_rt=4:00:00

module purge
source ~/.bashrc

Rscript remapPlot.R $1 $2

#!/bin/bash -l
#$ -V
#$ -l mem_total=50G
#$ -l h_rt=12:00:00
#$ -pe omp 28
#$ -j y

module purge
source ~/.bashrc

Rscript predictRF.R $1 $2


#!/bin/bash -l
#$ -V
#$ -l mem_total=50G
#$ -pe omp 28
#$ -l h_rt=4:00:00
#$ -j y

module purge
source ~/.bashrc



Rscript buildMap_v3.R $1 $2

# backup uses epochs
# plain is annual but lacks gap filling
# _v2 is annual and with gap filling but memory intensive
# _v3 does gap filling and is annual, not memory intensive but depends on cleaned precitions

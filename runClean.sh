#!/bin/bash
#$ -V
#$ -j y
#$ -pe omp 28
#$ -l h_rt=12:00:00
#$ -l mem_total=50G

Rscript cleanPredict.R $1 $2

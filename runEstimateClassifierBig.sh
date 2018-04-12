#!/bin/bash
#$ -V
#$ -pe omp 28
#$ -l mem_per_core=18G
#$ -l h_rt=12:00:00
#$ -j y
#$ -N bE_X24h_d4_15k

Rscript estimateClassifier_bigSample.R

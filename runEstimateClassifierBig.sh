#!/bin/bash
#$ -V
#$ -pe omp 28
#$ -l mem_per_core=9G
#$ -l h_rt=24:00:00
#$ -j y
#$ -N bE_24h_d4_15k

Rscript estimateClassifier_bigSample.R

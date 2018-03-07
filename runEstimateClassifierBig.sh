#!/bin/bash
#SBATCH --time=12:00:00
#SBATCH --mem-per-cpu=120G

Rscript estimateClassifier_bigSample.R

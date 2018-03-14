#!/bin/bash

## A script to take a tile and submit prediction, annual clean up, 
##	raster building, and remapping in sequence.

# $1 is tile
# $2 is model

# predict
qsub -N prd_$1 runPredict.sh $1 $2 

# clean
qsub -N cln_$1 -hold_jid prd_$1 runClean.sh $1 $2

# build
qsub -n bld_$1 -hold_jid cln_$1 runbuildmap.sh $1 $2
## build quick fix
#qsub -N bld_$1 runBuildMap.sh $1 $2

# remap
qsub -N rmp_$1 -hold_jid bld_$1 runRemapPlot.sh $1 $2
  
#FIRST=$(sbatch -J prd_$1 -o prd_$1 runPredict.sh $1 $2 | cut -f 4 -d' ')
#echo $FIRST
#
#SECOND=$(sbatch -J cln_$1 -o cln_$1 -d afterok:$FIRST runClean.sh $1 $2 | cut -f 4 -d' ')
#echo $SECOND
#
#THIRD=$(sbatch -J bld_$1 -o bld_$1 -d afterok:$SECOND runBuildMap.sh $1 $2 | cut -f 4 -d' ')
#echo $THIRD
#
#FOURTH=$(sbatch -J rmp_$1 -o rmp_$1 -d afterok:$THIRD runRemapPlot.sh $1 $2 | cut -f 4 -d' ')
#echo $FOURTH
#exit 0

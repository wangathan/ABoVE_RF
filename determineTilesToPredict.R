#################
#
#   Analyze tiles that are fully featured and fully predicted in order
#     to generate batch predict/mapping script.
#
#
#

library(data.table)
library(parallel)

rfid = commandArgs(TRUE)[1]
rfid = "tc_20180411_k50_pam_rf"

checker=function(tile,type){
  if(type=="feats"){
    inpath = feats[grep(tile,feats)]
    numfi = length(list.files(inpath))
  }
  if(type=="preds"){
    inpath = preds[grep(tile,preds)]
    numfi = length(list.files(inpath))
  }
  return(numfi==3000)
}

## get list of features
## determine which features are complete

feats = list.files("../../data/features/",
                   pattern="Bh.*[0-9]$",
                   full.names=T)

featdt = data.table(tilefeats = basename(feats))

featcompleted=mclapply(featdt$tilefeats,checker,type="feats",mc.cores=detectCores())
featdt[,completed:=featcompleted]

finishedfeat=featdt[completed==T,tilefeats]

## get list of predictions
## determine which ones are complete

preds= list.files(paste0("../../data/rf/predict_",rfid),
                   pattern="Bh.*[0-9]$",
                   full.names=T)

preddt = data.table(tilepreds = basename(preds))

predcompleted=mclapply(preddt$tilepreds,checker,type="preds",mc.cores=detectCores())
preddt[,completed:=predcompleted]

finishedpred=preddt[completed==T, tilepreds]

## subtract

predToDo = finishedfeat[! finishedfeat %in% finishedpred]

## write batch file

# to predict
batchPredictFile = data.table(tile = predToDo)

batchPredictFile[, tile := paste0("qsub runSeqPreBuiCleRem.sh ",tile," ",rfid)]
batchPredictFile = rbind(data.table(tile = "#!/bin/bash"),batchPredictFile)

write.table(batchPredictFile, "batchSeq_20180413.sh", row.names=F, col.names=F, quote=F)

batchPredictFile = data.table(tile = predToDo)
batchPredictFile[, tile := paste0("qsub runPredict.sh ",tile," ",rfid)]
batchPredictFile = rbind(data.table(tile = "#!/bin/bash"),batchPredictFile)
write.table(batchPredictFile, "batchPredict_20180413.sh",row.names=F,col.names=F,quote=F)

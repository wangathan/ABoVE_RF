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
rfid = "tc_20180416_noGeo_k55_pam_rf"

checker=function(tile,type){
  if(type=="feats"){
    inpath = feats[grep(tile,feats)]
    numfi = length(list.files(inpath))
  }
  if(type=="ccdc"){
    inpath = feath[grep(tile,feath)]
    numfi = length(list.files(inpath))
  }
  if(type=="preds"){
    inpath = preds[grep(tile,preds)]
    numfi = length(list.files(inpath))
  }
  return(numfi)
}

## get list of features
## determine which features are complete

feats = list.files("../../data/features/",
                   pattern="^Bh.*[0-9]$",
                   full.names=T)

preds= list.files(paste0("../../data/rf/predict_",rfid),
                   pattern="^Bh.*[0-9]$",
                   full.names=T)

feath = list.files(paste0("../../data/ccdc_feathers"),
                   pattern="^Bh.*[0-9]$",
                   full.names=T)

featdt = data.table(tilefeats = basename(feats))

featcompleted=unlist(mclapply(featdt$tilefeats,checker,type="feats",mc.cores=detectCores()))
featdt[,numfeat:=featcompleted]

ccdccompleted=unlist(mclapply(featdt$tilefeats,checker,type="ccdc",mc.cores=detectCores()))
featdt[,numccdc := ccdccompleted]

predcompleted=unlist(mclapply(featdt$tilefeats,checker,type="preds",mc.cores=detectCores()))
featdt[,numpred := predcompleted]

featdt[,badFeathers := ((numpred < numfeat) & numpred > 0)]

######
#
featherCatcher = function(ti){

  # get list of features
  # get list of predictions
  # which features not in predictions?

  features = list.files(paste0("../../data/features/",ti),
                        full.names=T)
  predictions = list.files(paste0("../../data/rf/predict_",rfid,"/",ti),
                           full.names=T)

  featIDs = strsplit(features,"_|c")
  featIDs = sapply(featIDs,"[[",3)

  predIDs = strsplit(predictions,"_")
  predIDs = sapply(predIDs,"[[",grep("rfpredict",predIDs[[1]])+1)

  badFeats = featIDs[! featIDs %in% predIDs]
  return(badFeats)

}
featherKiller = function(ti){
  
  # catch bad feathers
  catch = featherCatcher(ti)

  # KILL
  features = list.files(paste0("../../data/features/",ti),
                        full.names=T)
  targets = paste0("_c",catch,"_")
  targetIDs = sapply(targets, grep,features)
  if(length(targetIDs) > 100)return(NULL)

  return(file.remove(features[targetIDs]))

}

badTiles = featdt[badFeathers==T,tilefeats]
deleting = lapply(badTiles,featherKiller)

## get list of predictions
## determine which ones are complete

#preddt = data.table(tilepreds = basename(preds))
#
#predcompleted=mclapply(preddt$tilepreds,checker,type="preds",mc.cores=detectCores())
#preddt[,completed:=predcompleted]
#
#finishedpred=preddt[completed==T, tilepreds]
#
### subtract
#
#predToDo = finishedfeat[! finishedfeat %in% finishedpred]
#
### write batch file
#
## to predict
#batchPredictFile = data.table(tile = predToDo)
#
#batchPredictFile[, tile := paste0("qsub runSeqPreBuiCleRem.sh ",tile," ",rfid)]
#batchPredictFile = rbind(data.table(tile = "#!/bin/bash"),batchPredictFile)
#
#write.table(batchPredictFile, "batchSeq_20180413.sh", row.names=F, col.names=F, quote=F)
#
#batchPredictFile = data.table(tile = predToDo)
#batchPredictFile[, tile := paste0("qsub runPredict.sh ",tile," ",rfid)]
#batchPredictFile = rbind(data.table(tile = "#!/bin/bash"),batchPredictFile)
#write.table(batchPredictFile, "batchPredict_20180413.sh",row.names=F,col.names=F,quote=F)

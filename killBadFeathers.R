#################
#
#		Issues with data integity for feathers.
#
#		Maybe related to disk use problems from earlier?
#
#		Soln: delete and retry making features.
#

library(feather)
library(parallel)

# try reading all files and record the bad ones
featherTry = function(i){
  if(i%%100==0)print(i)
  return(class(try(read_feather(ffiles[i]),silent=T)))
}

featherkiller = function(ti){

  ffiles = list.files(paste0("../../data/features/",ti),
                      full.names=T)

  # takes about 5-10 mins to check
  featherCheck = mclapply(1:length(ffiles),featherTry,mc.cores=detectCores())
  problemFiles = which(featherCheck=="try-error")

  # delete problem files
  file.remove(ffiles[problemFiles])

  # which ones need redoing?
  if(length(problemFiles)>0)return(ti)
  if(length(ffiles)<3000)return(ti)
  return(NULL)

}
## run through all tiles
## keep track of the bad ones
## build a feature batch script?

## the tiles
feattiles = list.files("../../data/features/",
                       pattern="^Bh.*[0-9]$")

## clean them up
refeature = lapply(feattiles, featherkiller)

feathercheck = function(ti){

  inpath = 

}


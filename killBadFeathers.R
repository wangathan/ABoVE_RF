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

ti = commandArgs(TRUE)[1]

ffiles = list.files(paste0("../../data/features/",ti),
										full.names=T)

# try reading all files and record the bad ones
featherTry = function(i){
	if(i%%100==0)print(i)
	class(try(read_feather(ffiles[i]),silent=T))
}

# takes about 5-10 mins to check
featherCheck = mclapply(1:length(ffiles),featherTry,mc.cores=detectCores())
problemFiles = which(featherCheck=="try-error")

# delete problem files
file.remove(ffiles[problemFiles])

#################
#
#		Merge land cover labels into featureset
#
#
require(caret)
library(cluster)
library(stats)
#require(DMwR)
library(randomForest)
#library(unbalanced)
library(data.table)
library(foreach)
library(doParallel)
library(ranger)
#library(edarf)
library(treeClust)
library(raster)

set.seed(1337)
print(Sys.time())
# feature set
# coefsdt = fread("../../data/features/featureSet_20171102_hasFilterAndDiffAndEcozNormMonth.csv")
# coefsdt = fread("../../data/features/featureSet_20171201_hasFilterAndEcozAndDEMAndSW.csv")
# coefsdt = fread("../../data/features/featureSet_20180111_hasFilterAndEcozAndDEMAndSWAndChrom.csv")
# coefsdt = fread("../../data/features/featureSet_20180119_coreSample.csv")
coefsdt = fread("../../data/features/featureSet_20180411_coreSample.csv")

# labels
labelfs = list.files("../../data/landcover_labels",
										 full.names=T)

labelfs = labelfs[!grepl("BR",labelfs)]

labelsdt = rbindlist(lapply(labelfs,fread), use.names=T)

getPZI = function(i,dt){
  
  ti = dt[i, tile]
  px = dt[i, px]
  py = dt[i, py]
  
  if(!file.exists(paste0("../../data/PZI/tiles/",ti,"_PZI.tif")))
    return(NA)
  PZI_tile = raster(paste0("../../data/PZI/tiles/",ti,"_PZI.tif"))
  
  PZI = PZI_tile[px+1,py+1]
  return(PZI)
}

getBioclim = function(i, dt, bcvar){
  ti = dt[i, tile]
  px = dt[i, px]
  py = dt[i, py]
  
  bcfile = paste0("../../data/bioclim/tiles/",ti,"/",ti,"_",bcvar,".tif")
  if(!file.exists(bcfile))
    return(NA)
  bcrast = raster(bcfile)
  bcval = c(bcvar=bcrast[px+1, py+1])
  return(bcval)
}

labelsdt[,mergeid := paste0(tile,'-',samp)]
labelsdt=labelsdt[-1,]
coefsdt[,mergeid := paste0(set,'-',samp)]

setkey(coefsdt, mergeid)
setkey(labelsdt,mergeid)

# left outer join - keep the synthetic data, include as much year data as poss
coefs_labels = merge(coefsdt, labelsdt, all.x=T)

# manually do some of the years
coefs_labels[set == "toolik", year := 2015]
coefs_labels[set == "ES", year := 2015]
coefs_labels[set == "chasmer", year := 2010]

# keep tile naming good
setnames(coefs_labels, "tile.x", "tile")
setnames(coefs_labels, "samp.x", "samp")
coefs_labels[, c("samp.y", "tile.y"):=NULL]

# intersect year and segment range
# include skips? it's another 3000. 
 coefs_labels[, overlap := (year >= yr_start & year <= yr_end)]
 coefs_labels[, postyear := (year > 2014)]
 coefs_labels[, unchanging := nbreaks < 1]
 coefs_labels = coefs_labels[(overlap | postyear | unchanging) & (skipped != 1 | is.na(skipped)),]
# test = coefs_labels[(overlap | postyear) & (skipped != 1 | is.na(skipped)),]
# test = coefs_labels[(overlap | postyear | unchanging) & (skipped != 1 | is.na(skipped)),]

 coefs_labels = unique(coefs_labels)
 
 #coefs_labels = coefs_labels[(overlap | postyear) & skipped == 0 & surfaceType != 0,]

# get rid of soem garbage
coefs_labels[, c("completed", "leafType", "whySkipped") := NULL]
#coefs_labels = coefsdt
#coefs_labels = na.omit(coefs_labels) not yet - now doing lots of unlabelled data
coefs_labels[, ecozone := as.factor(ecozone)]

# some diagnostics
# remove 
# coefs_labels[, num_na := Reduce(`+`, lapply(.SD, function(x) is.na(x)))]

# coefs_labels = coefs_labels[num_na == 0,]

# one-hot encoding for categorical variables?
# only ecozone
coefs_labels[ecozone == "Boreal Shield", eco_BorealShield := as.factor(1)]
coefs_labels[ecozone != "Boreal Shield", eco_BorealShield := as.factor(0)]

coefs_labels[ecozone == "Boreal PLain", eco_BorealPlain := as.factor(1)]
coefs_labels[ecozone != "Boreal PLain", eco_BorealPlain := as.factor(0)]

coefs_labels[ecozone == "Bering Taiga", eco_BeringTaiga := as.factor(1)]
coefs_labels[ecozone != "Bering Taiga", eco_BeringTaiga := as.factor(0)]

coefs_labels[ecozone == "Bering Tundra", eco_BeringTundra := as.factor(1)]
coefs_labels[ecozone != "Bering Tundra", eco_BeringTundra := as.factor(0)]

coefs_labels[ecozone == "Intermontane Boreal", eco_IntermBor := as.factor(1)]
coefs_labels[ecozone != "Intermontane Boreal", eco_IntermBor := as.factor(0)]

coefs_labels[ecozone == "Alaska Range Transition", eco_AKRangeT := as.factor(1)]
coefs_labels[ecozone != "Alaska Range Transition", eco_AKRangeT := as.factor(0)]

coefs_labels[ecozone == "Aleutian Meadows", eco_AleutMead := as.factor(1)]
coefs_labels[ecozone != "Aleutian Meadows", eco_AleutMead := as.factor(0)]

coefs_labels[ecozone == "Coastal Rainforests", eco_CoastalRF := as.factor(1)]
coefs_labels[ecozone != "Coastal Rainforests", eco_CoastalRF := as.factor(0)]

coefs_labels[ecozone == "Arctic Tundra", eco_ArcTundra := as.factor(1)]
coefs_labels[ecozone != "Arctic Tundra", eco_ArcTundra := as.factor(0)]

coefs_labels[ecozone == "Pacific Mountains Transition", eco_PacMtnTra := as.factor(1)]
coefs_labels[ecozone != "Pacific Mountains Transition", eco_PacMtnTra := as.factor(0)]

coefs_labels[ecozone == "Coast Mountains Transition", eco_CoastMtnTra := as.factor(1)]
coefs_labels[ecozone != "Coast Mountains Transition", eco_CoastMtnTra := as.factor(0)]

coefs_labels[ecozone == "Boreal Cordillera", eco_BorCordill := as.factor(1)]
coefs_labels[ecozone != "Boreal Cordillera", eco_BorCordill := as.factor(0)]

coefs_labels[ecozone == "Pacific Maritime", eco_PacMarit := as.factor(1)]
coefs_labels[ecozone != "Pacific Maritime", eco_PacMarit := as.factor(0)]

coefs_labels[ecozone == "Taiga Cordillera", eco_TaigaCord := as.factor(1)]
coefs_labels[ecozone != "Taiga Cordillera", eco_TaigaCord := as.factor(0)]

coefs_labels[ecozone == "Montane Cordillera", eco_MontCord := as.factor(1)]
coefs_labels[ecozone != "Montane Cordillera", eco_MontCord := as.factor(0)]

coefs_labels[ecozone == "Taiga Plain", eco_TaigaPlain := as.factor(1)]
coefs_labels[ecozone != "Taiga Plain", eco_TaigaPlain := as.factor(0)]

coefs_labels[ecozone == "Southern Arctic", eco_SouthArctic := as.factor(1)]
coefs_labels[ecozone != "Southern Arctic", eco_SouthArctic := as.factor(0)]

coefs_labels[ecozone == "Northern Arctic", eco_NorthArctic := as.factor(1)]
coefs_labels[ecozone != "Northern Arctic", eco_NorthArctic := as.factor(0)]

coefs_labels[ecozone == "Prairie", eco_Prairie := as.factor(1)]
coefs_labels[ecozone != "Prairie", eco_Prairie := as.factor(0)]

coefs_labels[ecozone == "Taiga Shield", eco_TaigaShield := as.factor(1)]
coefs_labels[ecozone != "Taiga Shield", eco_TaigaShield := as.factor(0)]

coefs_labels[,ecozone := NULL]

allPZI = mclapply(1:nrow(coefs_labels),getPZI,dt=coefs_labels,mc.cores=detectCores())
allAMT <- mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "annMeanTemp",dt=coefs_labels,mc.cores=detectCores())
allMDR = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "meanDiurnalRange",dt=coefs_labels,mc.cores=detectCores())
allISO = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "isothermality",dt=coefs_labels,mc.cores=detectCores())
allTSeas = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "tempSeasonality",dt=coefs_labels,mc.cores=detectCores())
allWrmMax = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "warmestMax",dt=coefs_labels,mc.cores=detectCores())
allCldMin = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "coldestMin",dt=coefs_labels,mc.cores=detectCores())
allAnnRng = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "annualRange",dt=coefs_labels,mc.cores=detectCores())
allWetTemp = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "wettestTemp",dt=coefs_labels,mc.cores=detectCores())
allDryTemp = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "driestTemp",dt=coefs_labels,mc.cores=detectCores())
allWrmTemp = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "warmestTemp",dt=coefs_labels,mc.cores=detectCores())
allCldTemp = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "coldestTemp",dt=coefs_labels,mc.cores=detectCores())
allAnnPcp = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "annPrecip",dt=coefs_labels,mc.cores=detectCores())
allPcpSeas = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "precipSeasonality",dt=coefs_labels,mc.cores=detectCores())
allWetPcp = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "wettestPrecip",dt=coefs_labels,mc.cores=detectCores())
allDryPcp = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "driestPrecip",dt=coefs_labels,mc.cores=detectCores())
allWrmPcp = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "warmestPrecip",dt=coefs_labels,mc.cores=detectCores())
allCldPcp = mclapply(1:nrow(coefs_labels),getBioclim,bcvar = "coldestPrecip",dt=coefs_labels,mc.cores=detectCores())

coefs_labels[,PZI:=unlist(allPZI)]

coefs_labels[,annMeanTemp:=unlist(allAMT)]
coefs_labels[,mnDiurnalRange:=unlist(allMDR)]
coefs_labels[,isothermality:=unlist(allISO)]
coefs_labels[,tempSeasonality:=unlist(allTSeas)]
coefs_labels[,warmMax:=unlist(allWrmMax)]
coefs_labels[,coldMin:=unlist(allCldMin)]
coefs_labels[,annRange:=unlist(allAnnRng)]
coefs_labels[,wetTemp:=unlist(allWetTemp)]
coefs_labels[,dryTemp:=unlist(allDryTemp)]
coefs_labels[,wrmTemp:=unlist(allWrmTemp)]
coefs_labels[,cldTemp:=unlist(allCldTemp)]
coefs_labels[,annPcp:=unlist(allAnnPcp)]
coefs_labels[,pcpSeasonality:=unlist(allPcpSeas)]
coefs_labels[,wetPcp:=unlist(allWetPcp)]
coefs_labels[,dryPcp:=unlist(allDryPcp)]
coefs_labels[,wrmPcp:=unlist(allWrmPcp)]
coefs_labels[,cldPcp:=unlist(allCldPcp)]

coefs_labels[,GSL:=snowfall-snowmelt]

## calculate mean values

thebands = c("blue", "green", "red", "nir", "swir1", "swir2", "bt",
             "bcc", "gcc", "rcc",
             "ndvi", "evi", "nbr", "ndsi",
             "tcg", "tcw", "tcb",
             "tcwgd", "nbrevi")

themons = c("m1", "m2", "m3", "m3", "m5", "m6", "m7")

for(b in thebands){
  mnvals = rowMeans(coefs_labels[,paste0(themons,"_",b),with=F])
  coefs_labels[,paste0("mn_",b):=mnvals]
}


coefsnames = names(coefs_labels)
featureNames = coefsnames[!grepl("mergeid|unchanging|rowi|V1|px|py|start|end|^br$|a0_|c1_|a1_|b1_|a2_|b2_|a3_|b3_|magnitude_|tile|set|pixel|yr_start|yr_end|yr_br|^rmse|^i$|samp|ecozone|surfaceType|vegForm|phenotype|density|under|wetlandFlag|landUse|confidence|skipped|overlap|postyear|year",coefsnames)]

# tree clust is okay with missing values?
#coefs_labels = na.omit(coefs_labels)
#save(featureNames, file = "../../data/rf/featureNames")
#save(featureNames, file = "../../data/rf/featureNames_20180319")
#save(featureNames, file = "../../data/rf/featureNames_20180327")
save(featureNames, file = "../../data/rf/featureNames_20180411")

# deal with some NA

coefs_labels[,PZI := ifelse(is.na(PZI),mean(PZI, na.rm=T), PZI),by=tile]
ecoVars = featureNames[grep("eco_",featureNames)]
coefs_labels[,(ecoVars) := lapply(.SD, function(x)ifelse(is.na(x),0,x)),.SDcols = ecoVars]

# cycle through bioclim vars
bcvars = c("annMeanTemp", "annPcp","mnDiurnalRange",
           "warmMax", "wrmPcp", "wrmTemp",
           "coldMin", "cldPcp", "cldTemp",
           "wetPcp", "wetTemp","dryPcp", "dryTemp",
           "pcpSeasonality", "tempSeasonality", "isothermality", "annRange",
           "PZI")

for(bc in bcvars){ # impute by tile (or by everything if nothing from tile)
  print(bc)
  coefs_labels[,(bc) := ifelse(is.na(get(bc)),median(get(bc),na.rm=T),get(bc)),by=tile]
  coefs_labels[,(bc) := ifelse(is.na(get(bc)),median(get(bc),na.rm=T),get(bc))]
}

#coefs_labels = na.omit(coefs_labels)
save(coefs_labels, file="../../data/rf/clusters/tc_20180411_dt")

# d.num = 4 broke memory when big on discover
# how about on geo? breaks when d.num=4 and interactive with default memory
system.time(
coefs_tc <- treeClust(coefs_labels[,featureNames,with=F], 
										 d.num = 4,
										 control = treeClust.control(parallelnodes = detectCores()),
										 final.algorithm = "pam",
										 k = 50)
)

# sub indicates this is just for the labelled points (n = 6k)
#save(coefs_tc, file = "../../data/rf/clusters/tc_20180319_k50_d4")
#save(coefs_tc, file = "../../data/rf/clusters/tc_20180327_k50_d4_big")
save(coefs_tc, file = "../../data/rf/clusters/tc_20180411_k50_d4_big")
#save(coefs_tc, file = "../../data/rf/clusters/tc_20180219_k30")

print("CLUSTERS SAVED")

#
coefs_labels[, tcCluster := coefs_tc$final.clust$clustering]
coefs_labels[,tcCluster := as.factor(tcCluster)]

coefs_labels = coefs_labels[complete.cases(coefs_labels[,c("tcCluster",featureNames),with=F]),]

### do some train/test splitting for the random forests
clustsample = sample(1:nrow(coefs_labels), nrow(coefs_labels)/4)
clusttest = coefs_labels[clustsample,]
clusttrain= coefs_labels[!clustsample,]

clustrang13 = randomForest(tcCluster~., data =clusttrain[,c("tcCluster",featureNames),with=F], 
										 importance=T,
                     keep.forest=T)
clust13_pred_r = predict(clustrang13, clusttest[,featureNames,with=F])
clust13_prob_r = predict(clustrang13, clusttest[,featureNames,with=F],type="vote")
conf_rang13 = confusionMatrix(clust13_pred_r, clusttest$tcCluster)

save(conf_rang13, file = "../../data/rf/clusters/tc_20180411_k50_conf")
save(coefs_labels, file="../../data/rf/clusters/tc_20180411_k50_dt")
save(clusttest, file="../../data/rf/clusters/tc_20180411_k50_dt_test")
save(clusttrain, file="../../data/rf/clusters/tc_20180411_k50_dt_train")
save(clustrang13, file= "../../data/rf/model/tc_20180411_k50_pam_rf")

testcv = rfcv(clusttrain[,featureNames,with=F],
              clusttrain[,tcCluster],
              step = 0.8)

save(testcv, "../../data/rf/clusters/tc_20180411_k50_testcv")

pplot = partialPlot(x=clustrang13,pred.data=clusttrain[,c("tcCluster",featureNames),with=F])
save(pplot, '../../data/rf/clusters/tc_20180411_k50_partialPlot')

# importance
#imp = sort(importance(clustrang13),decreasing=T)
#impnames = names(imp)

#impdt = data.table(imp = imp, vari = impnames)

#ggplot(data = impdt[1:30,]) + geom_bar(aes(x = as.factor(vari), y = imp), stat="identity") 

# try it with random forest
#system.time(
#  unrf <- randomForest(coefs_labels[,featureNames,with=F],
#                      proximity=T)
#)
#
#save(unrf, file="../../data/rf/clusters/unrf_20180219")
#
## cluster
#system.time(
#  pam30 <- pam(1 - unrf$proximity, k=30, diss=T)
#)
#save(cluster_pam30, file = "../../data/rf/clusters/unrf_20180219_k30")








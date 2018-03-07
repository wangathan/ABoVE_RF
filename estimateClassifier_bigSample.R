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

set.seed(1337)
print(Sys.time())
# feature set
# coefsdt = fread("../../data/features/featureSet_20171102_hasFilterAndDiffAndEcozNormMonth.csv")
# coefsdt = fread("../../data/features/featureSet_20171201_hasFilterAndEcozAndDEMAndSW.csv")
# coefsdt = fread("../../data/features/featureSet_20180111_hasFilterAndEcozAndDEMAndSWAndChrom.csv")
coefsdt = fread("../../data/features/featureSet_20180119_coreSample.csv")
coefsnames = names(coefsdt)
# labels
labelfs = list.files("../../data/landcover_labels",
										 full.names=T)

labelfs = labelfs[!grepl("BR",labelfs)]

labelsdt = rbindlist(lapply(labelfs,fread), use.names=T)

# merge
setkey(coefsdt, set, samp)
setkey(labelsdt, tile,samp)

coefs_labels = coefsdt[labelsdt, nomatch=0]
#coefs_labels = coefsdt
# i dont' know why the first one is NA row... drop it
# coefs_labels = coefs_labels[-1,]

# intersect year and segment range
#coefs_labels[, overlap := (year >= yr_start & year <= yr_end)]
#coefs_labels[, postyear := (year > 2014)]

#coefs_labels = coefs_labels[(overlap | postyear) & skipped == 0 & surfaceType != 0,]

# get rid of soem garbage
coefs_labels[, c("completed", "leafType", "whySkipped") := NULL]
#coefs_labels = coefsdt
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

featureNames = coefsnames[!grepl("rowi|V1|px|py|start|end|^br$|a0_|c1_|a1_|b1_|a2_|b2_|a3_|b3_|magnitude_|tile|set|pixel|yr_start|yr_end|yr_br|^rmse|^i$|samp|ecozone",coefsnames)]
featureNames_coefs = coefsnames[grepl("robust",coefsnames)]

# tree clust is okay with missing values?
#coefs_labels = na.omit(coefs_labels)

save(featureNames, file = "../../data/rf/featureNames")

coefs_labels = na.omit(coefs_labels)

# d.num = 4 broke memory
system.time(
coefs_tc <- treeClust(coefs_labels[,featureNames,with=F], 
										 d.num = 4,
										 control = treeClust.control(parallelnodes = 28),
										 final.algorithm = "clara",
										 k = 28)
)

# sub indicates this is just for the labelled points (n = 6k)
save(coefs_tc, file = "../../data/rf/clusters/tc_20180219_k30_d4")
#save(coefs_tc, file = "../../data/rf/clusters/tc_20180219_k30")

#
coefs_labels[, tc_pam30 := coefs_tc$final.clust$clustering]
coefs_labels[,tc_pam30 := as.factor(tc_pam30)]
#
#coefs_labels = na.omit(coefs_labels)
### do some train/test splitting for the random forests
clustsample = sample(1:nrow(coefs_labels), nrow(coefs_labels)/3)
clusttest = coefs_labels[clustsample,]
clusttrain= coefs_labels[!clustsample,]
#
clustrang13prob = ranger(tc_pam30~., data =clusttrain[,c("tc_pam30",featureNames),with=F], 
										 importance="impurity",
                     probability=T)
clustrang13 = ranger(tc_pam30~., data =clusttrain[,c("tc_pam30",featureNames),with=F], 
										 importance="impurity")
clust13_pred_r = predict(clustrang13, clusttest[,featureNames,with=F])
clust13_pred_r_prob = predict(clustrang13prob, clusttest[,featureNames,with=F])
conf_rang13 = confusionMatrix(clust13_pred_r$predictions, clusttest$tc_pam30)

save(conf_rang13, file = "../../data/rf/clusters/tc_20180219_k30_conf")
save(coefs_labels, file="../../data/rf/clusters/tc_20180219_k30_dt")
save(clustrang13, file= "../../data/rf/model/tc_20180219_k30_rf")

# importance
#imp = sort(importance(clustrang13),decreasing=T)
#impnames = names(imp)

#impdt = data.table(imp = imp, vari = impnames)

#ggplot(data = impdt[1:30,]) + geom_bar(aes(x = as.factor(vari), y = imp), stat="identity") 

# try it with random forest
system.time(
  unrf <- randomForest(coefs_labels[,featureNames,with=F],
                      proximity=T)
)

save(unrf, file="../../data/rf/clusters/unrf_20180219")

# cluster
system.time(
  pam30 <- pam(1 - unrf$proximity, k=30, diss=T)
)
save(cluster_pam30, file = "../../data/rf/clusters/unrf_20180219_k30")








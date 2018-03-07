#################
#
#		A script to experiment with the impact of having N samples 
#		Also to experiment with the impact of having different numbers of clusters
#		Search the space and find an optimum
#
#			Goal graphs: change in accuracy with N, look for asymptote
#			Do it for k = 10, 11, ..., 19, 20.
#
#

require(caret)
library(cluster)
library(stats)
require(DMwR)
library(randomForest)
library(unbalanced)
library(data.table)
library(foreach)
library(doParallel)
library(ranger)
library(edarf)

set.seed(1337)
# feature set
# coefsdt = fread("../../data/features/featureSet_20171102_hasFilterAndDiffAndEcozNormMonth.csv")
coefsdt = fread("../../data/features/featureSet_20171102_hasFilterAndEcoz.csv")
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
# i dont' know why the first one is NA row... drop it
# coefs_labels = coefs_labels[-1,]

# intersect year and segment range
coefs_labels[, overlap := (year >= yr_start & year <= yr_end)]
coefs_labels[, postyear := (year > 2014)]

coefs_labels = coefs_labels[(overlap | postyear) & skipped == 0 & surfaceType != 0,]

# get rid of soem garbage
coefs_labels[, c("completed", "leafType", "whySkipped") := NULL]
coefs_labels[, ecozone := as.factor(ecozone)]

# some diagnostics
coefs_labels[, num_na := Reduce(`+`, lapply(.SD, function(x) is.na(x)))]
# remove 
coefs_labels = coefs_labels[num_na == 0,]

featureNames = coefsnames[!grepl("V1|px|py|start|end|^br$|a0_|c1_|a1_|b1_|a2_|b2_|a3_|b3_|magnitude_|tile|set|pixel|yr_start|yr_end|yr_br|^rmse|^i$|samp",coefsnames)]
featureNames_coefs = coefsnames[grepl("robust",coefsnames)]


testN = function(N){ # wrap RF/clustering algorithm in this function and produce a confusion matrix object

	samp = sample(1:nrow(coefs_labels), N)
	subdt = coefs_labels[samp,featureNames,with=F]

	unrf = randomForest(subdt,
											sampsize = nrow(subdt),	
											replace = F,		
											keep.forest=F, 
											proximity=T)

	clusters_pam10 = pam(1 - unrf$proximity, k = 10, diss=T)
	clusters_pam11 = pam(1 - unrf$proximity, k = 11, diss=T)
	clusters_pam12 = pam(1 - unrf$proximity, k = 12, diss=T)
	clusters_pam13 = pam(1 - unrf$proximity, k = 13, diss=T)
	clusters_pam14 = pam(1 - unrf$proximity, k = 14, diss=T)
	clusters_pam15 = pam(1 - unrf$proximity, k = 15, diss=T)
	clusters_pam5 = pam(1 - unrf$proximity, k = 5, diss=T)
	clusters_pam20 = pam(1 - unrf$proximity, k = 20, diss=T)

	pamdt = data.table(pam5 = clusters_pam5$clustering,
										 pam10 = clusters_pam10$clustering,
										 pam11 = clusters_pam11$clustering,
										 pam12 = clusters_pam12$clustering,
										 pam13 = clusters_pam13$clustering,
										 pam14 = clusters_pam14$clustering,
										 pam15 = clusters_pam15$clustering,
										 pam20 = clusters_pam20$clustering)

	clustsample = sample(1:nrow(pamdt), nrow(pamdt)/3)
	pamtrain = pamdt[!clustsample,]
	pamtest = pamdt[clustsample,]
	clusttrain = subdt[!clustsample,]
	clusttest = subdt[clustsample,]

	traindt = cbind(pamtrain$pam10, clusttrain)
	setnames(traindt, "V1", "pam10")
	clustrf10 = ranger(as.factor(pam10) ~ ., data = traindt) 

	traindt = cbind(pamtrain$pam11, clusttrain)
	setnames(traindt, "V1", "pam11")
	clustrf11 = ranger(as.factor(pam11) ~ ., data = traindt) 

	traindt = cbind(pamtrain$pam12, clusttrain)
	setnames(traindt, "V1", "pam12")
	clustrf12 = ranger(as.factor(pam12) ~ ., data = traindt) 

	traindt = cbind(pamtrain$pam14, clusttrain)
	setnames(traindt, "V1", "pam14")
	clustrf14 = ranger(as.factor(pam14) ~ ., data = traindt) 

	traindt = cbind(pamtrain$pam13, clusttrain)
	setnames(traindt, "V1", "pam13")
	clustrf13 = ranger(as.factor(pam13) ~ ., data = traindt) 

	traindt = cbind(pamtrain$pam15, clusttrain)
	setnames(traindt, "V1", "pam15")
	clustrf15 = ranger(as.factor(pam15) ~ ., data = traindt) 

	traindt = cbind(pamtrain$pam20, clusttrain)
	setnames(traindt, "V1", "pam20")
	clustrf20 = ranger(as.factor(pam20) ~ ., data = traindt) 

	traindt = cbind(pamtrain$pam5, clusttrain)
	setnames(traindt, "V1", "pam5")
	clustrf5 = ranger(as.factor(pam5) ~ ., data = traindt) 

	clust15_pred = predict(clustrf15, clusttest[,featureNames,with=F])$predictions
	clust10_pred = predict(clustrf10, clusttest[,featureNames,with=F])$predictions
	clust11_pred = predict(clustrf11, clusttest[,featureNames,with=F])$predictions
	clust12_pred = predict(clustrf12, clusttest[,featureNames,with=F])$predictions
	clust13_pred = predict(clustrf13, clusttest[,featureNames,with=F])$predictions
	clust14_pred = predict(clustrf14, clusttest[,featureNames,with=F])$predictions
	clust5_pred = predict(clustrf5, clusttest[,featureNames,with=F])$predictions
	clust20_pred = predict(clustrf20, clusttest[,featureNames,with=F])$predictions

	conf_pam15 = confusionMatrix(clust15_pred, pamtest$pam15)
	conf_pam5 = confusionMatrix(clust5_pred, pamtest$pam5) 
	conf_pam10 = confusionMatrix(clust10_pred, pamtest$pam10) 
	conf_pam11 = confusionMatrix(clust11_pred, pamtest$pam11)
	conf_pam12 = confusionMatrix(clust12_pred, pamtest$pam12)
	conf_pam13 = confusionMatrix(clust13_pred, pamtest$pam13)
	conf_pam14 = confusionMatrix(clust14_pred, pamtest$pam14)
	conf_pam20 = confusionMatrix(clust20_pred, pamtest$pam20)

	save(conf_pam5, file = paste0("../../data/rf/ntest/conf_pam5_n", N))
	save(conf_pam10, file = paste0("../../data/rf/ntest/conf_pam10_n", N))
	save(conf_pam11, file = paste0("../../data/rf/ntest/conf_pam11_n", N))
	save(conf_pam12, file = paste0("../../data/rf/ntest/conf_pam12_n", N))
	save(conf_pam13, file = paste0("../../data/rf/ntest/conf_pam13_n", N))
	save(conf_pam14, file = paste0("../../data/rf/ntest/conf_pam14_n", N))
	save(conf_pam15, file = paste0("../../data/rf/ntest/conf_pam15_n", N))
	save(conf_pam20, file = paste0("../../data/rf/ntest/conf_pam20_n", N))
	print(paste0("done with:", N))
}

testN(500)
testN(1000)
testN(1500)
testN(2000)
testN(2500)
testN(3000)
testN(3500)
testN(4000)
testN(4500)
testN(5000)


conf500 = get(load("../../data/rf/ntest/conf_pam14_n500"))
conf1000 = get(load("../../data/rf/ntest/conf_pam14_n1000"))
conf1500 = get(load("../../data/rf/ntest/conf_pam14_n1500"))
conf2000 = get(load("../../data/rf/ntest/conf_pam14_n2000"))
conf2500 = get(load("../../data/rf/ntest/conf_pam14_n2500"))
conf3000 = get(load("../../data/rf/ntest/conf_pam14_n3000"))
conf3500 = get(load("../../data/rf/ntest/conf_pam14_n3500"))
conf4000 = get(load("../../data/rf/ntest/conf_pam14_n4000"))
conf4500 = get(load("../../data/rf/ntest/conf_pam14_n4500"))

accdt = data.table(n = c(500,1000,1500,2000,2500,3000,3500,4000,4500),
									 acc = c(conf500$overall[1],
													 conf1000$overall[1],
													 conf1500$overall[1],
													 conf2000$overall[1],
													 conf2500$overall[1],
													 conf3000$overall[1],
													 conf3500$overall[1],
													 conf4000$overall[1],
													 conf4500$overall[1]))


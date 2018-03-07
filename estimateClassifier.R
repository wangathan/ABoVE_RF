#################
#
#		Merge land cover labels into featureset
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
# coefsdt = fread("../../data/features/featureSet_20171201_hasFilterAndEcozAndDEMAndSW.csv")
coefsdt = fread("../../data/features/featureSet_20180111_hasFilterAndEcozAndDEMAndSWAndChrom.csv")
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

#save(featureNames, file = "../../data/rf/featureNames")

# water
coefs_labels[(surfaceType == 1) | 
						 (density == 1 & under == 4),# | 
#						 (wetlandFlag == 3),
   	  			 LCMAP := "Water"]
#coefs_labels[(surfaceType == 2 & landUse == 1),
#						 LCMAP := "Developed"]
coefs_labels[(landUse == 2 | landUse == 3), 
						 LCMAP := "Cropland"]
coefs_labels[(surfaceType == 2 & landUse != 1) |
						 (surfaceType == 3 & density %in% c(1) & under == 1),
					   LCMAP := "Barren"]
coefs_labels[(surfaceType == 3 & vegForm == 4 & phenotype %in% c(1,3) & density %in% c(2,3)) & wetlandFlag == 1,
						 LCMAP := "Decid Forest"]
#coefs_labels[(surfaceType == 3 & vegForm == 4 & phenotype %in% c(3) & density %in% c(2,3)) & wetlandFlag == 1,
#						 LCMAP := "Mixed Forest"]
coefs_labels[(surfaceType == 3 & vegForm == 4 & phenotype == 2 & density %in% c(2,3)) & wetlandFlag == 1,
						 LCMAP := "Everg Forest"]

# coefs_labels[(surfaceType == 4 & vegForm == 3 & phenotype == 2 & density == 1 & wetlandFlag == 1),
# 							LCMAP := "Sparse Dry Forest"]

# coefs_labels[(surfaceType == 4 & vegForm == 3 & phenotype == 2 & density == 1 & wetlandFlag == 2),
# 							LCMAP := "Sparse Wetland Forest"]

coefs_labels[(surfaceType == 3 & vegForm %in% c(2) & density %in% c(2,3) & wetlandFlag == 1), 
						 LCMAP := "Grass"]
coefs_labels[(surfaceType == 3 & vegForm %in% c(3) & density %in% c(2,3) & wetlandFlag == 1),
						 LCMAP := "Shrub"]
coefs_labels[(surfaceType == 3 & wetlandFlag == 3),
						 LCMAP := "Shallow Lake"]
#coefs_labels[(surfaceType == 3 & vegForm %in% c(1) & density %in% c(1,2,3) & wetlandFlag == 2),
#						 LCMAP := "Bog"]
#coefs_labels[(surfaceType == 3 & vegForm %in% c(2) & density %in% c(1,2,3) & wetlandFlag == 2),
#						 LCMAP := "Fen"]
coefs_labels[(surfaceType == 3 & vegForm %in% c(1,2,3,4) & density %in% c(1,2,3) & wetlandFlag == 2),
						 LCMAP := "Wetland"]
#coefs_labels[(surfaceType == 4 | snowiness > 0.7),
#						 LCMAP := "Snow/ice"]
#coefs_labels[landUse == 5,
#						 LCMAP := "Disturbed/transitional"]

coefs_labels[, LCMAP := as.factor(LCMAP)]

## testing training split



### with spectral-temporal features

LCMAPdt_f = na.omit(coefs_labels[,c("LCMAP", featureNames),with=F])
testsample = sample(1:nrow(LCMAPdt_f), nrow(LCMAPdt_f)/3)

LCMAPdt_test = LCMAPdt_f[testsample,]
LCMAPdt_train= LCMAPdt_f[!testsample,]

#LCMAPdt_train = LCMAPdt_train[c(wetlandkeep, notrows),]
## unsupervised
unrf = randomForest(coefs_labels[,featureNames,with=F],
									  sampsize = nrow(coefs_labels),
									  replace = F,	
										keep.forest=F, 
										proximity=T)
#unrf = ranger(coefs_labels[,featureNames,with=F])
#save(unrf, file="../../data/rf/clusters/unrf_20171201")
#load("../../data/rf/clusters/unrf")

#prox = extract_proximity(unrf)
#unrf_pca = princomp(prox)

# plot(unrf_pca$scores[,1], unrf_pca$scores[,2])
# plot(unrf_pca$scores[,3], unrf_pca$scores[,2])
# plot(unrf_pca$scores[,3], unrf_pca$scores[,4])
# plot(unrf_pca$scores[,2], unrf_pca$scores[,4])
# plot(unrf_pca$scores[,4], unrf_pca$scores[,5])
# plot(unrf_pca$scores[,5], unrf_pca$scores[,6])
# plot(unrf_pca$scores[,2], unrf_pca$scores[,5])

#plot_prox(unrf_pca)
#mds = MDSplot(unrf, LCMAPdt_f$LCMAP, k = 10)

# to keep things stable/deterministic for now:

# from 20171102 version
#med5=c(3523,4766,4920,5312,4142)
#med10=c(3523,2609,4766,4920,4632,3270,4325,1454,4142,1274)
#med11=c(3523,2609,4766,4920,4632,3270,4022,2649,1454,4142,1274)
#med12=c(3523,2609,4766,4920,4632,3270,4022,2649,1190,4142,616,1274)
#med13=c(3228,5167,4766,4920,4632,2297,4022,2649,476,4142,616,1274,2967)
#med14=c(3228,4757,4766,4920,4632,2297,4022,2609,2649,1454,4142,351,616,1274)
#med15=c(3228,2609,4766,4632,5043,2297,4325,4473,1454,4142,351,616,1274,5449,2967)
#med20=c(3228,2609,4766,4632,4354,3556,4325,5043,2649,1454,4163,351,2470,616,1274,5449,4406,2006,2967,4142)

# from 20171201 version
med10 = c(4367,2405,5377,1584,2027,3470,6040,4839,966,5330)
med11 = c(4367,2405,5377,5282,1331,6040,2027,3336,3334,4839,
					966)
med12 = c(4367,2405,5377,4107,6040,2027,1256,3336,3334,4839,
					966,5330)
med13 = c(189,2405,5377,4107,6040,4367,3706,1256,3336,3334,
					4839,966,5330)
med14 = c(189,6062,4065,5377,5684,6040,4367,3706,3334,4839,
					966,3583,3336,5330)
med15 = c(189,6062,4065,5377,6040,4367,3706,1256,3334,822,
					966,3583,3336,5330,4700)
med20 = c(5837,5377,1584,4594,4367,2027,813,3470,2405,6040,
					4065,2863,3788,2310,2066,3520,1331,966,4839,5330)

# from 20180111 version
#med10 = c(4367,2405,5377,1584,2027,3470,6040,4839,966,5330)
#med11 = c(4367,2405,5377,5282,1331,6040,2027,3336,3334,4839,
#					966)
#med12 = c(4367,2405,5377,4107,6040,2027,1256,3336,3334,4839,
#					966,5330)
#med13 = c(189,2405,5377,4107,6040,4367,3706,1256,3336,3334,
#					4839,966,5330)
#med14 = c(189,6062,4065,5377,5684,6040,4367,3706,3334,4839,
#					966,3583,3336,5330)
#med15 = c(189,6062,4065,5377,6040,4367,3706,1256,3334,822,
#					966,3583,3336,5330,4700)
med20 = c(1678, 2924, 5665, 4247, 4086, 542,  519,  3739, 182,  4653,
					2863, 5048, 4726, 967,  5815, 3507, 3776, 5159, 2197, 5346)


#clusters_pam = pam(1 - unrf$proximity, k = 10, diss=T)#, medoids=med10)
#clusters_pam11 = pam(1 - unrf$proximity, k = 11, diss=T)#, medoids=med11)
#clusters_pam12 = pam(1 - unrf$proximity, k = 12, diss=T)#, medoids=med12)
#clusters_pam13 = pam(1 - unrf$proximity, k = 13, diss=T)#, medoids=med13)
#clusters_pam14 = pam(1 - unrf$proximity, k = 14, diss=T)#, medoids=med14)
#clusters_pam15 = pam(1 - unrf$proximity, k = 15, diss=T)#, medoids=med15)
#clusters_pam5 = pam(1 - unrf$proximity, k = 5, diss=T)#, medoids=med5)
clusters_pam20 = pam(1 - unrf$proximity, k = 20, diss=T)#,medoids=med20)

#dir.create("../../data/rf/clusters_20171201/")
#save(clusters_pam, file = "../../data/rf/clusters_20171201/clusters_pam")
#save(clusters_pam11, file = "../../data/rf/clusters_20171201/clusters_pam11")
#save(clusters_pam12, file = "../../data/rf/clusters_20171201/clusters_pam12")
#save(clusters_pam13, file = "../../data/rf/clusters_20171201/clusters_pam13")
#save(clusters_pam14, file = "../../data/rf/clusters_20171201/clusters_pam14")
#save(clusters_pam15, file = "../../data/rf/clusters_20171201/clusters_pam15")
#save(clusters_pam20, file = "../../data/rf/clusters_20171201/clusters_pam20")
save(clusters_pam20, file = "../../data/rf/clusters_20180111/clusters_pam20")
#save(clusters_pam5, file = "../../data/rf/clusters_20171201/clusters_pam5")


load(file = "../../data/rf/clusters_20171201/clusters_pam20")

# dendo = hclust(as.dist(1 - unrf$proximity))
# this looks like garbage!
# do some train/test splitting for the random forests
clustsample = sample(1:nrow(coefs_labels), nrow(coefs_labels)/3)
clusttest = coefs_labels[clustsample,]
clusttrain= coefs_labels[!clustsample,]

pamdt = data.table(#pam5 = clusters_pam5$clustering,
									 #pam10 = clusters_pam$clustering,
									 #pam11 = clusters_pam11$clustering,
									 #pam12 = clusters_pam12$clustering,
									 #pam13 = clusters_pam13$clustering,
									 #pam14 = clusters_pam14$clustering,
									 #pam15 = clusters_pam15$clustering,
									 pam20 = clusters_pam20$clustering)

#pamdt[, paste0("ch_",names(pamdt)):=lapply(.SD, convertToWords), by=1:nrow(pamdt)]

pamtrain = pamdt[!clustsample,]
pamtest = pamdt[clustsample,]

##chclustrf15 = randomForest(x = clusttrain[,featureNames,with=F], 
##											 y = as.factor(pamtrain$ch_pam15))
#clustrf15 = randomForest(x = clusttrain[,featureNames,with=F], 
#											 y = as.factor(pamtrain$pam15))
##											 keep.forest=T,
##											 proximity=T)
#
#clustrf11 = randomForest(x = clusttrain[,featureNames,with=F], 
#											 y = as.factor(pamtrain$pam11),
#											 keep.forest=T,
#											 proximity=T)
#
#clustrf12 = randomForest(x = clusttrain[,featureNames,with=F], 
#											 y = as.factor(pamtrain$pam12),
#											 keep.forest=T,
#											 proximity=T)
#
#clustrf13 = randomForest(x = clusttrain[,featureNames,with=F], 
#												 y = as.factor(pamtrain$pam13),
#												 keep.forest=T,
#												 proximity=T)
#
#clustrf14 = randomForest(x = clusttrain[,featureNames,with=F], 
#											 y = as.factor(pamtrain$pam14),
#											 keep.forest=T,
#											 proximity=T)
#
#clustrf10 = randomForest(x = clusttrain[,featureNames,with=F], 
#											 y = as.factor(pamtrain$pam10),
#											 keep.forest=T,
#											 proximity=T)
#
#clustrf5 = randomForest(x = clusttrain[,featureNames,with=F], 
#											 y = as.factor(pamtrain$pam5),
#											 keep.forest=T,
#											 proximity=T)

clustrf20 = randomForest(x = clusttrain[,featureNames,with=F], 
											 y = as.factor(pamtrain$pam20),
											 keep.forest=T,
											 proximity=T)

save(clustrf10, file = "../../data/rf/model/clustrf10_20171201")
save(clustrf11, file = "../../data/rf/model/clustrf11_20171201")
save(clustrf12, file = "../../data/rf/model/clustrf12_20171201")
save(clustrf13, file = "../../data/rf/model/clustrf13_20171201")
save(clustrf14, file = "../../data/rf/model/clustrf14_20171201")
save(clustrf15, file = "../../data/rf/model/clustrf15_20171201")
save(clustrf20, file = "../../data/rf/model/clustrf20_20180111")
#save(clustrf20, file = "../../data/rf/model/clustrf20_20171201")
save(clustrf5, file = "../../data/rf/model/clustrf5_20171201")

load("../../data/rf/model/clustrf20_20171201")

#clust13dt = data.table(clust = as.factor(pamtrain$pam13))
#clust13dt = cbind(clust13dt, clusttrain[,featureNames,with=F])
#clustrang13 = ranger(clust~., data = clust13dt, 
#										 always.split.variables = c("ecozone"),
#										 importance="impurity")

#clust13_pred_r = predict(clustrang13, clusttest[,featureNames,with=F])
#conf_rang13 = confusionMatrix(clust13_pred_r$predictions, pamtest$pam13)

clust15_pred = predict(clustrf15, clusttest[,featureNames,with=F])
clust10_pred = predict(clustrf10, clusttest[,featureNames,with=F])
clust11_pred = predict(clustrf11, clusttest[,featureNames,with=F])
clust12_pred = predict(clustrf12, clusttest[,featureNames,with=F])
clust13_pred = predict(clustrf13, clusttest[,featureNames,with=F])
clust14_pred = predict(clustrf14, clusttest[,featureNames,with=F])
clust5_pred = predict(clustrf5, clusttest[,featureNames,with=F])
clust20_pred = predict(clustrf20, clusttest[,featureNames,with=F])

clust20_dt = data.table(lcmap = as.numeric(as.character(clust20_pred)))
clust20_dt[lcmap %in% c(5,6,9,13), newlc:= "Everg F"] #Everg       
clust20_dt[lcmap %in% c(8), newlc:= "Decid F"] #Decid           
clust20_dt[lcmap %in% c(7), newlc:= "Mixed F"] #Mixed          
clust20_dt[lcmap %in% c(3,4), newlc:= "Woodland"] #Woodland      
clust20_dt[lcmap %in% c(17, 19, 16), newlc:= "Grass"] #Grass      
clust20_dt[lcmap %in% c(10,15,20), newlc:= "Wetland"] #Wetland  
clust20_dt[lcmap %in% c(1,11), newlc:= "Woody Wetland"] #Woody Wetland
clust20_dt[lcmap %in% c(12,14), newlc:= "Barren"] #Barren      
clust20_dt[lcmap %in% c(18), newlc:= "Water"] #Water          
clust20_dt[lcmap %in% c(2), newlc:= "Shallows"] #Shallows       
clust20_dt[lcmap %in% c(21), newlc:= "Shadow"] #Shadow        
clust20_dt[,pamtest := as.numeric(as.character(pamtest$pam20))]
clust20_dt[pamtest %in% c(5,6,9,13), pam_remap:= "Everg F"] #Everg       
clust20_dt[pamtest %in% c(8), pam_remap:= "Decid F"] #Decid           
clust20_dt[pamtest %in% c(7), pam_remap:= "Mixed F"] #Mixed          
clust20_dt[pamtest %in% c(3,4), pam_remap:= "Woodland"] #Woodland      
clust20_dt[pamtest %in% c(17, 19, 16), pam_remap:= "Grass"] #Grass      
clust20_dt[pamtest %in% c(10,15,20), pam_remap:= "Wetland"] #Wetland  
clust20_dt[pamtest %in% c(1,11), pam_remap:= "Woody Wetland"] #Woody Wetland
clust20_dt[pamtest %in% c(12,14), pam_remap:= "Barren"] #Barren      
clust20_dt[pamtest %in% c(18), pam_remap:= "Water"] #Water          
clust20_dt[pamtest %in% c(2), pam_remap:= "Shallows"] #Shallows       
clust20_dt[pamtest %in% c(21), pam_remap:= "Shadow"] #Shadow        

conf_pam15 = confusionMatrix(clust15_pred, pamtest$pam15) # overall acc: 68%
conf_pam5 = confusionMatrix(clust5_pred, pamtest$pam5) # overall acc: 79%
conf_pam10 = confusionMatrix(clust10_pred, pamtest$pam10) # overall acc: 73%
conf_pam11 = confusionMatrix(clust11_pred, pamtest$pam11) # overall acc: 73%
conf_pam12 = confusionMatrix(clust12_pred, pamtest$pam12) # overall acc: 71%
conf_pam13 = confusionMatrix(clust13_pred, pamtest$pam13) # overall acc: 70%
conf_pam14 = confusionMatrix(clust14_pred, pamtest$pam14) # overall acc: 70%
conf_pam20 = confusionMatrix(clust20_pred, pamtest$pam20) # overall acc: 64%

conf_pam20 = confusionMatrix(clust20_dt$newlc, clust20_dt$pam_remap) # overall acc: 64%
save(conf_pam20, file = "../../data/rf/clusters_20171201/conf_pam20_remap")

accdt = data.table(k = c(5,10,11,12,13,14,15,20),
									 acc = c(conf_pam5$overall[1],conf_pam10$overall[1],conf_pam11$overall[1],conf_pam12$overall[1],conf_pam13$overall[1],conf_pam14$overall[1],conf_pam15$overall[1],conf_pam20$overall[1]),
									 kap = c(conf_pam5$overall[2],conf_pam10$overall[2],conf_pam11$overall[2],conf_pam12$overall[2],conf_pam13$overall[2],conf_pam14$overall[2],conf_pam15$overall[2],conf_pam20$overall[2]))

save(conf_pam5, file = "../../data/rf/clusters_20171201/conf_pam5")
save(conf_pam10, file = "../../data/rf/clusters_20171201/conf_pam10")
save(conf_pam11, file = "../../data/rf/clusters_20171201/conf_pam11")
save(conf_pam12, file = "../../data/rf/clusters_20171201/conf_pam12")
save(conf_pam13, file = "../../data/rf/clusters_20171201/conf_pam13")
save(conf_pam14, file = "../../data/rf/clusters_20171201/conf_pam14")
save(conf_pam15, file = "../../data/rf/clusters_20171201/conf_pam15")
save(conf_pam20, file = "../../data/rf/clusters_20171201/conf_pam20")

# accuracies
# 5  : 82.6
# 10 : 77.9 
# 11 : 76.8
# 12 : 77.3
# 13 : 77.2
# 14 : 75.6
# 15 : 76.4
# 20 : 74.7


system.time(lcmaprf_f <- randomForest(LCMAP ~ ., data = LCMAPdt_train, mtry=30, ntrees=500)) # ... about 80 s
#system.time(lcmaprf_f_ranger <- ranger(LCMAP ~ ., 
#																			 data = LCMAPdt_train, 
#																			 mtry=30, 
#																			 num.trees=500, 
#																			 always.split.variables="ecozone", 
#																			 importance="impurity"))  # ... about 12 s

system.time(lcmap_pred <- predict(lcmaprf_f, LCMAPdt_test))
lcmapconf = confusionMatrix(lcmap_pred, LCMAPdt_test$LCMAP)

#system.time(lcmap_pred_r <- predict(lcmaprf_f_ranger, LCMAPdt_test)$predictions)
#lcmapconf_r = confusionMatrix(lcmap_pred_r, LCMAPdt_test$LCMAP)

#wetlandrows = which(LCMAPdt_train$LCMAP == "Wetland")
#notrows = which(!LCMAPdt_train$LCMAP %in% c("Wetland", "Disturbed/transitional"))
#wetlandkeep = sample(wetlandrows, length(wetlandrows)-150)

#LCMAPdt_smote = SMOTE(LCMAP ~ ., LCMAPdt_train, perc.over = 1000, perc.under =1000) 
#lcmaprf_smote = randomForest(LCMAP ~ ., data = LCMAPdt_smote)

#system.time(smote_pred <- predict(lcmaprf_smote, LCMAPdt_test))
#smoteconf = confusionMatrix(smote_pred, LCMAPdt_test$LCMAP)

#noamp_train = LCMAPdt_train[,names(LCMAPdt_train)[!grepl("LCMAP|ecozone",names(LCMAPdt_train))],with=F]
#noamp_test = LCMAPdt_test[,names(LCMAPdt_test)[!grepl("LCMAP|ecozone",names(LCMAPdt_test))],with=F]

#lcmappca = prcomp(as.matrix(noamp_train),
#									center=T,
#									scale.=T)

#lcmap_pc_train = predict(lcmappca, noamp_train)
#lcmap_pc_test = predict(lcmappca, noamp_test)
#lcmap_pcdt_train = as.data.table(cbind.data.frame(LCMAPdt_train$LCMAP, lcmap_pc_train[,1:15]))
#lcmap_pcdt_test  = as.data.table(cbind.data.frame(LCMAPdt_test$LCMAP, lcmap_pc_test[,1:15]))

#setnames(lcmap_pcdt_train, "LCMAPdt_train$LCMAP", "LCMAP")
#setnames(lcmap_pcdt_test, "LCMAPdt_test$LCMAP", "LCMAP")

#rf_pc = randomForest(LCMAP ~ ., data = lcmap_pcdt_train, mtry = 8)

#save(surfacerf, file="../../data/rf/model/surfacerf_f")
#save(vegrf, file="../../data/rf/model/vegrf_f")
#save(wetrf, file="../../data/rf/model/wetrf_f")
#save(phenorf, file="../../data/rf/model/phenorf_f")
#save(densityrf, file="../../data/rf/model/densityrf_f")
#save(underrf, file="../../data/rf/model/underrf_f")
#save(landUserf, file="../../data/rf/model/landUserf_f")
#save(lcmaprf_f, file="../../data/rf/model/lcmaprf_f_notrans")
#save(lcmaprf, file="../../data/rf/model/lcmaprf")
#save(lcmaprf_smote, file="../../data/rf/model/lcmaprf_smote")
#save(LCMAPdt_train, file="../../data/rf/model/lcmapdt_train")

#load("../../data/rf/model/lcmaprf_f")

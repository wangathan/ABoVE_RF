#################
#
#		take a set of clusterings and data and determine some spectral profiles of each cluster	
#
#
require(caret)
require(DMwR)
library(randomForest)
library(unbalanced)
library(data.table)
library(foreach)
library(doParallel)
library(ranger)
library(edarf)

assignLambda = function(x){
	if(x=="blue")return(0.485)
	if(x=="green")return(0.56)
	if(x=="red")return(0.66)
	if(x=="nir")return(0.84)
	if(x=="swir1")return(1.65)
	if(x=="swir2")return(2.40)
	if(x=="bt")return(3)
	if(x=="ndvi")return(3.25)
	if(x=="evi")return(3.5)
	if(x=="nbrevi")return(3.75)
	if(x=="nbr")return(4)
	if(x=="tcg")return(4.25)
	if(x=="tcb")return(4.5)
	if(x=="tcw")return(4.75)
	if(x=="tcwgd")return(5)
	if(x=="nbreaks")return(5.25)
	if(x=="seglength")return(5.5)
	if(x=="snowiness")return(5.75)
	if(x=="cloudiness")return(6)
	if(x=="snowmelt")return(8.25)
	if(x=="snowfall")return(8)
	if(x=="elv")return(6.25)
	if(x=="asp")return(6.5)
	if(x=="slp")return(6.75)
	if(x=="swocc")return(7)
	if(x=="swrec")return(7.25)
	if(x=="swsea")return(7.5)
	if(x=="swext")return(7.75)
}

banddt = data.table(lambda = c(0.485, 0.56, 0.66, 0.84, 1.65, 2.4, 3,
															 3.25, 3.5, 3.75, 4, 4.25, 4.5, 4.75, 5,
															 5.25, 5.5, 5.75, 6, 6.25, 6.5, 6.75, 7,
															 7.25, 7.5, 7.75, 8, 8.25, 8.5, 8.75, 9),
										band = c("blue", "green", "red", "nir", "swir1", "swir2", "bt",
														 "ndvi", "evi", "nbrevi", "nbr", "tcg", "tcb", "tcw",
														 "tcwgd", "bcc", "gcc", "rcc", "elv", "asp", "slp",
														 "nbreaks", "seglength", "snowiness","cloudiness", "snowmelt", "snowfall", "swocc", "swrec", "swsea", "swext")) 
														 


clustPlotter = function(clustRange, medoidmelt){
	clustprof = ggplot() + 
		geom_ribbon(data = dcast(medoidmelt[metr %in% c("max", "min") & clust %in% clustRange,],
														 lambda + clust + band ~ metr, value.var="value"),
								aes(x = lambda, ymin = min, ymax = max), 
								fill = "#CCCCCC") + facet_grid(clust ~ .) +
#		geom_ribbon(data = dcast(medoidmelt[metr %in% c("q25", "q75") & clust %in% clustRange,],
#														 lambda + clust + band ~ metr, value.var="value"),
#											 aes(x = lambda, ymin = q25, ymax = q75), 
#					 						 fill = "#888888") +
		geom_line(data = medoidmelt[metr=="m2" & clust %in% clustRange,], aes(x = lambda, y = value)) + 
		geom_point(data = medoidmelt[metr=="m2" & clust %in% clustRange,], aes(x = lambda, y = value)) +
#		geom_bar(stat='identity',data = medoidmelt[metr%in%c("snowiness","nbreaks","seglength","cloudiness","elv","asp","slp","swocc","swrec","swext","swsea") & clust %in% clustRange,], aes(x = lambda, y = value)) +
		geom_jitter(data = medoidmelt[metr %in% c("trnd","amp","rmse") & clust %in% clustRange & abs(value) > 0.01],
								aes(x = lambda, y = value, col = metr)) + 
		geom_text(aes(x,y,label = LCMAP), data= labdat[clust %in% clustRange,]) +
		theme_bw() + 
		xlab("") +  ylab("") + ggtitle(paste0("Feature profile for clusters ",min(clustRange),":",max(clustRange))) + 
		#scale_x_continuous(breaks = banddt[lambda < 7,lambda],
		#									 labels = banddt[lambda < 7, band]) +
		scale_x_continuous(breaks=banddt$lambda, labels=banddt$band) + 
#											 c(0.485, 0.560, 0.660, 0.840, 1.650, 2.4, 3, 3.25, 3.5, 3.75, 4, 4.25, 4.5, 4.75, 5, 5.25, 5.5, 5.75, 6, 6.25, 6.5, 6.75, 7, 7.25, 7.5, 7.75),
#											 labels = c("B", "G", "R", "nir", "swir1", "swir2", "bt", "ndvi", "evi", "nbrevi", "nbr", "tcg", "tcb", "tcw", "tcwgd", "nseg*10", "segl*10000", "snow", "cloud", "elv", "asp", "slp", "swocc", "swrec", "swsea", "swext")) + 
		ylim(-0.35,1) + 
		facet_grid(clust ~ .) + 
		theme(panel.grid.minor.x = element_blank(), 
					axis.text = element_text(size=11), 
					axis.text.x = element_text(angle = -45,vjust=1, hjust=0))
	return(clustprof)
}

# feature set
# coefsdt = fread("../../data/features/featureSet_20171102_hasFilterAndDiffAndEcozNormMonth.csv")
# coefsdt = fread("../../data/features/featureSet_20171102_hasFilterAndEcoz.csv")
# coefsdt = fread("../../data/features/featureSet_20171201_hasFilterAndEcozAndDEMAndSW.csv")
# coefsdt = fread("../../data/features/featureSet_20180111_hasFilterAndEcozAndDEMAndSWAndChrom.csv")
# coefs_labels = get(load("../../data/rf/clusters/tc_20180219_k30_dt"))
coefs_labels = get(load("../../data/rf/clusters/tc_20180219_k20_dt_sub"))

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
coefs_labels[(surfaceType == 3 & vegForm == 4 & phenotype %in% c(1) & density %in% c(2,3)) & wetlandFlag == 1,
						 LCMAP := "Decid Forest"]
coefs_labels[(surfaceType == 3 & vegForm == 4 & phenotype %in% c(3) & density %in% c(2,3)) & wetlandFlag == 1,
						 LCMAP := "Mixed Forest"]
coefs_labels[(surfaceType == 3 & vegForm == 4 & phenotype == 2 & density %in% c(3)) & wetlandFlag == 1,
						 LCMAP := "Everg Forest"]
coefs_labels[(surfaceType == 3 & vegForm == 4 & phenotype == 2 & density %in% c(2,1)) & wetlandFlag == 1 & under == 1,
						 LCMAP := "Peat Woodland"]
coefs_labels[(surfaceType == 3 & vegForm %in% c(2,3) & landUse == 5),
						 LCMAP := "Post-fire"]
coefs_labels[(surfaceType == 3 & vegForm == 4 & landUse == 5),
						 LCMAP := "Secondary Forest"]
# coefs_labels[(surfaceType == 4 & vegForm == 3 & phenotype == 2 & density == 1 & wetlandFlag == 1),
# 							LCMAP := "Sparse Dry Forest"]

coefs_labels[(surfaceType == 3 & vegForm %in% c(3) & under %in% c(2,3) & density %in% c(2,1) & wetlandFlag == 1),
						 LCMAP := "Open Shrubs"]
coefs_labels[(surfaceType == 3 & vegForm %in% c(1,2) & density %in% c(1,2) & under == 1),
						 LCMAP := "Sparsely Vegetated"]
coefs_labels[(surfaceType == 3 & vegForm %in% c(2,3) & landUse == 5),
						 LCMAP := "Post-fire"]
# coefs_labels[(surfaceType == 4 & vegForm == 3 & phenotype == 2 & density == 1 & wetlandFlag == 2),
# 							LCMAP := "Sparse Wetland Forest"]

coefs_labels[(surfaceType == 3 & vegForm %in% c(2) & density %in% c(2,3) & wetlandFlag == 1), 
						 LCMAP := "Grass"]
coefs_labels[(surfaceType == 3 & vegForm %in% c(3) & density %in% c(2,3) & wetlandFlag == 1),
						 LCMAP := "Shrub"]
coefs_labels[(wetlandFlag == 3),
						 LCMAP := "Shallow Lake"]
coefs_labels[(surfaceType == 3 & vegForm %in% c(1) & density %in% c(1,2,3) & wetlandFlag == 2),
						 LCMAP := "Bog"]
coefs_labels[(surfaceType == 3 & vegForm %in% c(2) & density %in% c(1,2,3) & wetlandFlag == 2),
						 LCMAP := "Fen"]
coefs_labels[(surfaceType == 3 & vegForm %in% c(3,4) & density %in% c(1,2,3) & wetlandFlag == 2),
						 LCMAP := "Woody Wetland"]
#coefs_labels[(surfaceType == 3 & vegForm %in% c(1,2,3,4) & density %in% c(1,2,3) & wetlandFlag == 2),
#						 LCMAP := "Wetland"]
#coefs_labels[(surfaceType == 4 | snowiness > 0.7),
#						 LCMAP := "Snow/ice"]
#coefs_labels[landUse == 5,
#						 LCMAP := "Disturbed/transitional"]

coefs_labels[, LCMAP := as.factor(LCMAP)]

## testing training split



library(cluster)
#clusters_pam5 = get(load("../../data/rf/clusters_20171201/clusters_pam5"))
#clusters_pam = get(load("../../data/rf/clusters_20171201/clusters_pam"))
#clusters_pam11 = get(load("../../data/rf/clusters_20171201/clusters_pam11"))
#clusters_pam12 = get(load("../../data/rf/clusters_20171201/clusters_pam12"))
#clusters_pam13 = get(load("../../data/rf/clusters_20171201/clusters_pam13"))
#clusters_pam14 = get(load("../../data/rf/clusters_20171201/clusters_pam14"))
#clusters_pam15 = get(load("../../data/rf/clusters_20171201/clusters_pam15"))
#clusters_pam20 = get(load("../../data/rf/clusters_20171201/clusters_pam20"))
#A
#clusters_clara28 = get(load("../../data/rf/clusters/tc_20180219_k30_d4"))
clusters_clara20sub = get(load("../../data/rf/clusters/tc_20180219_k20_d4_sub"))
featureNames = names(coefs_labels)
featureNames = featureNames[!grepl("pam35|LCMAP|rowi|surfaceType|vegForm|phenotype|under|density|wetlandFlag|landUse|year|confidence|skipped",featureNames)]

#conf_pam20 = get(load("../../data/rf/clusters_20171201/conf_pam20"))
#conf_pam5 = get(load("../../data/rf/clusters_20171201/conf_pam5"))
#conf_pam11 = get(load("../../data/rf/clusters_20171201/conf_pam11"))
#conf_pam12 = get(load("../../data/rf/clusters_20171201/conf_pam12"))
#conf_pam13 = get(load("../../data/rf/clusters_20171201/conf_pam13"))
#conf_pam14 = get(load("../../data/rf/clusters_20171201/conf_pam14"))
#conf_pam15 = get(load("../../data/rf/clusters_20171201/conf_pam15"))
#conf_pam10 = get(load("../../data/rf/clusters_20171201/conf_pam10"))


generateProfiles = function(clusters){

	# examine medoid spectral signatures
	#medoids = clusters$medoids
	medoids = clusters$final.clust$i.med
	k = length(unique(clusters$final.clust$clustering))
	print(paste0("profiling: ", k))

	medoiddt_l = lapply(as.numeric(medoids), function(x)return(coefs_labels[x,]))
	medoiddt = rbindlist(medoiddt_l)
	medoiddt[,med:=medoids]
	medoiddt[,clust:=1:length(unique(clusters$final.clust$clustering))]

	medoidmelt = melt(medoiddt,
										id.vars=c('LCMAP','clust'),
										measure.vars = c(featureNames[!grepl("ecozone",featureNames)]))

	# get some feature info and normalize them
	medoidmelt[, band := tstrsplit(variable, "_")[2]]
	medoidmelt[band=="rmse", band := tstrsplit(variable, "_")[3]]
	medoidmelt[, metr := tstrsplit(variable, "_")[1]]
	medoidmelt[metr=="robust",metr:="rmse"]
	medoidmelt = medoidmelt[band %in% c("blue","green","red","nir","swir1","swir2","bt","ndvi","evi","tcb","tcg","tcw","tcwgd", "nbr", "nbrevi", "bcc","gcc","rcc") | metr %in% c("snowiness", "cloudiness", "seglength", "nbreaks","robust", "elv", "asp", "slp", "swocc", "swrec", "swsea", "swext")]
	medoidmelt[is.na(band),band:=metr]
	setkey(medoidmelt, band)
	setkey(banddt, band)

	#medoidmelt[,lambda := assignLambda(band), by=1:nrow(medoidmelt)]
	medoidmelt = medoidmelt[banddt]

	medoidmelt[,value:=as.numeric(value)]
#
#	medoidmelt[metr == "nbreaks", value:= (value+1)/10]
#	medoidmelt[metr == "rmse", value := value/1000]
#	medoidmelt[metr == "seglength", value:= value/10000]
#	medoidmelt[metr == "trnd", value:= value*10000]
#	medoidmelt[metr == "asp", value:= value/360]
#	medoidmelt[metr == "elv", value:= value/10000]
#	medoidmelt[metr == "slp", value:= value*100]
#	medoidmelt[metr == "swocc", value:= value/100]
#	medoidmelt[metr == "swsea", value:= value/12]
#	medoidmelt[metr == "swrec", value:= value/100]
#	medoidmelt[metr == "swext", value:= value/10]

	labdat = unique(medoidmelt[,.(clust,LCMAP)])
	labdat[, c("x", "y") := .(0.7, 0.8)]

  kstart = seq(1,k,by=5)	

	for(i in kstart){
  	kend   = i + 4
		if(kend > k)kend=k	
		ggsave(plot=clustPlotter(i:kend, medoidmelt), paste0("../../plots/clusters_20180219/clusterProfiles_c",k,"_",i,"_",kend,"_k20sub.png"), 
					 width=8, height=6, units="in")
	}
}

#generateProfiles(clusters_pam)
#generateProfiles(clusters_pam11)
#generateProfiles(clusters_pam12)
#generateProfiles(clusters_pam13)
#generateProfiles(clusters_pam14)
#generateProfiles(clusters_pam15)
#generateProfiles(clusters_pam20) # generating weird error about being unable to cast empty dt? no mid/max or something?
#generateProfiles(clusters_pam5)
#generateProfiles(clusters_clara28)

generateHistograms = function(clusters, clustrange){

	# combine data
	coefs_clusters = coefs_labels[, clust := clusters$final.clust$clustering]
	clusterdt = coefs_clusters[, .(surfaceType, vegForm, phenotype, under, density, wetlandFlag, landUse,clust)] 
	clusterdt = clusterdt[surfaceType != 0,]
#	clusterdt[density %in% c(0,3), dclass := "closed"]
#	clusterdt[density %in% c(1), dclass := "sparse"]
#	clusterdt[density %in% c(2), dclass := "open"]
	clustermelt = melt(clusterdt,
										 id.vars = c("clust", "density"))
										 #measure.vars = "dclass")

	clustermelt = clustermelt[clust %in% clustrange,]
	clustermelt = clustermelt[,count:=.N, by=c("variable","density","clust", "value")]
	clustermelt = rbind(clustermelt, cbind(expand.grid(variable=unique(clustermelt$variable),density=unique(clustermelt$density),clust=unique(clustermelt$clust),value=unique(clustermelt$value)), count=0))
	clustermelt = unique(clustermelt)

	# plot
	c_histos = ggplot(data = clustermelt, aes(x = as.factor(value), y = count, fill = as.factor(density))) + 
		geom_bar(position="dodge", stat='identity') +
		facet_grid(variable ~ clust) + 
		theme_bw() + 
		theme(panel.grid.minor = element_blank())
	k = clustrange[1] 
		#length(unique(clusters$clustering))

	ggsave(paste0("../../plots/clusters_20180219/clara20sub_",k,"_histos.png"), c_histos, height = 6, width = 8, units="in")
}

generateHistograms(clusters_clara20sub,1:5)
generateHistograms(clusters_clara20sub,6:10)
generateHistograms(clusters_clara20sub,11:15)
generateHistograms(clusters_clara20sub,16:20)


## plot confusion matrix
conf = get(load("../../data/rf/clusters/tc_20180219_k20_conf"))

table20 = conf$table

referenceTotals = colSums(table20)

table20prop = lapply(1:ncol(table20), function(x)table20[,x]/referenceTotals[x])
table20prop = do.call(cbind,table20prop)
colnames(table20prop) = rownames(table20prop)
round(table20prop, 2)*100

confdt = data.table(TClass = rep(1:20,20),
										PClass = unlist(lapply(20:1,function(x)rep(x,20))),
										Y = unlist(lapply(20:1,function(x)table20[,x])),
										Yp = unlist(lapply(20:1,function(x)table20prop[,x])))

confplot = ggplot(data =  confdt, mapping = aes(x = TClass, y = PClass)) +
	geom_tile(aes(fill = Yp), colour = "white") +
	geom_text(aes(label = sprintf("%1.0f", Y)), size = 1.5) +
	scale_fill_gradient("Proportion",low = "#3333CC", high = "#CC3333", breaks=seq(from=0,to=1,by=0.1)) +
	theme_bw() +
	ylab("Predicted Class") + xlab("Reference Class") +
	scale_x_continuous(breaks = 1:20) +
	scale_y_continuous(trans="reverse", breaks = 1:20) +
	theme(axis.text.x = element_text(angle = -35, vjust = 1, hjust = 0),
				axis.text = element_text(size = 6)) +
ggtitle("Confusion Matrix of Classifier")
ggsave("../../plots/clusters_20180219/clara20_confusion.png",confplot, height=4, width=5, units='in')

mapworld = borders("world", color="gray50", fill="gray50")
mp = ggplot() + mapworld + geom_point(data=coefs_labels, aes(x = lon, y = lat), color='blue') + xlim(range(coefs_labels$lon, na.rm=T)) + ylim(range(coefs_labels$lat,na.rm=T))


## think about importance


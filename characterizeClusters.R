#################
#
#		take a set of clusterings and data and determine some spectral profiles of each cluster	
#
#
#require(caret)
library(cluster)
#require(DMwR)
library(randomForest)
#library(unbalanced)
library(data.table)
library(foreach)
library(doParallel)
library(ggplot2)
#library(ranger)

rfid = commandArgs(TRUE)[1]

## also do mean profiles?
## time, cluster, band mabybe best as a shiny app

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
	if(x=="GSL")return(8.75)
	if(x=="PZI")return(8.5)
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
															 7.25, 7.5, 7.75, 8, 8.25, 8.5, 8.75, 9,
                               9.25, 9.5),
										band = c("blue", "green", "red", "nir", "swir1", "swir2", "bt",
														 "ndvi", "evi", "nbrevi", "nbr", "tcg", "tcb", "tcw",
														 "tcwgd", "bcc", "gcc", "rcc", "elv", "asp", "slp",
														 "nbreaks", "seglength", "snowiness","cloudiness", "snowmelt", "snowfall", "swocc", "swrec", "swsea", "swext",
                             "PZI", "GSL")) 
														 


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

therf = get(load(paste0("../../data/rf/model/",rfid)))
#load("../../data/rf/featureNames_20180411")

if(rfid == "tc_20180319_k50_pam_rf"){

load("../../data/rf/featureNames_20180319")
coefs_labels=get(load('../../data/rf/clusters/tc_20180319_k50_dt'))
clusters_pam = get(load("../../data/rf/clusters/tc_20180319_k50_d4"))
}

if(rfid == "tc_20180411_k50_pam_rf"){

load("../../data/rf/featureNames_20180411")
coefs_labels=get(load('../../data/rf/clusters/tc_20180411_k50_dt'))
clusters_pam = get(load("../../data/rf/clusters/tc_20180411_k50_d4_big"))
}

if(rfid == "tc_20180415_noBC_k55_pam_rf"){

load("../../data/rf/featureNames_20180415")
coefs_labels=get(load('../../data/rf/clusters/tc_20180416_noBC_k55_dt'))
clusters_pam = get(load("../../data/rf/clusters/tc_20180415_noBC_k55_d4_big"))
}

if(rfid == "tc_20180416_noGeo_k55_pam_rf"){

load("../../data/rf/featureNames_20180415_nogeo")
coefs_labels=get(load('../../data/rf/clusters/tc_20180416_noGeo_k55_dt'))
clusters_pam = get(load("../../data/rf/clusters/tc_20180416_noGeo_k55_d4_big"))
}

if(rfid == "tc_20180416_noGeoNoBC_k55_pam_rf"){

load("../../data/rf/featureNames_20180415_noGeoNoBC")
clusters_pam = get(load("../../data/rf/clusters/tc_20180416_noGeoNoBC_k55_d4_big"))
coefs_labels=get(load('../../data/rf/clusters/tc_20180416_noGeoNoBC_k55_dt'))
}
if(rfid == "tc_20180416_ecozoneNoBC_k50_pam_rf"){

load("../../data/rf/featureNames_20180416_ecozoneNoBC")
clusters_pam = get(load("../../data/rf/clusters/tc_20180416_ecozoneNoBC_k50_d4_big"))
coefs_labels=get(load('../../data/rf/clusters/tc_20180416_ecozoneNoBC_k50_dt'))
}
## water
#coefs_labels[(surfaceType == 1) | 
#						 (density == 1 & under == 4),# | 
##						 (wetlandFlag == 3),
#   	  			 LCMAP := "Water"]
##coefs_labels[(surfaceType == 2 & landUse == 1),
##						 LCMAP := "Developed"]
#coefs_labels[(landUse == 2 | landUse == 3), 
#						 LCMAP := "Cropland"]
#coefs_labels[(surfaceType == 2 & landUse != 1) |
#						 (surfaceType == 3 & density %in% c(1) & under == 1),
#					   LCMAP := "Barren"]
#coefs_labels[(surfaceType == 3 & vegForm == 4 & phenotype %in% c(1) & density %in% c(2,3)) & wetlandFlag == 1,
#						 LCMAP := "Decid Forest"]
#coefs_labels[(surfaceType == 3 & vegForm == 4 & phenotype %in% c(3) & density %in% c(2,3)) & wetlandFlag == 1,
#						 LCMAP := "Mixed Forest"]
#coefs_labels[(surfaceType == 3 & vegForm == 4 & phenotype == 2 & density %in% c(3)) & wetlandFlag == 1,
#						 LCMAP := "Everg Forest"]
#coefs_labels[(surfaceType == 3 & vegForm == 4 & phenotype == 2 & density %in% c(2,1)) & wetlandFlag == 1 & under == 1,
#						 LCMAP := "Peat Woodland"]
#coefs_labels[(surfaceType == 3 & vegForm %in% c(2,3) & landUse == 5),
#						 LCMAP := "Post-fire"]
#coefs_labels[(surfaceType == 3 & vegForm == 4 & landUse == 5),
#						 LCMAP := "Secondary Forest"]
## coefs_labels[(surfaceType == 4 & vegForm == 3 & phenotype == 2 & density == 1 & wetlandFlag == 1),
## 							LCMAP := "Sparse Dry Forest"]
#
#coefs_labels[(surfaceType == 3 & vegForm %in% c(3) & under %in% c(2,3) & density %in% c(2,1) & wetlandFlag == 1),
#						 LCMAP := "Open Shrubs"]
#coefs_labels[(surfaceType == 3 & vegForm %in% c(1,2) & density %in% c(1,2) & under == 1),
#						 LCMAP := "Sparsely Vegetated"]
#coefs_labels[(surfaceType == 3 & vegForm %in% c(2,3) & landUse == 5),
#						 LCMAP := "Post-fire"]
## coefs_labels[(surfaceType == 4 & vegForm == 3 & phenotype == 2 & density == 1 & wetlandFlag == 2),
## 							LCMAP := "Sparse Wetland Forest"]
#
#coefs_labels[(surfaceType == 3 & vegForm %in% c(2) & density %in% c(2,3) & wetlandFlag == 1), 
#						 LCMAP := "Grass"]
#coefs_labels[(surfaceType == 3 & vegForm %in% c(3) & density %in% c(2,3) & wetlandFlag == 1),
#						 LCMAP := "Shrub"]
#coefs_labels[(wetlandFlag == 3),
#						 LCMAP := "Shallow Lake"]
#coefs_labels[(surfaceType == 3 & vegForm %in% c(1) & density %in% c(1,2,3) & wetlandFlag == 2),
#						 LCMAP := "Bog"]
#coefs_labels[(surfaceType == 3 & vegForm %in% c(2) & density %in% c(1,2,3) & wetlandFlag == 2),
#						 LCMAP := "Fen"]
#coefs_labels[(surfaceType == 3 & vegForm %in% c(3,4) & density %in% c(1,2,3) & wetlandFlag == 2),
#						 LCMAP := "Woody Wetland"]
##coefs_labels[(surfaceType == 3 & vegForm %in% c(1,2,3,4) & density %in% c(1,2,3) & wetlandFlag == 2),
##						 LCMAP := "Wetland"]
##coefs_labels[(surfaceType == 4 | snowiness > 0.7),
##						 LCMAP := "Snow/ice"]
##coefs_labels[landUse == 5,
##						 LCMAP := "Disturbed/transitional"]
#
#coefs_labels[, LCMAP := as.factor(LCMAP)]
#
#
#generateProfiles = function(clusters){
#
#	# examine medoid spectral signatures
#	#medoids = clusters$medoids
#	medoids = clusters$final.clust$id.med
#	k = length(unique(clusters$final.clust$clustering))
#	print(paste0("profiling: ", k))
#
#	medoiddt_l = lapply(as.numeric(medoids), function(x)return(coefs_labels[x,]))
#	medoiddt = rbindlist(medoiddt_l)
#	medoiddt[,med:=medoids]
#	medoiddt[,clust:=1:length(unique(clusters$final.clust$clustering))]
#
#	medoidmelt = melt(medoiddt,
#										id.vars=c('LCMAP','clust'),
#										measure.vars = c(featureNames[!grepl("ecozone",featureNames)]))
#
#	# get some feature info and normalize them
#	medoidmelt[, band := tstrsplit(variable, "_")[2]]
#	medoidmelt[band=="rmse", band := tstrsplit(variable, "_")[3]]
#	medoidmelt[, metr := tstrsplit(variable, "_")[1]]
#	medoidmelt[metr=="robust",metr:="rmse"]
#	medoidmelt = medoidmelt[band %in% c("blue","green","red","nir","swir1","swir2","bt","ndvi","evi","tcb","tcg","tcw","tcwgd", "nbr", "nbrevi", "bcc","gcc","rcc") | metr %in% c("snowiness", "cloudiness", "seglength", "nbreaks","robust", "elv", "asp", "slp", "swocc", "swrec", "swsea", "swext", "PZI", "GSL")]
#	medoidmelt[is.na(band),band:=metr]
#	setkey(medoidmelt, band)
#	setkey(banddt, band)
#
#	#medoidmelt[,lambda := assignLambda(band), by=1:nrow(medoidmelt)]
#	medoidmelt = medoidmelt[banddt]
#
#	medoidmelt[,value:=as.numeric(value)]
##
##	medoidmelt[metr == "nbreaks", value:= (value+1)/10]
##	medoidmelt[metr == "rmse", value := value/1000]
##	medoidmelt[metr == "seglength", value:= value/10000]
##	medoidmelt[metr == "trnd", value:= value*10000]
##	medoidmelt[metr == "asp", value:= value/360]
##	medoidmelt[metr == "elv", value:= value/10000]
##	medoidmelt[metr == "slp", value:= value*100]
##	medoidmelt[metr == "swocc", value:= value/100]
##	medoidmelt[metr == "swsea", value:= value/12]
##	medoidmelt[metr == "swrec", value:= value/100]
##	medoidmelt[metr == "swext", value:= value/10]
#
#	labdat = unique(medoidmelt[,.(clust,LCMAP)])
#	labdat[, c("x", "y") := .(0.7, 0.8)]
#
#  kstart = seq(1,k,by=5)	
#
#	for(i in kstart){
#  	kend   = i + 4
#		if(kend > k)kend=k	
#		ggsave(plot=clustPlotter(i:kend, medoidmelt), paste0("../../plots/clusters_",rfid,"/clusterProfiles_c",k,"_",i,"_",kend,".png"), 
#					 width=8, height=6, units="in")
#	}
#}
#
#generateProfiles(clusters_pam)
dir.create(paste0("../../plots/clusters_",rfid),showWarnings=F,recursive=T)

generateHistograms = function(clusters, clustrange){

	# combine data
	#coefs_clusters = coefs_labels[, clust := clusters$final.clust$clustering]
	clusterdt = coefs_labels[, .(surfaceType, vegForm, phenotype, under, density, wetlandFlag, landUse,tcCluster)] 
#	clusterdt = clusterdt[surfaceType != 0,]
#	clusterdt[density %in% c(0,3), dclass := "closed"]
#	clusterdt[density %in% c(1), dclass := "sparse"]
#	clusterdt[density %in% c(2), dclass := "open"]
	clustermelt = melt(clusterdt,
										 id.vars = c("tcCluster", "density"))
										 #measure.vars = "dclass")

	clustermelt = clustermelt[tcCluster %in% clustrange,]
	clustermelt = clustermelt[,count:=.N, by=c("variable","density","tcCluster", "value")]
	clustermelt = rbind(clustermelt, 
                     cbind(expand.grid(variable=unique(clustermelt$variable),
                                        density=unique(clustermelt$density),
                                        tcCluster=unique(clustermelt$tcCluster),
                                        value=unique(clustermelt$value)), 
                            count=0)
                     )
	clustermelt = unique(clustermelt)

	# plot
	c_histos_naless = ggplot(data = na.omit(clustermelt), aes(x = as.factor(value), y = count, fill = as.factor(density))) + 
		geom_bar(position="dodge", stat='identity') +
		facet_grid(variable ~ tcCluster) + 
		theme_bw() + 
		theme(panel.grid.minor = element_blank())
	c_histos = ggplot(data = clustermelt, aes(x = as.factor(value), y = count, fill = as.factor(density))) + 
		geom_bar(position="dodge", stat='identity') +
		facet_grid(variable ~ tcCluster) + 
		theme_bw() + 
		theme(panel.grid.minor = element_blank())
	k = clustrange[1] 
		#length(unique(clusters$clustering))

	ggsave(paste0("../../plots/clusters_",rfid,"/k",k,"_histos_nas.png"), c_histos, height = 6, width = 8, units="in")
	ggsave(paste0("../../plots/clusters_",rfid,"/k",k,"_histos.png"), c_histos_naless, height = 6, width = 8, units="in")
}

generateHistograms(clusters_pam,1:5)
generateHistograms(clusters_pam,6:10)
generateHistograms(clusters_pam,11:15)
generateHistograms(clusters_pam,16:20)
generateHistograms(clusters_pam,21:25)
generateHistograms(clusters_pam,26:30)
generateHistograms(clusters_pam,31:35)
generateHistograms(clusters_pam,36:40)
generateHistograms(clusters_pam,41:45)
generateHistograms(clusters_pam,46:50)
generateHistograms(clusters_pam,51:55)


### plot confusion matrix
#conf = get(load("../../data/rf/clusters/tc_20180411_k50_conf"))
#
#table50 = conf$table
#
#referenceTotals = colSums(table20)
#
#table50prop = lapply(1:ncol(table50), function(x)table50[,x]/referenceTotals[x])
#table50prop = do.call(cbind,table50prop)
#colnames(table50prop) = rownames(table50prop)
#round(table50prop, 2)*100
#
#confdt = data.table(TClass = rep(1:50,50),
#										PClass = unlist(lapply(50:1,function(x)rep(x,50))),
#										Y = unlist(lapply(50:1,function(x)table50[,x])),
#										Yp = unlist(lapply(50:1,function(x)table50prop[,x])))
#
#confplot = ggplot(data =  confdt, mapping = aes(x = TClass, y = PClass)) +
#	geom_tile(aes(fill = Yp), colour = "white") +
#	geom_text(aes(label = sprintf("%1.0f", Y)), size = 1.5) +
#	scale_fill_gradient("Proportion",low = "#3333CC", high = "#CC3333", breaks=seq(from=0,to=1,by=0.1)) +
#	theme_bw() +
#	ylab("Predicted Class") + xlab("Reference Class") +
#	scale_x_continuous(breaks = 1:50) +
#	scale_y_continuous(trans="reverse", breaks = 1:50) +
#	theme(axis.text.x = element_text(angle = -35, vjust = 1, hjust = 0),
#				axis.text = element_text(size = 6)) +
#ggtitle("Confusion Matrix of Classifier")
#ggsave("../../plots/clusters_20180219/clara20_confusion.png",confplot, height=4, width=5, units='in')
#
#mapworld = borders("world", color="gray50", fill="gray50")
#mp = ggplot() + mapworld + geom_point(data=coefs_labels, aes(x = lon, y = lat), color='blue') + xlim(range(coefs_labels$lon, na.rm=T)) + ylim(range(coefs_labels$lat,na.rm=T))
#
#
### think about importance
#

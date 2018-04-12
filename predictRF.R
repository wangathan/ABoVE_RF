#########
#
#		Take a tile
#			For each feature feather, run the randomforest classifier on it
#			Save results
#
#

## packages
library(randomForest)
#library(ranger)
library(feather)
library(data.table)
library(raster)
library(parallel)

## inputs
ti = commandArgs(TRUE)[1]
rfid = commandArgs(TRUE)[2]
#thepoch = as.numeric(commandArgs(TRUE)[3])

print(paste0("predicting for tile: ", ti))
#print(paste0("predicting for epoch: ", thepoch))
print(paste0("using: ", rfid))

# now we include the DEM data
elev = raster(paste0('../../data/aster/',ti,'/AST_DEM_',ti,'_elv.tif'))
asp = raster(paste0('../../data/aster/',ti,'/AST_DEM_',ti,'_asp.tif'))
slp = raster(paste0('../../data/aster/',ti,'/AST_DEM_',ti,'_slp.tif'))

# and Belward's surface water data
sw_occ = raster(paste0('../../data/Surface_water/occurrence_tiles/occurrence.',ti,'.tif'))
sw_ext = raster(paste0('../../data/Surface_water/extent_tiles/extent.',ti,'.tif'))
sw_sea = raster(paste0('../../data/Surface_water/seasonality_tiles/seasonality.',ti,'.tif'))
sw_rec = raster(paste0('../../data/Surface_water/recurrence_tiles/recurrence.',ti,'.tif'))

# and Gruber's PZI data
pzi = raster(paste0("../../data/PZI/tiles/",ti,"_PZI.tif"))

# and bioclim data
amt = raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_annMeanTemp.tif"))
mdr = raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_meanDiurnalRange.tif"))
iso = raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_isothermality.tif"))
TmpSea=raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_tempSeasonality.tif"))
WrmMax= raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_warmestMax.tif"))
CldMin= raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_coldestMin.tif"))
AnnRng= raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_annualRange.tif"))
WetTmp= raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_wettestTemp.tif"))
DryTmp= raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_driestTemp.tif"))
WrmTmp= raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_warmestTemp.tif"))
CldTmp = raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_coldestTemp.tif"))
AnnPcp = raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_annPrecip.tif"))
PcpSea = raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_precipSeasonality.tif"))
WetPcp = raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_wettestPrecip.tif"))
DryPcp = raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_driestPrecip.tif"))
WrmPcp= raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_warmestPrecip.tif"))
CldPcp = raster(paste0("../../data/bioclim/tiles/",ti,"/",ti,"_coldestPrecip.tif"))

## get files
#if(thepoch == 2010){
	ffiles = list.files(paste0("../../data/features/",ti),
										full.names=T)
#}else{
#	ffiles = list.files(paste0("../../data/features_",thepoch,"/",ti),
#											full.names=T)
#}
therf = get(load(paste0("../../data/rf/model/",rfid)))
load("../../data/rf/featureNames_20180319")
## also grab the training dataset
LCMAPdt_train=get(load('../../data/rf/clusters/tc_20180319_k50_dt'))
#LCMAPdt_train=get(load('../../data/rf/clusters/tc_20180219_k25_dt_mn_sub'))
#load('../../data/rf/model/lcmapdt_train')

print(length(ffiles))
#if(thepoch == 2010){
	dir.create(paste0("../../data/rf/predict_",rfid,"/",ti), showWarnings=F, recursive=T)
#}else{
#	dir.create(paste0("../../data/rf/predict_",rfid,"_",thepoch,"/",ti), showWarnings=F, recursive=T)
#}
impute = function(indt){
	for(i in names(indt)){
		if(class(unlist(indt[,..i])) != 'numeric')next
		themed = median(unlist(indt[,..i][is.finite(unlist(indt[,..i]))]),na.rm=T)
		indt[is.na(get(i)),(i):=themed]
		indt[get(i) == -Inf | get(i) == Inf,(i):=themed]
	}
	return(indt)
}

assignOneHotEcozone = function(coefs_labels){
	# one-hot encoding for categorical variables?                                         
	# only ecozone                                                                        
	coefs_labels[, c("eco_BorealShield","eco_BorealPlain","eco_BeringTaiga","eco_BeringTundra",
									"eco_IntermBor", "eco_AKRangeT", "eco_AleutMead", "eco_CoastalRF",
								 "eco_ArcTundra", "eco_PacMtnTra", "eco_CoastMtnTra", "eco_BorCordill",
								 "eco_PacMarit", "eco_TaigaCord", "eco_MontCord", "eco_TaigaPlain", "eco_SouthArctic",
								 "eco_NorthArctic", "eco_Prairie", "eco_TaigaShield") := as.factor(1)]            
	
	coefs_labels[ecozone != "Boreal Shield", eco_BorealShield := as.factor(0)]            

	coefs_labels[ecozone != "Boreal PLain", eco_BorealPlain := as.factor(0)]              

	coefs_labels[ecozone != "Bering Taiga", eco_BeringTaiga := as.factor(0)]              

	coefs_labels[ecozone != "Bering Tundra", eco_BeringTundra := as.factor(0)]            

	coefs_labels[ecozone != "Intermontane Boreal", eco_IntermBor := as.factor(0)]         

	coefs_labels[ecozone != "Alaska Range Transition", eco_AKRangeT := as.factor(0)]      

	coefs_labels[ecozone != "Aleutian Meadows", eco_AleutMead := as.factor(0)]            

	coefs_labels[ecozone != "Coastal Rainforests", eco_CoastalRF := as.factor(0)]         

	coefs_labels[ecozone != "Arctic Tundra", eco_ArcTundra := as.factor(0)]               

	coefs_labels[ecozone != "Pacific Mountains Transition", eco_PacMtnTra := as.factor(0)]

	coefs_labels[ecozone != "Coast Mountains Transition", eco_CoastMtnTra := as.factor(0)]

	coefs_labels[ecozone != "Boreal Cordillera", eco_BorCordill := as.factor(0)]          

	coefs_labels[ecozone != "Pacific Maritime", eco_PacMarit := as.factor(0)]             

	coefs_labels[ecozone != "Taiga Cordillera", eco_TaigaCord := as.factor(0)]            

	coefs_labels[ecozone != "Montane Cordillera", eco_MontCord := as.factor(0)]           

	coefs_labels[ecozone != "Taiga Plain", eco_TaigaPlain := as.factor(0)]                

	coefs_labels[ecozone != "Southern Arctic", eco_SouthArctic := as.factor(0)]           

	coefs_labels[ecozone != "Northern Arctic", eco_NorthArctic := as.factor(0)]

	coefs_labels[ecozone != "Prairie", eco_Prairie := as.factor(0)]

	coefs_labels[ecozone != "Taiga Shield", eco_TaigaShield := as.factor(0)]

	coefs_labels[,ecozone := NULL]

	return(coefs_labels)
}

## predict function
predictLCMAP = function(i, ti){
	# read and clean up

	if(i %% 100 == 0)print(i)


	rowi = as.numeric(strsplit(ffiles[i],"_c|_f")[[1]][2])
	## save outputs
#	if(thepoch==2010){
		fout = paste0("../../data/rf/predict_",rfid,"/",ti,"/rfpredict_",rowi,"_",rfid)
#	}else{
#		fout = paste0("../../data/rf/predict_",rfid,"_",thepoch,"/",ti,"/rfpredict_",rowi,"_",rfid)
#	}
	if(file.exists(fout))return(NULL)
	fdt = as.data.table(read_feather(ffiles[i]))
	fdt[,c("i.px", "i.py"):=NULL]
	#print(ffiles[i])

	fdt = assignOneHotEcozone(fdt)

	# set levels
	# randomforest throws a type mismatch error if the levels of factors don't match
	# between the training dataset and the new dataset
	# this is set up to rectify that. too bad ecozone doesn't matter much.
	ecos = names(fdt)[grep("eco_",names(fdt))]
	for(ezone in ecos){
		setattr(fdt[[ezone]],"levels",c("1","0"))
		#levels(fdt[[ezone]])=c("1","0")
	}

#	# for the missing ecozones, pick the most common ecozones in row? 
#	ecotable = table(fdt$ecozone)
#	fillzone = names(ecotable)[which.max(ecotable)]
#	if(is.null(fillzone)){
#		fillzone = "Taiga Plain" # the most common ecozone from LCMAPdt_train
#		#print(paste0("NULL ERROR FOR ECOZONE: ", i))
#	#print(is.null(names(ecotable)[which.max(ecotable)]))
#	}
#		fdt[is.na(ecozone), ecozone:=fillzone]
#
#	fdt[,ecozone := as.factor(ecozone)]
#	levels(fdt$ecozone) = levels(LCMAPdt_train$ecozone)
#
	
	## calculate mean values

	thebands = c("blue", "green", "red", "nir", "swir1", "swir2", "bt",
							 "bcc", "gcc", "rcc",
							 "ndvi", "evi", "nbr", "ndsi",
							 "tcg", "tcw", "tcb",
							 "tcwgd", "nbrevi")

	themons = c("m1", "m2", "m3", "m3", "m5", "m6", "m7")

	for(b in thebands){
		mnvals = rowMeans(fdt[,paste0(themons,"_",b),with=F])
		fdt[,paste0("mn_",b):=mnvals]
	}


	#fdt[,names(fdt) := lapply(.SD,function(x)if(x  == Inf | x == -Inf){return(NA)}else{return(x)})] 
	fdt[,missingrows:=Reduce(`+`,lapply(.SD,is.na))]

	if(max(fdt$missingrows > 0)){
		fdt=impute(fdt)
	}
	fdt[,missingrows:=NULL]
	
	# also having problems with snowmelt/snowfall values
	# do it again in case a whole file is missing these values
#	fdt[is.na(snowiness) | is.na(cloudiness) | is.na(snowmelt) | is.na(snowfall), 
#			c("snowiness", "cloudiness", "snowmelt", "snowfall") :=
	#			.(0.3, 0.6, 120, 300)]

	fdt[is.na(snowiness), snowiness:=median(fdt$snowiness,na.rm=T)]
	fdt[is.na(cloudiness), cloudiness:=median(fdt$cloudiness,na.rm=T)]
	fdt[is.na(snowmelt), snowmelt:=median(fdt$snowmelt,na.rm=T)]
	fdt[is.na(snowfall), snowfall:=median(fdt$snowfall,na.rm=T)]

	# attach DEM data - use rowi to determine the ones to get
	asp_i = getValues(asp, row = rowi*2+1, nrows=2)
	elv_i = getValues(elev, row =rowi*2+1, nrows=2)
	slp_i = getValues(slp, row = rowi*2+1, nrows=2)
	sw_occ_i = getValues(sw_occ, row=rowi*2+1, nrows=2)
	sw_rec_i = getValues(sw_rec, row=rowi*2+1, nrows=2)
	sw_sea_i = getValues(sw_sea, row=rowi*2+1, nrows=2)
	sw_ext_i = getValues(sw_ext, row=rowi*2+1, nrows=2)
	pzi_i = getValues(pzi, row=rowi*2+1, nrows=2)
  amt_i = getValues(amt, row=rowi*2+1, nrows=2)
  mdr_i = getValues(mdr, row=rowi*2+1, nrows=2)
  iso_i = getValues(iso, row=rowi*2+1, nrows=2)
  TmpSea_i = getValues(TmpSea, row=rowi*2+1, nrows=2)
  WrmMax_i = getValues(WrmMax, row=rowi*2+1, nrows=2)
  CldMin_i = getValues(CldMin, row=rowi*2+1, nrows=2)
  AnnRng_i = getValues(AnnRng, row=rowi*2+1, nrows=2)
  WetTmp_i = getValues(WetTmp, row=rowi*2+1, nrows=2)
  DryTmp_i = getValues(DryTmp, row=rowi*2+1, nrows=2)
  WrmTmp_i = getValues(WrmTmp, row=rowi*2+1, nrows=2)
  CldTmp_i = getValues(CldTmp, row=rowi*2+1, nrows=2)
  AnnPcp_i = getValues(AnnPcp, row=rowi*2+1, nrows=2)
  PcpSea_i = getValues(PcpSea, row=rowi*2+1, nrows=2)
  WetPcp_i = getValues(WetPcp, row=rowi*2+1, nrows=2)
  DryPcp_i = getValues(DryPcp, row=rowi*2+1, nrows=2)
  WrmPcp_i = getValues(WrmPcp, row=rowi*2+1, nrows=2)
  CldPcp_i = getValues(CldPcp, row=rowi*2+1, nrows=2)
	dempx = c(0:5999, 0:5999) # it's all zero-indexed in yatsm
	dempy = c(rep(rowi*2,6000), rep(rowi*2 + 1,6000))
  demdt = data.table(px = dempx, 
                     py = dempy, 
                     asp = asp_i, 
                     elv = elv_i, 
                     slp = slp_i,
                     PZI = pzi_i,
                     annMeanTemp = amt_i,
                     mnDiurnalRange = mdr_i,
                     isothermality = iso_i,
                     tempSeasonality = TmpSea_i, 
                     warmMax = WrmMax_i, 
                     coldMin = CldMin_i,
                     annRange = AnnRng_i, 
                     wetTemp = WetTmp_i, 
                     dryTemp = DryTmp_i, 
                     wrmTemp = WrmTmp_i, 
                     cldTemp = CldTmp_i, 
                     annPcp = AnnPcp_i, 
                     pcpSeasonality = PcpSea_i, 
                     wetPcp = WetPcp_i, 
                     dryPcp = DryPcp_i, 
                     wrmPcp = WrmPcp_i, 
                     cldPcp =CldPcp_i, 
                     swocc = sw_occ_i,
                     swrec = sw_rec_i,
                     swsea = sw_sea_i,
										 swext = sw_ext_i)
	demdt[is.na(asp), asp:=as.integer(median(demdt$asp, na.rm=T))]
	demdt[is.na(elv), elv:=as.integer(median(demdt$elv, na.rm=T))]
	demdt[is.na(slp), slp:=as.integer(median(demdt$slp, na.rm=T))]
	demdt[is.na(PZI), PZI:=as.integer(median(demdt$PZI, na.rm=T))]
	demdt[is.na(annMeanTemp), annMeanTemp:=as.integer(median(demdt$annMeanTemp, na.rm=T))]
	demdt[is.na(mnDiurnalRange), mnDiurnalRange:=as.integer(median(demdt$mnDiurnalRange, na.rm=T))]
	demdt[is.na(tempSeasonality), tempSeasonality:=as.integer(median(demdt$tempSeasonality, na.rm=T))]
	demdt[is.na(warmMax), warmMax:=as.integer(median(demdt$warmMax, na.rm=T))]
	demdt[is.na(coldMin), coldMin:=as.integer(median(demdt$coldMin, na.rm=T))]
	demdt[is.na(annRange), annRange:=as.integer(median(demdt$annRange, na.rm=T))]
	demdt[is.na(wetTemp), wetTemp:=as.integer(median(demdt$wetTemp, na.rm=T))]
	demdt[is.na(dryTemp), dryTemp:=as.integer(median(demdt$dryTemp, na.rm=T))]
	demdt[is.na(wrmTemp), wrmTemp:=as.integer(median(demdt$wrmTemp, na.rm=T))]
	demdt[is.na(cldTemp), cldTemp:=as.integer(median(demdt$cldTemp, na.rm=T))]
	demdt[is.na(annPcp), annPcp:=as.integer(median(demdt$annPcp, na.rm=T))]
	demdt[is.na(pcpSeasonality), pcpSeasonality:=as.integer(median(demdt$pcpSeasonality, na.rm=T))]
	demdt[is.na(wetPcp), wetPcp:=as.integer(median(demdt$wetPcp, na.rm=T))]
	demdt[is.na(dryPcp), dryPcp:=as.integer(median(demdt$dryPcp, na.rm=T))]
	demdt[is.na(wrmPcp), wrmPcp:=as.integer(median(demdt$wrmPcp, na.rm=T))]
	demdt[is.na(cldPcp), cldPcp:=as.integer(median(demdt$cldPcp, na.rm=T))]
	demdt[swocc==255, swocc:=0]
	demdt[swrec==255, swrec:=0]
	demdt[swext==255, swext:=0]
	demdt[swsea==255, swsea:=0]
	setkey(demdt, px, py)
	setkey(fdt, px, py)

	fdt = fdt[demdt, nomatch=0]
  fdt[, nbreaks := as.integer((.N)-1), by = c("px","py")]
  fdt[, GSL := snowfall-snowmelt]

	f_pred <- predict(therf, fdt[,featureNames,with=F])
	rm(demdt)

	fdt_out = data.table(tile = ti, 
											 px = fdt$px, 
											 py = fdt$py, 
											 yr_start = fdt$yr_start,
											 yr_end = fdt$yr_end,
											 lcmap = f_pred)
	rm(f_pred)
	rm(fdt)

	save(fdt_out, file = fout)
	gc()
}

system.time(
						test <- mclapply(1:length(ffiles), predictLCMAP, ti = ti,mc.cores=detectCores(), mc.preschedule=F)
)

predfail = which(unlist(lapply(test,class))=="try-error")
counter=1
while(length(predfail) > 0){
	if(counter > 12){
		print("loop took too long! broken!")
		break
	}
	system.time(
							test<-mclapply(predfail,predictLCMAP,ti=ti,mc.cores=detectCores(),mc.preschedule=F)
							)
	predfail = which(unlist(lapply(test,class))=="try-error")
	if(length(predfail)>0)counter=counter+1
}

print("done!")

### pain in the ass debugging
##LCMAPdt_train = LCMAPdt_train[,featureNames,with=F]
##
##classcheck = function(feat){
##	inclasses = unlist(lapply(LCMAPdt_train[,grep(feat,names(LCMAPdt_train)),with=F],class))
##	outclasses = unlist(lapply(fdt[,grep(feat,names(fdt)),with=F],class))
##	if(!all(inclasses==outclasses))
##		print(paste0("problem with feature: ", feat))
##}
##for(ind in c("ndvi", "evi", "nbr", "nbrevi", "ndsi", "tcg", "tcw", "tcb", "tcwgd", "red", "green", "blue", "nir", "swir1", "swir2", "bt", "asp", "elv", "slp", "sw"))
##	classcheck(ind)
##
##
##

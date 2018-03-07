#########
#
#		Take a tile
#			For each feature feather, run the randomforest classifier on it
#			Save results
#
#

## packages
library(randomForest)
library(feather)
library(data.table)
library(raster)
library(parallel)

## inputs
ti = commandArgs(TRUE)[1]
rfid = commandArgs(TRUE)[2]
thepoch = as.numeric(commandArgs(TRUE)[3])
waterclass = as.numeric(commandArgs(TRUE)[4])
shallowclass = as.numeric(commandArgs(TRUE)[5])

print(paste0("shading for tile: ", ti))
print(paste0("shading for epoch: ", thepoch))

# now we include the DEM data
slp = raster(paste0('../../data/aster/',ti,'/AST_DEM_',ti,'_slp.tif'))

# and Belward's surface water data
sw_occ = raster(paste0('../../../ABoVE_Xtra/Surface_water/occurrence_tiles/occurrence.',ti,'.tif'))
sw_ext = raster(paste0('../../../ABoVE_Xtra/Surface_water/extent_tiles/extent.',ti,'.tif'))
sw_sea = raster(paste0('../../../ABoVE_Xtra/Surface_water/seasonality_tiles/seasonality.',ti,'.tif'))
sw_rec = raster(paste0('../../../ABoVE_Xtra/Surface_water/recurrence_tiles/recurrence.',ti,'.tif'))

## get files
if(thepoch == 2010){
	ffiles = grep(list.files(paste0("../../data/rf/predict_",rfid,"/",ti),
										full.names=T),
								pattern="shade",
								inv=T,
								value=T)
#	ffiles = list.files(paste0("../../data/rf/predict_",rfid,"/",ti),
										#full.names=T)
}else{
	ffiles = grep(list.files(paste0("../../data/rf/predict_",rfid,"_",thepoch,"/",ti),
										full.names=T),
								pattern="shade",
								inv=T,
								value=T)
}

## check rf
therf = get(load(paste0("../../data/rf/model/",rfid)))
shadowclass = length(unique(therf$classes))+1


## predict function
fixShadows = function(i, ti, shadowclass, waterclass){
	# read and clean up

	if(i %% 100 == 0)print(i)

	fdt = get(load(ffiles[i]))
	rowi = as.numeric(strsplit(basename(ffiles[i]),"_")[[1]][2])

	fout = paste0(ffiles[i], "_shade")
	# attach sw data - use rowi to determine the ones to get
	# attach DEM data - use rowi to determine the ones to get
	slp_i = getValues(slp, row = rowi*2+1, nrows=2)
	sw_occ_i = getValues(sw_occ, row=rowi*2+1, nrows=2)
	sw_rec_i = getValues(sw_rec, row=rowi*2+1, nrows=2)
	sw_sea_i = getValues(sw_sea, row=rowi*2+1, nrows=2)
	sw_ext_i = getValues(sw_ext, row=rowi*2+1, nrows=2)
	dempx = c(0:5999, 0:5999) # it's all zero-indexed in yatsm
	dempy = c(rep(rowi*2,6000), rep(rowi*2 + 1,6000))
	demdt = data.table(px = dempx,
										 py = dempy,
										 slp = slp_i,
										 swocc = sw_occ_i,
										 swrec = sw_rec_i,
										 swsea = sw_sea_i,
										 swext = sw_ext_i)
	demdt[is.na(slp), slp:=median(demdt$slp, na.rm=T)]
	demdt[swocc==255, swocc:=0]
	demdt[swrec==255, swrec:=0]
	demdt[swext==255, swext:=0]
	demdt[swsea==255, swsea:=0]
	setkey(demdt, px, py)
	setkey(fdt, px, py)

	fdt = fdt[demdt, nomatch=0]

	fdt[,lcmap := as.character(lcmap)]
	fdt[,lcmap := as.numeric(lcmap)]
	fdt[lcmap %in% c(waterclass, shallowclass) & swocc == 0 & swext == 0 & slp > 8, lcmap := shadowclass]
	fdt[,lcmap := as.character(lcmap)]
	fdt[,lcmap := as.factor(lcmap)]

	save(fdt, file = fout)
}

mclapply(1:length(ffiles), fixShadows, ti = ti, shadowclass = shadowclass, waterclass = waterclass, mc.cores=detectCores())

print("done!")

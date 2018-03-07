library(raster)
library(data.table)
#library(feather)
library(parallel)
#library(foreach)
#library(doParallel)


ti = commandArgs(TRUE)[1]
rfid = commandArgs(TRUE)[2]

#mapyear = commandArgs(TRUE)[3]

print(ti)
print(rfid)
#source("../featureEngineering/featureFunctions.R")

# for manual runs
#rfid = "tc_20180219_k25_sub_mn_pam_rf"


for(mapyear in 1985:2014){

	rffiles = list.files(paste0("../../data/rf/predict_",rfid,"/clean_predict/",ti,"/",mapyear),
											 full.names=T)#,
											 #pattern="shade")

	print("doing year: ")
	print(mapyear)

	dir.create(paste0("../../data/rf/rast/",rfid,"/",ti), showWarnings=F)
	fout = paste0("../../data/rf/rast/",rfid,"/",ti,"/",ti,"_",mapyear,"_",rfid,".tif")
	if(file.exists(fout)){
		print("This year already done!")
	next
	}
	
	# process files
	system.time(
		rfout <- mclapply(rffiles, function(x)get(load(x)), mc.cores=detectCores())
		)
	rfdt = rbindlist(rfout)
	rm(rfout)
	setkey(rfdt, py, px)

	# make a fake raster to extract from
	LCrast = raster(paste0("../../data/aster/",ti,"/AST_DEM_",ti,"_slp.tif"))
	#LCrast[] = as.numeric(as.character(mapPxPy$lcmap))
	LCrast[] = rfdt$lcmap
	rm(rfdt)

	writeRaster(LCrast, fout, overwrite=T)
	if(file.exists(fout)){
		lapply(rffiles, file.remove, showWarnings=F)
	}
	rm(LCrast)
	gc()

}
# some testing for weird missinglines
#rfdt[, missingrows := sum(is.na(lcmap)), by = py]
#expectedPxPy[, missingrows := sum(is.na(lcmap)),by=py]
#expectedPxPy[, missingrows := sum(is.na(lcmap)),by=px]



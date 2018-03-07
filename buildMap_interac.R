######
#
#		For a given tile, take rf predictions and construct a map
#

library(rgdal)
library(raster)
library(data.table)
library(feather)
library(parallel)
library(foreach)
library(doParallel)

#mapyear = commandArgs(TRUE)[3]

print(ti)
print(rfid)
#source("../featureEngineering/featureFunctions.R")

#if(file.exists(fout)){
#	print("this one is done already")
#}
# assign epochs in order
#assignEpoch =function(yr_start, yr_end){
#
#	epoch = 0
#	if(yr_start > 2010)epoch=2014
#	if(yr_end < 1990)epoch=1986
#	if(1990 %in% yr_start:yr_end)epoch=1990
#	#if(1994 %in% yr_start:yr_end)epoch=1994
#	#if(1998 %in% yr_start:yr_end)epoch=1998
#	if(2002 %in% yr_start:yr_end)epoch=2002
#	if(2006 %in% yr_start:yr_end)epoch=2006
#	if(2010 %in% yr_start:yr_end)epoch=2010
#	return(epoch)
#
#}


# for manual runs
#rfid = "tc_20180219_k25_sub_mn_pam_rf"

# get data
#if(mapyear == 2010){
	rffiles = list.files(paste0("../../data/rf/predict_",rfid,"/",ti),
											 full.names=T)#,
											 #pattern="shade")

	rfout = mclapply(rffiles, function(x)get(load(x)), mc.cores=detectCores())
# don't forget shade classification!!!!
# do all years

#registerDoParallel(detectCores())
	expy = get(load("../../data/rf/expectedPxPy"))

for(mapyear in 2011:2014){

	print("doing year: ")
	print(mapyear)

	rfdt = rbindlist(rfout)
	fout = paste0("../../data/rf/rast/",ti,"_",mapyear,"_",rfid,".tif")

# pick by year
	rfdt = rfdt[yr_start <= mapyear & yr_end >= mapyear,]
	if(nrow(rfdt[yr_start <= mapyear & yr_end >= mapyear,]) < 36000000)
		print("WARNING! Incomplete map for this year")

	rfdt[,pxpy := paste0(px,'-',py)] 
#}else{
#	if(mapyear == 2006)epochs=c(2006)
#	if(mapyear == 2002)epochs=c(2002)
	#if(mapyear == 2002)epochs=c(2006,2002)
#	if(mapyear == 1998)epochs=c(2006,2002,1998)
#	if(mapyear == 1994)epochs=c(2006,2002,1998,1994)
#  #if(mapyear == 1990)epochs=c(2006,2002,1998,1994,1990)
#  if(mapyear == 1990)epochs=c(2002,1990)
#	if(mapyear == 1986)epochs=c(2006,2002,1998,1994,1990,1986)
#	if(mapyear == 2014)epochs=c(2014)

	# picking out jsut the relevant ones
#	buildEpochDT = function(i){
#		if(i %% 100 == 0){
#			print(paste0("reading file: ", i))
#		}
#		rffiles = unlist(lapply(epochs,
#										 function(x)list.files(paste0("../../data/rf/predict_",rfid,"_",x,"/",ti),full.names=T,pattern=paste0("_",i,"_",rfid,"_shade"))))
#		rffiles2010 = list.files(paste0("../../data/rf/predict_",rfid,"/",ti),
#														 full.names=T,
#														 pattern=paste0("_",i,"_",rfid, "_shade"))
#		rffiles = c(rffiles, rffiles2010)
#		
#		rfout = mclapply(rffiles, function(x)get(load(x)))#, mc.cores=detectCores())
#		rfdti = rbindlist(rfout)
#		rfdti[,pxpy := paste0(px,'-',py)] 
#		rfdti[,epoch := assignEpoch(yr_start,yr_end), by=1:nrow(rfdti)]
#		rfdti[,minpoch := min(epoch), by = pxpy]
#
#		rfdti = rfdti[minpoch == epoch,]
#		if(!is.data.table(rfdti)){
#			print(paste0("this one is not a data.table!!: ", i))
#			return(NULL)
#		}
#
#		return(rfdti)
#	}
#	rfdt_l = mclapply(0:2999, buildEpochDT, mc.cores=detectCores())
#	rfdt = rbindlist(rfdt_l)

#	rffiles = list.files(paste0("../../data/rf/predict_",rfid,"_",mapyear,"/",ti),
#											 full.names=T)
#}


### should be reading this in
#
#	setkey(expectedPxPy, pxpy)
#

# the data that goes into the raster
# expectedPxPy = as.data.table(expand.grid(0:5999, 0:5999))
# setnames(expectedPxPy, c("Var1", "Var2"), c("px", "py"))
# expectedPxPy[, pxpy := paste0(px, '-', py)]
# save(expectedPxPy, file="../../data/rf/expectedPxPy")

setkey(rfdt, pxpy)

# clean up dupes where break is on 2010 - pick the longest segment
rfdt[, seglength := yr_end - yr_start]
rfdt[, dupepxpy := .N, by = pxpy]

# for diagnosing Bh11v11 problems...
# rfdt_dup[pxpy == unique(rfdt_dup$pxpy)[sample(1:length(unique(rfdt_dup$pxpy)),1)],]

rfdt_u = rfdt[dupepxpy == 1,]
rfdt_dup = rfdt[dupepxpy > 1,]

# if there are duplicates, pick the longer segment
if(nrow(rfdt_dup) > 0){

	rfdt_dup[,longerseg := (seglength == max(seglength)), by = pxpy]
	rfdt_dedupe = rfdt_dup[longerseg==TRUE,]
	
	rfdt_dedupe[, c("longerseg"):=NULL]
	
	rfdt = rbindlist(list(rfdt_u, rfdt_dedupe))
}

setkey(rfdt, pxpy)
rfdt = unique(rfdt)
setkey(expy, pxpy)

mapPxPy = merge(expy,rfdt[,.(pxpy,lcmap)],all.x=T, by = "pxpy")
#mapPxPy = expectedPxPy[rfdt]
#expectedPxPy = merge(expectedPxPy,rfdt[,.(pxpy,lcmap)])#,all.x=T)

#above = readOGR("../../data/above_shp/",
#								"ABoVE_30mgrid_tiles_Final")

# get ABoVE tile
#abovedt = as.data.table(above@data)
#abovedt[, names(abovedt):=lapply(.SD, as.character)]
#abovedt[, names(abovedt):=lapply(.SD, as.numeric)]
#abovedt[, c("theBh", "theBv"):=.(Ahh * 6 + Bh, Avv*6 + Bv)]
#abovedt[,tileid := paste0("Bh",sprintf("%02d",theBh),"v",sprintf("%02d",theBv))]
#tileUID = abovedt[tileid == ti, UID]

# get the tile
#abovetile = above[above@data$UID == tileUID,]

# make a fake raster to extract from
LCrast = raster(paste0("../../data/aster/",ti,"/AST_DEM_",ti,"_slp.tif"))
#LCrast = raster(extent(abovetile),0)
#res(LCrast) = c(30,30)
#projection(LCrast) = projection(abovetile)

# still some dupes? 
#mapPxPy = expectedPxPy[, .(py.x, px.x, pxpy, lcmap)]
#setnames(mapPxPy, c("pxpy","px","py", "lcmap"))
mapPxPy = unique(mapPxPy)
setkey(mapPxPy, py, px)
mapPxPy = mapPxPy[!duplicated(mapPxPy$pxpy),] #a litte naive... need to thinko f a more intelligent solution to duplicates

LCrast[] = as.numeric(as.character(mapPxPy$lcmap))

# do some test plots...
# color mapping

writeRaster(LCrast, fout, overwrite=T)

}
# some testing for weird missinglines
#rfdt[, missingrows := sum(is.na(lcmap)), by = py]
#expectedPxPy[, missingrows := sum(is.na(lcmap)),by=py]
#expectedPxPy[, missingrows := sum(is.na(lcmap)),by=px]



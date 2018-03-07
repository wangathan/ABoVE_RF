######
#
#		For a given tile, take rf predictions and construct a map
#
#		Due to memory constraints, splitting this into two steps: 
#			- fill and dedupe predictions for each year and write to file
#			- construct map
#
#
#

#library(rgdal)
library(raster)
library(data.table)
#library(feather)
library(parallel)
#library(foreach)
#library(doParallel)


ti = commandArgs(TRUE)[1]
rfid = commandArgs(TRUE)[2]

print(ti)
print(rfid)
#source("../featureEngineering/featureFunctions.R")

# for manual runs
#rfid = "tc_20180219_k25_sub_mn_pam_rf"

# get data

	loadersaver = function(fi, mapyear){

		# get indt
		
		indt = get(load(fi))

		fout = paste0("../../data/rf/predict_",rfid,"/clean_predict/",ti,"/",mapyear,"/",basename(fi),"_clean_",mapyear)
		if(file.exists(fout))return(NULL)

		fpy = as.numeric(strsplit(fi,"_")[[1]][9])
		realpy = fpy*2

		# each feather covers two py and all px (0:5999)
		# check that they all exist
		lcdt = indt[yr_start <= mapyear & yr_end >= mapyear,]
		rowpxpy = data.table(px = rep(0:5999,2), py = c(rep(realpy,6000),rep(realpy+1,6000))) 

		# combine and see what's missing
		setkey(rowpxpy, py, px)
		setkey(lcdt, py, px)		

		rowpxpy = merge(rowpxpy,lcdt,all=T)
		rm(lcdt)

		# get missing rows and attempt to assign nearest-time-neighbor lc
		napxpy = rowpxpy[is.na(lcmap),]
		newlc <- sapply(1:nrow(napxpy), function(x)nafiller(napxpy[x,px],napxpy[x,py],mapyear,indt[px==napxpy[x,px] & py==napxpy[x,py],]))
		napxpy[, lcmap := newlc]
		rm(newlc)
		rm(indt)

		# so we don't hang on to the NA values
		rowpxpy = na.omit(rowpxpy)
		# get duplicated rows and attempt to prune to longest time segment lc
		rowpxpy[, dupes := .N, by = c("px", "py")]	

		# clean up dupes where break is on 2010 - pick the longest segment
		rowpxpy[, seglength := yr_end - yr_start]

		# for diagnosing Bh11v11 problems...
		# rowpxpy_dup[pxpy == unique(rowpxpy_dup$pxpy)[sample(1:length(unique(rowpxpy_dup$pxpy)),1)],]

		rowpxpy_u = rowpxpy[dupes == 1,]
		rowpxpy_dup = rowpxpy[dupes > 1,]
		

		# if there are duplicates, pick the longer segment
		if(nrow(rowpxpy_dup) > 0){
			rowpxpy_dup[,longerseg := (seglength == max(seglength)), by = c("px","py")]
			# if both segments are the same length, pick the earlier one
			rowpxpy_dup[,earlier := (yr_start == min(yr_start)), by = c("px", "py")]
			
			rowpxpy_dedupe = rowpxpy_dup[longerseg==TRUE,]
			
			# another dupe check to break seglength ties
			rowpxpy_dedupe[,dupes := .N, by = c("px","py")]

			rowpxpy_dedupe_u = rowpxpy_dedupe[dupes==1,]
			rowpxpy_dedupe_dup = rowpxpy_dedupe[dupes>1,]

			rowpxpy_dedupe_dedupe = rowpxpy_dedupe_dup[earlier==TRUE,]
		
			rowpxpy = rbindlist(list(rowpxpy_u[,.(py,px,lcmap)],
															 rowpxpy_dedupe_u[,.(py,px,lcmap)], # seglenth used to break year ties
															 rowpxpy_dedupe_dedupe[,.(py,px,lcmap)], # seglength ties are broken
															 napxpy[,.(py,px,lcmap)]))
			rm(rowpxpy_u)
			rm(rowpxpy_dedupe_u)
			rm(rowpxpy_dedupe_dedupe)
		}else{
			rowpxpy = rbindlist(list(rowpxpy[,.(py,px,lcmap)],
																napxpy[,.(py,px,lcmap)]))

		}
		rm(napxpy)

		rowpxpy = unique(rowpxpy)
		rowpxpy[,pxpy:=paste0(px,'-',py)]
		rowpxpy = rowpxpy[!duplicated(rowpxpy$pxpy),] #a litte naive... need to thinko f a more intelligent solution to duplicates
		
		if(nrow(rowpxpy) != 12000){
			print(paste0("ERROR! File: ", fi, " resulted in not-12000 row file!"))
		}
		setkey(rowpxpy, py, px)

		save(rowpxpy, file=fout)
	}

	nafiller = function(inpx,inpy,mapyear,pxlist) {

		# tested using ti Bh11v05, mapyear 2012, py = 5505, px = 3600
		# this is a end-of-series NA

		# get all predictions for the pixel

		# do the easy cases first
		if(nrow(pxlist)==0)return(NA)
		if(nrow(pxlist)==1)return(pxlist[1,lcmap])

		# compare all years to the mapyear
		pxlist[, yeardiff := min(c(mapyear-yr_start,mapyear-yr_end)), by=1:nrow(pxlist)]

		if(min(pxlist$yeardiff) > 5)return(NA)

		# pick the closest one
		closelc = pxlist[yeardiff == min(pxlist$yeardiff),lcmap]
		return(closelc[1])

	}



	rffiles = list.files(paste0("../../data/rf/predict_",rfid,"/",ti),
											 full.names=T)#,
											 #pattern="shade")

for(mapyear in 1985:2014){

	print("cleaning year: ")
	print(mapyear)

	rastout = paste0("../../data/rf/rast/",rfid,"/",ti,"/",ti,"_",mapyear,"_",rfid,".tif")
	if(file.exists(rastout)){
		print("this raster already cleaned and built!")
		next
	}

	dir.create(paste0("../../data/rf/predict_",rfid,"/clean_predict/",ti,"/",mapyear), showWarnings=F, recursive=T)
	
	# process files
	system.time(
		mclapply(rffiles, loadersaver, mapyear = mapyear, mc.cores=detectCores())
		)

	print("done!")
	gc()
}

warnings()



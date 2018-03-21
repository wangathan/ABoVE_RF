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

		fout = paste0("../../data/rf/predict_",rfid,"/clean_predict/",ti,"/",mapyear,"/",basename(fi),"_clean_",mapyear)
		if(file.exists(fout))return(NULL)
		
		indt = get(load(fi))

    # make it to always pick the prediction index correctly based on the name
    splitting= strsplit(fi,"_")[[1]]
    predictindex=grep("rfpredict",splitting)
    fpy = as.numeric(splitting[predictindex+1])
    
    realpy = fpy*2

		# each feather covers two py and all px (0:5999)
		# check that they all exist
		lcdt = indt[yr_start <= mapyear & yr_end >= mapyear,]
    lcdt = unique(lcdt)
		rowpxpy = data.table(px = rep(0:5999,2), py = c(rep(realpy,6000),rep(realpy+1,6000))) 

		# combine and see what's missing
		setkey(rowpxpy, py, px)
		setkey(lcdt, py, px)		

		rowpxpylc = merge(rowpxpy,lcdt,all=T)
		#rm(lcdt)

		# get missing rows and attempt to assign nearest-time-neighbor lc
		napxpy = rowpxpylc[is.na(lcmap),]
		newlc <- sapply(1:nrow(napxpy), function(x)nafiller(napxpy[x,px],napxpy[x,py],mapyear,indt[px==napxpy[x,px] & py==napxpy[x,py],]))
		napxpy[, lcmap := newlc]
		#rm(newlc)
		#rm(indt)

		# so we don't hang on to the NA values
		rowpxpylc = na.omit(rowpxpylc)
		# get duplicated rows and attempt to prune to longest time segment lc
		rowpxpylc[, dupes := .N, by = c("px", "py")]	

		# clean up dupes where break is on 2010 - pick the longest segment
		rowpxpylc[, seglength := yr_end - yr_start]

		# for diagnosing Bh11v11 problems...
		# rowpxpylc_dup[pxpy == unique(rowpxpylc_dup$pxpy)[sample(1:length(unique(rowpxpylc_dup$pxpy)),1)],]

		rowpxpylc_u = rowpxpylc[dupes == 1,]
		rowpxpylc_dup = rowpxpylc[dupes > 1,]
		

		# if there are duplicates, pick the newest segment
		if(nrow(rowpxpylc_dup) > 0){
			#rowpxpylc_dup[,longerseg := (seglength == max(seglength)), by = c("px","py")]
			# if both segments are the same length, pick the earlier one
			rowpxpylc_dup[,earlier := (yr_start <= min(yr_start)), by = c("px", "py")]
			rowpxpylc_dedupe = rowpxpylc_dup[earlier!=TRUE,]

      rowpxpylc_dedupe = rowpxpylc_dup[yr_start == mapyear,]
			#rowpxpylc_dedupe = rowpxpylc_dup[longerseg==TRUE,]
			
			# another dupe check to break seglength ties
			#rowpxpylc_dedupe[,dupes := .N, by = c("px","py")]

			#rowpxpylc_dedupe_u = rowpxpylc_dedupe[dupes==1,]
			#rowpxpylc_dedupe_dup = rowpxpylc_dedupe[dupes>1,]

			#rowpxpylc_dedupe_dedupe = rowpxpylc_dedupe_dup[earlier==TRUE,]
		
			rowpxpylc = rbindlist(list(rowpxpylc_u[,.(py,px,lcmap)],
                               rowpxpylc_dedupe[,.(py,px,lcmap)],
															 #rowpxpylc_dedupe_u[,.(py,px,lcmap)], # seglenth used to break year ties
															 #rowpxpylc_dedupe_dedupe[,.(py,px,lcmap)], # seglength ties are broken
															 napxpy[,.(py,px,lcmap)]))
			rm(rowpxpylc_u)
			#rm(rowpxpylc_dedupe_u)
			#rm(rowpxpylc_dedupe_dedupe)
		}else{
			rowpxpylc = rbindlist(list(rowpxpylc[,.(py,px,lcmap)],
																napxpy[,.(py,px,lcmap)]))

		}
		rm(napxpy)

		rowpxpylc = unique(rowpxpylc)
		rowpxpylc[,pxpy:=paste0(px,'-',py)]
		rowpxpylc = rowpxpylc[!duplicated(rowpxpylc$pxpy),] #a litte naive... need to thinko f a more intelligent solution to duplicates
		
		if(nrow(rowpxpylc) != 12000){
			print(paste0("ERROR! File: ", fi, " resulted in not-12000 row file!"))
		}
		setkey(rowpxpy, py, px)
    setkey(rowpxpylc,py,px)
    row_out = merge(rowpxpy,rowpxpylc,all.x=T)

		save(row_out, file=fout)
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

		# pick the closest one and don't pick backwards
    pxlist = pxlist[yr_start >= mapyear,]
		closelc = pxlist[yeardiff == min(pxlist$yeardiff),lcmap]
		return(closelc[1])

	}



	rffiles = list.files(paste0("../../data/rf/predict_",rfid,"/",ti),
											 full.names=T)

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



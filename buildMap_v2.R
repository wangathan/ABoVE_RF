######
#
#		For a given tile, take rf predictions and construct a map
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

#mapyear = commandArgs(TRUE)[3]

print(ti)
print(rfid)
#source("../featureEngineering/featureFunctions.R")

# for manual runs
#rfid = "tc_20180219_k25_sub_mn_pam_rf"

# get data

loader = function(fi, mapyear){
  # get indt
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

    # pick the newest if the duplicate is from a break
    rowpxpylc_dedupe = rowpxpylc_dup[yr_start == mapyear,]
    rowpxpylc_dedupe[,pxpy:=paste0(px,"-",py)]
    rowpxpylc_dup[,pxpy:=paste0(px,"-",py)]

    # if there's a dupe that's not related to contemporaneous break
    # just pick the first one and hope for the best... maybe print a note
    rowpxpylc_nonbreakdupe = rowpxpylc_dup[!pxpy %in% rowpxpylc_dedupe$pxpy,]
    rowpxpylc_nonbreakdupe[,theFirst := .I, by = pxpy]
    rowpxpylc_out = rbind(rowpxpylc_dedupe, rowpxpylc_nonbreakdupe[theFirst==1,names(rowpxpylc_dup),with=F])


    if(nrow(rowpxpylc_nonbreakdupe) > 0 ){
      print(paste0("File: ", fi, " has weird duplicates!"))
    }

    #rowpxpylc_dedupe = rowpxpylc_dup[longerseg==TRUE,]

    # another dupe check to break seglength ties
    #rowpxpylc_dedupe[,dupes := .N, by = c("px","py")]

    #rowpxpylc_dedupe_u = rowpxpylc_dedupe[dupes==1,]
    #rowpxpylc_dedupe_dup = rowpxpylc_dedupe[dupes>1,]

    #rowpxpylc_dedupe_dedupe = rowpxpylc_dedupe_dup[earlier==TRUE,]

    rowpxpylc = rbindlist(list(rowpxpylc_u[,.(py,px,lcmap)],
                               rowpxpylc_out[,.(py,px,lcmap)],
#                               rowpxpylc_dedupe[,.(py,px,lcmap)],
#                               rowpxpylc_nonbreakdupe[,.(py,px,lcmap)],
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

  rowpxpylc = unique(rowpxpylc)
  rowpxpylc[,pxpy:=paste0(px,'-',py)]
  rowpxpylc = rowpxpylc[!duplicated(rowpxpylc$pxpy),] #a litte naive... need to thinko f a more intelligent solution to duplicates

  if(nrow(rowpxpylc) != 12000){
    print(paste0("ERROR! File: ", fi, " resulted in not-12000 row file!"))
  }
  setkey(rowpxpy, py, px)
  setkey(rowpxpylc,py,px)
  row_out = merge(rowpxpy,rowpxpylc,all.x=T)

  rm(rowpxpy)
  rm(rowpxpylc)
  rm(napxpy)
  rm(rowpxpylc_u)
  rm(rowpxpylc_dup)
  gc()

  return(row_out)
  #save(row_out, file=paste0('temp/',ti,'/',basename(fi)))
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
  if(nrow(pxlist[yr_start>=mapyear,])>0){
    pxlist = pxlist[yr_start >= mapyear,]
  }

  closelc = pxlist[yeardiff == min(pxlist$yeardiff),lcmap]
  return(closelc[1])

}

for(mapyear in 1984:2014){

	rffiles = list.files(paste0("../../data/rf/predict_",rfid,"/",ti),
											 full.names=T)#,
											 #pattern="shade")

  if(length(rffiles) < 3000){
    print("hey we're missing prediction files here!!")
#    stop
  }

	print("doing year: ")
	print(mapyear)

	dir.create(paste0("../../data/rf/rast/",rfid,"/",ti), showWarnings=F,recursive=T)
	fout = paste0("../../data/rf/rast/",rfid,"/",ti,"/",ti,"_",mapyear,"_",rfid,".tif")
	#if(file.exists(fout)){
	#	print("This year already done!")
	#	next
	#}
	

	# process files
	system.time(
		rfout <- mclapply(rffiles, loader, mapyear = mapyear, mc.cores=detectCores())
		)
	rfdt = rbindlist(rfout)
	setkey(rfdt, py, px)

	# make a fake raster to extract from
	LCrast = raster(paste0("../../data/aster/",ti,"/AST_DEM_",ti,"_slp.tif"))
	#LCrast[] = as.numeric(as.character(mapPxPy$lcmap))
  

  if(ti %in% c("Bh06v00", "Bh07v00")){

   # merge by pxpy...
    expxpy = as.data.table(expand.grid(0:5999,0:5999))
    setnames(expxpy, c("py","px"))

#   setkey(rfdt, py, px)
   setkey(expxpy, py, px)

   rfdt2 = merge(expxpy, rfdt, all.x=T)  
  rfdt = rfdt2

  }
  if(nrow(rfdt) < 6000*6000){
    print("bad map!!")
  }


	LCrast[] = rfdt$lcmap

	writeRaster(LCrast, fout, overwrite=T)
	rm(LCrast)
	rm(rfdt)
  rm(rfout)

  gc()
}
# some testing for weird missinglines
#rfdt[, missingrows := sum(is.na(lcmap)), by = py]
#expectedPxPy[, missingrows := sum(is.na(lcmap)),by=py]
#expectedPxPy[, missingrows := sum(is.na(lcmap)),by=px]



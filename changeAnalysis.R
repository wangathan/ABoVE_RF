##################
#
#
#		For any given tile, read in and remap prediction files 
#     and determine what kind of changes in each pixel
#
#   1) year of last change (so, age)
#   2) net change (greening or browning?)
#   3) number of real changes 
#
#

library(raster)
library(data.table)
library(parallel)

ti = commandArgs(TRUE)[1]
rfid = commandArgs(TRUE)[2]

# get all files
prdfiles = list.files(paste0("../../data/rf/predict_",rfid,"/",ti),
                      full.names=T)


catalogFile = function(fi){

  # read a file
  prdrow = get(load(fi))

  # determine all the px,py values (so, all the pixels)
  prdrow[, pixel := paste0(px,"-",py)]
  setkey(prdrow,pixel)
  rowpixels = unique(prdrow$pixel)

  # remap
  prdrow[lcmap %in% c(1,15,16,31), remap:= 1] #Everg F                           
  prdrow[lcmap %in% c(25,34), remap:= 2] #Decid F                           
  prdrow[lcmap %in% c(20,30), remap:= 3] #Mixed F                           
  prdrow[lcmap %in% c(3,4,44,49,52,53), remap:= 4] #Bog Forest                          
  
  prdrow[lcmap %in% c(27,39), remap:= 5] #Low Shrub                          
  prdrow[lcmap %in% c(19,23,50), remap:= 6] #High Shrub                          
  prdrow[lcmap %in% c(7,18,21,22,41,51,54), remap:= 7] #Sparse Shrub                          
  prdrow[lcmap %in% c(14,40), remap:= 8] #Grass                          
  prdrow[lcmap %in% c(42), remap:=9] #Tussock Tundra
  prdrow[lcmap %in% c(8,26,28,36,37,48), remap:= 10] #Sparse Grass/Lichen                
                                                                            
  prdrow[lcmap %in% c(2,10,11,35,46), remap:= 11] #Fen                            
  prdrow[lcmap %in% c(32), remap:= 12] #Bog                               
  prdrow[lcmap %in% c(12), remap:= 13] #Shallows                            

  prdrow[lcmap %in% c(13,17,29,43,45,55), remap:= 14] #Barren                        
  prdrow[lcmap %in% c(5,6,9,24,33,38,47), remap:= 15] #Water                             
  prdrow[, numbreaks := .N-1, by = pixel]

  # merge like rows
  system.time(prdrow_combined <- rbindlist(mclapply(unique(prdrow$pixel), combineRows, indt = prdrow, mc.cores=detectCores()),
                                           use.names=T))

  # determine change metrics...
  # if we're keeping all segments no need to build up all the summary metrics
#  # number of changes
#  prdrow_combined[, numchanges := .N-1, by = pixel]
#  # last change
#  prdrow_combined[, lastchange := max(yr_start), by = pixel]
#  # sort for retrieving first/last lc
#  setkey(prdrow_combined,pixel)
#  # first LC
#
#  # get the first row of each pixel
#  # J() gives a join table, see https://stats.stackexchange.com/questions/7884/fast-ways-in-r-to-get-the-first-row-of-a-data-frame-grouped-by-an-identifier
#  firstlc = prdrow_combined[J(unique(pixel)),mult="first"] 
#  lastlc  = prdrow_combined[J(unique(pixel)),mult="last"] 
#
#  firstlc = firstlc[,.(pixel, remap)]
#  lastlc = lastlc[,.(pixel, remap)]
#
#  setnames(firstlc, c("pixel", "firstlc"))
#  setnames(lastlc, c("pixel", "lastlc"))
#
#  # combine
#  prdrow_combined = prdrow_combined[firstlc][lastlc]

  # clean up
  prdrow_combined[,pixel:=NULL]

  # get data
  return(prdrow_combined)

}

combineRows = function(inpixel, indt){

  # isolate pixel
  pxdt = indt[pixel == inpixel,]
  # sort by order
  setkey(pxdt, yr_start)

  # go through rows in order and determine if they should be merge

  # if no changes in pixel, return the pixel
  if(nrow(pxdt)==1)return(pxdt[,.(tile,px,py,yr_start,yr_end,pixel,remap, numbreaks)])

  # determine sequences
  lcseq = rle(pxdt$remap)

  seqvec = numeric()#numeric(length=nrow(pxdt))
  # assign sequences
  for(i in 1:length(lcseq$lengths)){
    seqvec=c(seqvec, rep(i, lcseq$lengths[i]))
  }
  pxdt[,seqID := seqvec]

  # synchronize sequences
  pxdt[,seq_start := min(yr_start,na.rm=T), by = seqID]
  pxdt[,seq_end   := max(yr_end,na.rm=T), by = seqID]

  # isolate sequences
  pxdt[, c("yr_start","yr_end","lcmap", "seqID"):=NULL]
  setnames(pxdt, c("seq_start","seq_end"), c("yr_start","yr_end"))

  return(unique(pxdt))
}


# read and save them for later analysis and mapping

dir.create(paste0("../../data/rf/remap_",rfid,"/",ti),
           recursive=T,showWarnings=F)

for ( f in 1:length(prdfiles) ) {

  # get file
  pfi = prdfiles[f]
  if(f %% 100 == 0)print(f)

  # build name
  fout = paste0("../../data/rf/remap_",rfid,"/",ti,"/",gsub("rfpredict","rfremap",basename(pfi)))
  if(file.exists(fout))next

  # do
  rmpdt = catalogFile(pfi)

  # save
  save(rmpdt, file = fout)

}
print("done!")

###############
#
#
#		For any given tile, read in all rasters available and 
#			construct a big data.table to catalog all the land cover 
#			changes
#
#
#

library(raster)
library(data.table)
library(parallel)

ti = commandArgs(TRUE)[1]
rfid = commandArgs(TRUE)[2]

# figure out which epochs were estimated... or for now just stick to the three

rast1990 = raster(paste0("../../data/rf/rast/",ti,"_1990_",rfid,".tif"))
rast2002 = raster(paste0("../../data/rf/rast/",ti,"_2002_",rfid,".tif"))
rast2010 = raster(paste0("../../data/rf/rast/",ti,"_2010_",rfid,".tif"))

pxpy = as.data.table(expand.grid(0:5999, 0:5999))
setnames(pxpy, c("px", "py"))
setkey(pxpy, py, px)

pxpy[, lc1990:=values(rast1990)]
pxpy[, lc2002:=values(rast2002)]
pxpy[, lc2010:=values(rast2010)]

#"Mixed F", #1      
#"Open F", #2       
#"Recov Shrub", #3  
#"Shallows", #4     
#"Wetland", #5      
#"Woody Wetland", #6
#"Everg F", #7      
#"Everg F 2", #8    
#"Decid F", #9      
#"Barren", #10      
#"Water", #11       
#"Grass", #12       
#"Secondary F", #13 
#"Wetland 2", #14   
#"Snow/Ice" #NA    

pxpy[, change1990_2010 := lc1990!=lc2010]
# wetlands thaw
pxpy[lc1990 %in% c(1,7,8,9,13) & lc2010 %in% c(5,6,14),thawloss := 1]

# forest gain
pxpy[!(lc1990 %in% c(1,7,8,9,13)) & lc2010 %in% c(1,7,8,9,13),forestgain := 1]

forest1990 = nrow(pxpy[lc1990 %in% c(1,7,8,9,13),])
forest2002= nrow(pxpy[lc2002 %in% c(1,7,8,9,13),])
forest2010= nrow(pxpy[lc2010 %in% c(1,7,8,9,13),])


thawlossRast= copy(rast2010)
thawlossRast[] = pxpy[,thawloss]

forestGainRast= copy(rast2010)
forestGainRast[] = pxpy[,forestgain]

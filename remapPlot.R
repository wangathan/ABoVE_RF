#################
#
#		For aggregating classes. This might have to be edited/defined
#			more or less manually.
#
#
#

library(data.table)
library(raster)
library(parallel)
library(rasterVis)
library(ggplot2)
library(foreach)
library(doParallel)

ti = commandArgs(TRUE)[1]
rfid = commandArgs(TRUE)[2]

#mapyear = commandArgs(TRUE)[3]
#registerDoParallel(detectCores())

for(mapyear in 1985:2014){
	print(mapyear)
	dir.create(paste0("../../plots/rf/lcmap/",ti,"/remap"),showWarnings=F,recursive=T)
	dir.create(paste0("../../data/rf/rast/",rfid,"/",ti,"/remap/"),showWarnings=F,recursive=T)
  fout = paste0("../../plots/rf/lcmap/",ti,"/",ti,"_",mapyear,"_",rfid,"_remap.png")
  fout_r = paste0("../../data/rf/rast/",rfid,'/',ti,'/remap/',ti,"_",mapyear,"_",rfid,"_remap.tif")

	#if(file.exists(fout) & file.exists(fout_r))
#		next

# load lcmap
#lcmap = raster(paste0("../../data/rf/rast/",ti,"_",mapyear,"_",rfid,".tif"))
lcmap = raster(paste0("../../data/rf/rast/",rfid,"/",ti,"/",ti,"_",mapyear,"_",rfid,".tif"))

# edit this based on the lcmap legend we have
# goal legend:
# 1 - Everg F
# 2 - Decid F
# 3 - Mixed F
# 4 - Woodland/Shrub
# 5 - Grass
# 6 - Wetland
# 7 - Woody Wetland
# 8 - Barren
# 9 - Water
# 10 - Shallows
# 11 - Shadow
if(rfid == "clustrf20_20171201"){
#newrast = copy(lcmap)
remapdt = data.table(lcmap = values(lcmap))
remapdt[lcmap %in% c(5,6,9, 13), newlc:= 1] #Everg
remapdt[lcmap %in% c(8), newlc:= 2] #Decid
remapdt[lcmap %in% c(7), newlc:= 3] #Mixed
remapdt[lcmap %in% c(3,4), newlc:= 4] #Woodland
remapdt[lcmap %in% c(17, 16,19), newlc:= 5] #Grass
remapdt[lcmap %in% c(10,15,20), newlc:= 6] #Wetland
remapdt[lcmap %in% c(1,11), newlc:= 7] #Woody Wetland
remapdt[lcmap %in% c(12,14), newlc:= 8] #Barren
remapdt[lcmap %in% c(18), newlc:= 9] #Water
remapdt[lcmap %in% c(2), newlc:= 10] #Shallows
remapdt[lcmap %in% c(21), newlc:= 11] #Shadow

lcmap[] = remapdt$newlc
# goal colors (old)
lcmap_col = c("#006633", #everg
							"#00DD00", # decid
							"#339966", # mixed
							"#666633", #woodland
							"#FFFF99", #grass
							"#996699", #wetland
							"#330033", #woody wetland
							"#AAAAAA", #barren
							"#3333FF", #water
							"#3399CC", #shallows
							"#555555") #shadow

lcmap_leg = c("Everg F",
							"Decid F",
							"Mixed F",
							"Woodland/Shrub",
							"Grass",
							"Wetland",
							"Woody Wetland",
							"Barren", 
							"Water",
							"Shallows",
							"Shadow")
}
#if(rfid == "tc_20180219_k25_mn_sub_pam_rf"){
##newrast = copy(lcmap)
#remapdt = data.table(lcmap = values(lcmap))
#remapdt[lcmap %in% c(17,21), newlc:= 1] #Everg
#remapdt[lcmap %in% c(15,13), newlc:= 2] #Decid
#remapdt[lcmap %in% c(9,14), newlc:= 3] #Mixed
#remapdt[lcmap %in% c(11), newlc:= 4] #Open ENF
#remapdt[lcmap %in% c(6), newlc:= 5] #Open DBF
#remapdt[lcmap %in% c(23), newlc:= 6] #Open MF
#remapdt[lcmap %in% c(3,25), newlc:= 7] #Shrub
#remapdt[lcmap %in% c(2,12,16), newlc:= 8] #Grass
#
#remapdt[lcmap %in% c(1), newlc:= 9] #Fen
#remapdt[lcmap %in% c(8,10), newlc:= 10] #Bog
#remapdt[lcmap %in% c(19), newlc:= 11] #Shrubby bog
#remapdt[lcmap %in% c(22,24), newlc:= 13] #Barren
#remapdt[lcmap %in% c(18,20), newlc:= 14] #Sparse Grass/Lichen
#remapdt[lcmap %in% c(7,5), newlc:= 15] #Water
#remapdt[lcmap %in% c(4), newlc:= 16] #Shallows
##remapdt[lcmap %in% c(2), newlc:= 17] #Lichen
#
#lcmap[] = remapdt$newlc
#}
if(rfid == "tc_20180319_k50_pam_rf"){ # after a tour of street view #2
  #newrast = copy(lcmap)                                                    
  remapdt = data.table(lcmap = values(lcmap))                               
  remapdt[lcmap %in% c(32,48,20), newlc:= 1] #Everg F                           
  remapdt[lcmap %in% c(10,17,25), newlc:= 2] #Decid F                           
  #remapdt[lcmap %in% c(25), newlc:= 3] #Mixed F                           
  remapdt[lcmap %in% c(15,37,41,46), newlc:= 3] #Everg W                           
  remapdt[lcmap %in% c(28), newlc:= 4] #Decid W                           
  #remapdt[lcmap %in% c(4), newlc:= 6] #Mixed W                           
  remapdt[lcmap %in% c(24), newlc:= 5] #Bog Forest                          
  
  remapdt[lcmap %in% c(1,44,12,14), newlc:= 6] #Low Shrub                          
  remapdt[lcmap %in% c(13,18,42,4), newlc:= 7] #High Shrub                          
  remapdt[lcmap %in% c(3,14,30,45,49), newlc:= 8] #Sparse Shrub                          
  remapdt[lcmap %in% c(11,7,27), newlc:= 9] #Grass                          
  remapdt[lcmap %in% c(21), newlc:=10] #Tussock Tundra
  remapdt[lcmap %in% c(29,2,34,38,47,50), newlc:= 11] #Sparse Grass/Lichen                
                                                                            
  remapdt[lcmap %in% c(19,22), newlc:= 12] #Fen                            
  remapdt[lcmap %in% c(9,43), newlc:= 13] #Bog                               
  remapdt[lcmap %in% c(5), newlc:= 14] #Shallows                            

  remapdt[lcmap %in% c(26,23,35,33,36,39,40), newlc:= 15] #Barren                        
  remapdt[lcmap %in% c(6,8,16,31), newlc:= 16] #Water                             
                                                                            
  lcmap[] = remapdt$newlc                                                   

  lcmap_col = c("#004400", #evergF
							"#00CC00", #decidF
							#"#008800", #mixedF

              "#336633", #evergW
							"#22BB22", #decidW
							#"#339933", #mixedW
							"#799B2B", #bogW

							"#845A06", #low shrub
							"#AD3714", #high shrub
							"#AFA377", #sparse shrub
							"#EAC856", #grass
							"#CE7D0C", #tussock
							"#DFE5A2", #sparse veg low

							"#4DC183", #Fen
							"#F7A765", #Bog
							"#75ACFF", #Shallows

							"#888888", #barren
							"#3333FF") #water

lcmap_leg = c("Everg F",
							"Decid F",
							#"Mixed F",

              "Everg W",
							"Decid W",
							#"Mixed W",
              "Bog-Forest",

							"Low Shrub",
							"Shrub",
							"Sparse Shrub",
							"Grass",
							"Tussock Tundra",
							"Sparse Grass", 

							"Fen",
							"Bog",
              "Shallows",

              "Barren",
              "Water")
}                                                                         


if(rfid == "tc_20180219_k25_mn_sub_pam_rf"){ # after a tour of street view
  #newrast = copy(lcmap)                                                    
  remapdt = data.table(lcmap = values(lcmap))                               
  remapdt[lcmap %in% c(17,21), newlc:= 1] #Everg                            
  remapdt[lcmap %in% c(15,13), newlc:= 2] #Decid                            
  remapdt[lcmap %in% c(9, 14), newlc:= 3] #Mixed                            
  remapdt[lcmap %in% c(11), newlc:= 4] #Bog Forest                          
  remapdt[lcmap %in% c(19,3,25), newlc:= 5] #Shrub                          
  remapdt[lcmap %in% c(2,6,16), newlc:= 6] #Grass                           
                                                                            
  remapdt[lcmap %in% c(23,12,1), newlc:= 7] #Fen                            
  remapdt[lcmap %in% c(8,10), newlc:= 8] #Bog                               
  remapdt[lcmap %in% c(18,22,24), newlc:= 9] #Barren                        
  remapdt[lcmap %in% c(20), newlc:= 10] #Sparse Grass/Lichen                
  remapdt[lcmap %in% c(7,5), newlc:= 11] #Water                             
  remapdt[lcmap %in% c(4), newlc:= 12] #Shallows                            
                                                                            
  lcmap[] = remapdt$newlc                                                   
# goal colors (tc_20180219_k25_mn_sub_pam_rf)                  
lcmap_col = c("#003300", # everg     
              "#009900", # decid     
              "#669933", # mixed     
              "#FF9966", # Bog forest
              "#666633", # shrub     
              "#FFFF99", # grass     
              "#CC66FF", # fen       
              "#CC3355", # bog       
              "#AAAAAA", # barren    
              "#CCEECC", # sparse veg
              "#3333FF", # water     
              "#3399CC") # shallows  

lcmap_leg = c("Everg F",             
              "Decid F",             
              "Mixed F",             
              "Bog/Forest",          
              "Shrub",               
              "Grass",               
              "Fen",                 
              "Bog",                 
              "Barren",              
              "Sparse Veg",          
              "Water",               
              "Shallows")            
}                                                                         

if(rfid == "clustrf15_20171201"){
#newrast = copy(lcmap)
remapdt = data.table(lcmap = values(lcmap))
remapdt[lcmap %in% c(6,7), newlc:= 1] #Everg
remapdt[lcmap %in% c(9,13), newlc:= 2] #Decid
remapdt[lcmap %in% c(1), newlc:= 3] #Mixed
remapdt[lcmap %in% c(3,15), newlc:= 4] #Woodland
remapdt[lcmap %in% c(12, 14), newlc:= 5] #Grass
remapdt[lcmap %in% c(5,8), newlc:= 6] #Wetland
remapdt[lcmap %in% c(2,15), newlc:= 7] #Woody Wetland
remapdt[lcmap %in% c(10), newlc:= 8] #Barren
remapdt[lcmap %in% c(11), newlc:= 9] #Water
remapdt[lcmap %in% c(4), newlc:= 10] #Shallows
remapdt[lcmap %in% c(16), newlc:= 11] #Shadow

lcmap[] = remapdt$newlc
}


## goal colors (new)
#lcmap_col = c("#003300", # everg
#							"#009900", # decid
#							"#669933", # mixed
#							"#006600", # o everg
#							"#66FF66", # o decid
#							"#66BB00", # o mixed
#							"#666633", # shrub
#							"#FFFF99", # grass
#							"#CC66FF", # fen
#							"#FF6666", # bog
#							"#FF9966", # woody wetland
#							"#AAAAAA", # barren
#							"#CCEECC", # sparse veg
#							"#3333FF", # water
#							"#3399CC", # shallows
#							"#FFB3B3") # lichen
#
#lcmap_leg = c("Everg F",
#							"Decid F",
#							"Mixed F",
#							"Everg W",
#							"Decid W",
#							"Mixed W",
#							"Shrub",
#							"Grass",
#							"Fen",
#							"Bog",
#							"Woody Wetland",
#							"Barren", 
#							"Sparse Veg", 
#							"Water",
#							"Shallows",
#							"Lichen")


# for analyzing lake drainage in old crow flats
#lakeext = extent(-1708659, -1697033, 3688252, 3698762)
#lakemap = crop(lcmap, lakeext)

plotBS = gplot(lcmap, maxpixels = ncell(lcmap)/3) +
	geom_raster(aes(x, y, fill=factor(value))) +
	coord_equal() +
	scale_fill_manual(name="Land Cover",
										values = lcmap_col,
										labels = lcmap_leg) +
ggtitle(paste0("Land Cover for ",ti," in ", mapyear)) +
theme_void()


	writeRaster(lcmap, fout_r, overwrite=T)
ggsave(fout, plotBS, height=6,width=7,units='in')
}

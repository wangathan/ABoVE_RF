################
#
#		A small scale study
#
#
#
library(raster)
library(parallel)
library(ggalluvial)
library(rasterVis)
library(data.table)
library(ggplot2)
library(alluvial)
# goal colors
lcmap_col = c("#006633", #everg
							"#00DD00", # decid
							"#339966", # mixed
							"#666633", #woodland
							"#FFFF99", #grass
							"#663366", #wetland
							"#FF9966", #woody wetland
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



# a function to loop through epochs, load raster, crop it out, and summarize it
examineRaster = function(epoch, type){

	if(type=="fgrowth"){
		fin = paste0("../../data/rf/rast/Bh12v11_",epoch,"_clustrf20_20171201.tif")
		subext = extent(-1225664, -1112779, 2630275, 2660427)
	}

	if(type=="wetlandexpand"){
		fin = paste0("../../data/rf/rast/Bh11v10_",epoch,"_clustrf20_20171201.tif")
	}

	if(type=="lakechange"){
		fin = paste0("../../data/rf/rast/Bh09v05_",epoch,"_clustrf20_20171201.tif")
		subext = extent(-1707000, -1699000, 3688252, 3695000)
	}
	rast = crop(raster(fin),subext)

	# plot a mini
	# extract some vals
	rastdt = data.table(clustrf20 = values(rast))
	# remap
	rastdt[clustrf20 %in% c(5,6,9, 13), newlc:= 1] #Everg
	rastdt[clustrf20 %in% c(8), newlc:= 2] #Decid
	rastdt[clustrf20 %in% c(7), newlc:= 3] #Mixed
	rastdt[clustrf20 %in% c(3,4), newlc:= 4] #Woodland
	rastdt[clustrf20 %in% c(17, 16,19), newlc:= 5] #Grass
	rastdt[clustrf20 %in% c(10,15,20), newlc:= 6] #Wetland
	rastdt[clustrf20 %in% c(1,11), newlc:= 7] #Woody Wetland
	rastdt[clustrf20 %in% c(12,14), newlc:= 8] #Barren
	rastdt[clustrf20 %in% c(18), newlc:= 9] #Water
	rastdt[clustrf20 %in% c(2), newlc:= 10] #Shallows
	rastdt[clustrf20 %in% c(21), newlc:= 11] #Shadow

	rastdt[,clustrf20:=NULL]

	#rast[] = rastdt$newlc	
	#rast[1:11]=1:11
#plotBS = gplot(rast, maxpixels = ncell(rast)) +
#	geom_raster(aes(x, y, fill=factor(value))) +
#	coord_equal() +
#	scale_fill_manual(name="Land Cover",
#										values = lcmap_col,
#										labels = lcmap_leg) +
#ggtitle(paste0(type,"_",epoch)) +
#theme_void()

#	ggsave(paste0("../../plots/rf/subplots/",type,"_",epoch,".png"),
#				 plotBS,
#				 width=5,
#				 height=3,
#				 units='in')

	setnames(rastdt, "newlc", paste0("lc_",epoch))
	return(rastdt)
		                                                     
}
epochs = c(2006,2010)
lakedts_l = mclapply(epochs,examineRaster,type="lakechange", mc.cores=detectCores())

lakedts = do.call(cbind, lakedts_l)

# Barren 1
# Grass/shrub 2
# Forest 3
# Water 4
# Wetland 5

## preparing alluvial plot
lakedts[,i:=1:nrow(lakedts)]
lakemelt = melt(lakedts, id.var="i")
lakemelt[,yr:=tstrsplit(variable,"_")[2]]

lakemelt[value %in% c(8), sublc := 1]
lakemelt[value %in% c(1,2,3,4,5), sublc := 2]
lakemelt[value %in% c(9,10,11), sublc := 3]
lakemelt[value %in% c(6,7), sublc := 4]

levels(lakemelt$sublc) = rev(levels(lakemelt$sublc))
lakemelt[,yr:=as.numeric(yr)]
lakemelt[,freq:=.N,by=c("sublc","yr")]
#lakemelt[value==1,lc:="everg f"]
#lakemelt[value==2,lc:="decid f"]
#lakemelt[value==3,lc:="mixed f"]
#lakemelt[value==4,lc:="shrub"]
#lakemelt[value==5,lc:="grass"]
#lakemelt[value==6,lc:="wetland"]
#lakemelt[value==7,lc:="woody wetland"]
#lakemelt[value==8,lc:="barren"]
#lakemelt[value==9,lc:="water"]
#lakemelt[value==10,lc:="shallows"]
#lakemelt[value==11,lc:="shadow"]

lakemelt[sublc == 1, lc:="Barren"]
lakemelt[sublc == 2, lc:="Vegetated"]
lakemelt[sublc == 3, lc:="Water"]
lakemelt[sublc == 4, lc:="Wetland"]
lakemelt[,lc :=as.factor(lc)]
lakemelt[,c("value","variable","sublc"):=NULL]

gglake = ggplot(lakemelt,
			 aes(x = as.factor(yr),stratum=lc,alluvium=i,
#				 weight = freq,
				 fill = lc, label = lc)) + 
geom_flow(alpha = 0.4) + 
geom_stratum(alpha = 0.8) + 
geom_text(stat="stratum",size=3) + 
scale_fill_manual("",
									labels = c("Barren", "Vegetated", "Water", "Wetland"),
									values = c("#CCCCCC", "#00CC00", "#0000CC", "#CC00CC")) + 
theme_bw() + 
ylab("Pixels") + xlab("Year") + ggtitle("Change in land cover, Old Crow Flats")

 
ggsave("../../plots/analyses/oldCrowLakeDrain.png", gglake, height=6, width = 6, units = 'in')


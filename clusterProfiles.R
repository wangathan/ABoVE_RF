#############################
#
#   A script to take tcCluster and characterize some things about them. 
#     - Vegetation index and band seasonal profiles
#     - Min/Max/Mean values across bands
#     - Climate info
#

library(ggplot2)
library(data.table)
library(randomForest)
#library(caret)


rfid = commandArgs(TRUE)[1]
rfid = "tc_20180416_noGeo_k55"

clustdt = get(load(paste0('../../data/rf/clusters/',rfid,'_dt')))

# plot groups
bcolor = c("red", "green", "blue")
binfra = c("nir", "swir1", "swir2")
btassl = c("tcb", "tcg", "tcw")
bcoord = c("rcc", "gcc", "bcc")
bnd    = c("ndvi", "ndsi")
bnd2   = c("nbr", "evi")
bother = c("nbrevi", "tcwgd", "bt")

plotgroups = c("bcolor", "binfra", "btassl", "bcoord", "bnd", "bnd2", "bother")

slandsat = c("blue", "green", "red", "nir", "swir1", "swir2", "bt")
sindices = c("ndvi", "evi", "nbr", "tcb", "tcg", "tcw")
sother = c("bcc", "gcc", "rcc", "nbrevi", "tcwgd")
btypes = c("min","max","mn","amp","trnd")
spgroups = c("slandsat", "sindices", "sother")

for( pg in plotgroups ){

  print(pg)
  bndnames = get(pg)

  # The Monthly Vluaes
  bnds_m = expand.grid(bndnames, paste0("m",1:7))
  bnds = paste0(bnds_m$Var2,"_",bnds_m$Var1)

  for(c1 in seq(1,55,5)){
    c2 = c1 + 4
  # reorganize for ggplot  
  pgdt = clustdt[tcCluster %in% c1:c2, c("tcCluster", bnds), with=F]
  pgmelt = melt(pgdt, id.vars="tcCluster")
  pgmelt[,mo:=unlist(tstrsplit(variable,"",keep=2L))]
  pgmelt[,bnd:=unlist(tstrsplit(variable,"_",keep=2L))]

  pgplot = ggplot(data = pgmelt, aes(x = as.factor(mo), 
                                     fill = bnd, 
                                    color = bnd, 
                                     y = value)) + 
  geom_boxplot(outlier.size=0) +
  facet_grid(tcCluster~.) + 
  scale_fill_manual("",
#                     labels = c(bndnames[1],bndnames[2],bndnames[3]),
                     values = c("red","darkgreen","blue")) + 
  scale_color_manual("",
 #                    labels = c(bndnames[1],bndnames[2],bndnames[3]),
                     values = c("red","darkgreen","blue")) +
  scale_x_discrete("",
                   breaks = 1:7, labels = c("Winter (5%)",
                                            "Spring (20%)", 
                                            "Early Sum (35%)", 
                                            "Peak (50%)", 
                                            "Late Sum (65%)",
                                            "Fall (80%)",
                                            "Winter (95%)")) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle=-20,vjust=1,hjust=0),
        panel.grid.major = element_line(color = "#808080")) + 
  ylab("") + ggtitle(paste0("Seasonal Values: ", pg))
  

  ylim1 = boxplot.stats(pgmelt$value)$stats[c(1,5)]
  #pgplot_lim = pgplot + coord_cartesian(ylim = c(ylim1[1],ylim1[2]*1.15))
  pgplot_lim = pgplot + ylim(-0.5,1)

  fout = paste0("../../plots/clusters_",rfid,"_pam_rf/profiles/monthly_",pg,"_",c1,"_prof.png")
  ggsave(fout,pgplot_lim,height=5,width=6,units='in')
  }

  # The Differentials
  bnds_m = expand.grid(bndnames, paste0("d",1:6))
  bnds = paste0(bnds_m$Var2,"_",bnds_m$Var1)

  for(c1 in seq(1,55,5)){
    c2 = c1 + 4
  # reorganize for ggplot  
  pgdt = clustdt[tcCluster %in% c1:c2, c("tcCluster", bnds), with=F]
  pgmelt = melt(pgdt, id.vars="tcCluster")
  pgmelt[,mo:=unlist(tstrsplit(variable,"",keep=2L))]
  pgmelt[,bnd:=unlist(tstrsplit(variable,"_",keep=2L))]

  pgplot = ggplot(data = pgmelt, aes(x = as.factor(mo), 
                                     fill = bnd, 
                                    color = bnd, 
                                     y = value)) + 
  geom_boxplot(outlier.size=0) +
  geom_hline(aes(yintercept=0)) + 
  facet_grid(tcCluster~.) + 
  scale_fill_manual("",
#                     labels = c(bndnames[1],bndnames[2],bndnames[3]),
                     values = c("red","darkgreen","blue")) + 
  scale_color_manual("",
 #                    labels = c(bndnames[1],bndnames[2],bndnames[3]),
                     values = c("red","darkgreen","blue")) +
  scale_x_discrete("",
                   breaks = 1:6, labels = c("Winter-Spring",
                                            "Spring-Summer", 
                                            "Summer-Peak", 
                                            "Peak-Summer", 
                                            "Summer-Fall",
                                            "Fall-Winter")) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle=-20,vjust=1,hjust=0),
        panel.grid.major = element_line(color = "#808080")) + 
  ylab("") + ggtitle(paste0("Seasonal Differentials: ",pg))
  

  ylim1 = boxplot.stats(pgmelt$value)$stats[c(1,5)]
  #pgplot_lim = pgplot + coord_cartesian(ylim = c(ylim1[1],ylim1[2]*1.15))
  pgplot_lim = pgplot + ylim(-0.01,0.01)

  fout = paste0("../../plots/clusters_",rfid,"_pam_rf/profiles/diff_",pg,"_",c1,"_prof.png")
  ggsave(fout,pgplot_lim,height=5,width=6,units='in')
  }
}
for( spg in spgroups ){

  print(spg)
  bndnames = get(spg)

  # The Annual Vluaes
  bnds_m = expand.grid(bndnames, btypes)
  bnds = paste0(bnds_m$Var2,"_",bnds_m$Var1)

  for(c1 in seq(1,55,5)){
    c2 = c1 + 4
  # reorganize for ggplot  
  spgdt = clustdt[tcCluster %in% c1:c2, c("tcCluster", bnds), with=F]
  spgmelt = melt(spgdt, id.vars="tcCluster")
  spgmelt[,type:=unlist(tstrsplit(variable,"_",keep=1L))]
  spgmelt[,bnd:=unlist(tstrsplit(variable,"_",keep=2L))]

  spgplot = ggplot(data = spgmelt, aes(x = type, 
                                     fill = type, 
                                    color = type, 
                                     y = value)) + 
  geom_boxplot(outlier.size=0) +
  facet_grid(tcCluster~as.factor(bnd)) + 
  scale_fill_manual("",
  #                   labels = c(btypes[1],btypes[2],btypes[3],btypes[4],btypes[5]),
                     values = c("red","darkgreen","blue","black","orange")) + 
  scale_color_manual("",
   #                  labels = c(btypes[1],btypes[2],btypes[3],btypes[4],btypes[5]),
                     values = c("red","darkgreen","blue","black","orange")) + 
  scale_x_discrete("",
                   breaks = 1:length(bndnames), labels = bndnames)+
  theme_bw()+
  theme(panel.grid.major = element_line(color="#808080")) +
  #theme(axis.text.x = element_text(angle=-20,vjust=1,hjust=0)) + 
  ylab("") + ggtitle(paste0("Annual Values: ", spg))
  

  ylim1 = boxplot.stats(spgmelt$value)$stats[c(1,5)]
  #spgplot_lim = spgplot + coord_cartesian(ylim = ylim1*1.15)
  spgplot_lim = spgplot + ylim(-0.5,1)

  fout = paste0("../../plots/clusters_",rfid,"_pam_rf/profiles/annual_",spg,"_",c1,"_prof.png")
  ggsave(fout,spgplot_lim,height=5,width=6,units='in')
  }
}



################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################

# repeat with remap
if(rfid == "tc_20180416_noGeo_k55"){
  clustdt[tcCluster %in% c(1,15,16,31), newlc:= 1] #Everg F                           
  clustdt[tcCluster %in% c(25,34), newlc:= 2] #Decid F                           
  clustdt[tcCluster %in% c(20,30), newlc:= 3] #Mixed F                           
  #clustdt[tcCluster %in% c(15), newlc:= 4] #Everg W                           
  #clustdt[tcCluster %in% c(22), newlc:= 5] #Decid W                           
  #clustdt[tcCluster %in% c(44), newlc:= 6] #Mixed W                          # definitely woodland. just "woodland"? 
  clustdt[tcCluster %in% c(3,4,44, 49), newlc:= 4] #Bog Forest                          
  # 22, 4 appears in suburbs too

  clustdt[tcCluster %in% c(27,39,53), newlc:= 5] #Low Shrub                          
  clustdt[tcCluster %in% c(19,23,50), newlc:= 6] #High Shrub                          
  clustdt[tcCluster %in% c(7,18,21,22,40,41,51,54), newlc:= 7] #Sparse Shrub                          
  clustdt[tcCluster %in% c(14), newlc:= 8] #Grass                          
  clustdt[tcCluster %in% c(42), newlc:=9] #Tussock Tundra
  clustdt[tcCluster %in% c(8,26,28,36,37,48), newlc:= 10] #Sparse Grass/Lichen                

  clustdt[tcCluster %in% c(2,10,11,35,46), newlc:= 11] #Fen                            
  clustdt[tcCluster %in% c(32,52), newlc:= 12] #Bog                               
  clustdt[tcCluster %in% c(12), newlc:= 13] #Shallows                            

  clustdt[tcCluster %in% c(9,13,17,29,43,45,55), newlc:= 14] #Barren                        
  clustdt[tcCluster %in% c(5,6,24,33,38,47), newlc:= 15] #Water                             
  for( pg in plotgroups ){

    print(pg)
    bndnames = get(pg)

    # The Monthly Vluaes
    bnds_m = expand.grid(bndnames, paste0("m",1:7))
    bnds = paste0(bnds_m$Var2,"_",bnds_m$Var1)

    for(c1 in seq(1,15,5)){
      c2 = c1 + 4
      # reorganize for ggplot  
      pgdt = clustdt[newlc %in% c1:c2, c("newlc", bnds), with=F]
      pgmelt = melt(pgdt, id.vars="newlc")
      pgmelt[,mo:=unlist(tstrsplit(variable,"",keep=2L))]
      pgmelt[,bnd:=unlist(tstrsplit(variable,"_",keep=2L))]

      pgplot = ggplot(data = pgmelt, aes(x = as.factor(mo), 
                                         fill = bnd, 
                                         color = bnd, 
                                         y = value)) + 
geom_boxplot(outlier.size=0) +
facet_grid(newlc~.) + 
scale_fill_manual("",
                  #labels = c(bndnames[1],bndnames[2],bndnames[3]),
                  values = c("red","darkgreen","blue")) + 
scale_color_manual("",
                  # labels = c(bndnames[1],bndnames[2],bndnames[3]),
                   values = c("red","darkgreen","blue")) +
scale_x_discrete("",
                 breaks = 1:7, labels = c("Winter (5%)",
                                          "Spring (20%)", 
                                          "Early Sum (35%)", 
                                          "Peak (50%)", 
                                          "Late Sum (65%)",
                                          "Fall (80%)",
                                          "Winter (95%)")) + 
theme_bw()+
theme(axis.text.x = element_text(angle=-20,vjust=1,hjust=0),
      panel.grid.major = element_line(color="#808080")) + 
ylab("") + ggtitle(paste0("Seasonal Values: ", pg))


  ylim1 = boxplot.stats(pgmelt$value)$stats[c(1,5)]
#pgplot_lim = pgplot + coord_cartesian(ylim = ylim1*1.25)
pgplot_lim = pgplot + ylim(-0.5,1)

fout = paste0("../../plots/clusters_",rfid,"_pam_rf/profiles_remap/monthly_",pg,"_",c1,"_prof.png")
ggsave(fout,pgplot_lim,height=5,width=6,units='in')
    }

    # The Differentials
    bnds_m = expand.grid(bndnames, paste0("d",1:6))
    bnds = paste0(bnds_m$Var2,"_",bnds_m$Var1)

    for(c1 in seq(1,15,5)){
      c2 = c1 + 4
      # reorganize for ggplot  
      pgdt = clustdt[newlc %in% c1:c2, c("newlc", bnds), with=F]
      pgmelt = melt(pgdt, id.vars="newlc")
      pgmelt[,mo:=unlist(tstrsplit(variable,"",keep=2L))]
      pgmelt[,bnd:=unlist(tstrsplit(variable,"_",keep=2L))]

      pgplot = ggplot(data = pgmelt, aes(x = as.factor(mo), 
                                         fill = bnd, 
                                         color =bnd, 
                                         y = value)) + 
geom_boxplot(outlier.size=0) +
geom_hline(aes(yintercept=0)) + 
facet_grid(newlc~.) + 
scale_fill_manual("",
                  #labels = c(bndnames[1],bndnames[2],bndnames[3]),
                  values = c("red","darkgreen","blue")) + 
scale_color_manual("",
                   #labels = c(bndnames[1],bndnames[2],bndnames[3]),
                   values = c("red","darkgreen","blue")) +
scale_x_discrete("",
                 breaks = 1:6, labels = c("Winter-Spring",
                                          "Spring-Summer", 
                                          "Summer-Peak", 
                                          "Peak-Summer", 
                                          "Summer-Fall",
                                          "Fall-Winter")) + 
theme_bw()+
theme(axis.text.x = element_text(angle=-20,vjust=1,hjust=0),
      panel.grid.major = element_line(color="#808080")) + 
ylab("") + ggtitle(paste0("Seasonal Differentials: ",pg))


  ylim1 = boxplot.stats(pgmelt$value)$stats[c(1,5)]
#  pgplot_lim = pgplot + coord_cartesian(ylim = ylim1*1.15)
pgplot_lim = pgplot + ylim(-0.01,0.01)

fout = paste0("../../plots/clusters_",rfid,"_pam_rf/profiles_remap/diff_",pg,"_",c1,"_prof.png")
ggsave(fout,pgplot_lim,height=5,width=6,units='in')
    }
  }

  for( spg in spgroups ){

    print(spg)
    bndnames = get(spg)

    # The Annual Vluaes
    bnds_m = expand.grid(bndnames, btypes)
    bnds = paste0(bnds_m$Var2,"_",bnds_m$Var1)

    for(c1 in seq(1,15,5)){
      c2 = c1 + 4
      # reorganize for ggplot  
      spgdt = clustdt[newlc %in% c1:c2, c("newlc", bnds), with=F]
      spgmelt = melt(spgdt, id.vars="newlc")
      spgmelt[,type:=unlist(tstrsplit(variable,"_",keep=1L))]
      spgmelt[,bnd:=unlist(tstrsplit(variable,"_",keep=2L))]

      spgplot = ggplot(data = spgmelt, aes(x = type, 
                                           fill = type, 
                                           color = type, 
                                           y = value)) + 
geom_boxplot(outlier.size=0) +
facet_grid(newlc~as.factor(bnd)) + 
scale_fill_manual("",
#                  labels = c(btypes[1],btypes[2],btypes[3],btypes[4],btypes[5]),
                  values = c("red","darkgreen","blue","black","orange")) + 
scale_color_manual("",
 #                  labels = c(btypes[1],btypes[2],btypes[3],btypes[4],btypes[5]),
                   values = c("red","darkgreen","blue","black","orange")) + 
scale_x_discrete("",
                 breaks = 1:length(bndnames), labels = bndnames)+
theme_bw()+
theme(panel.grid.major=element_line(color="#808080")) + 
#theme(axis.text.x = element_text(angle=-20,vjust=1,hjust=0)) + 
ylab("") + ggtitle(paste0("Annual Values: ", spg))


#  ylim1 = boxplot.stats(spgmelt$value)$stats[c(1,5)]
spgplot_lim = spgplot + ylim(-0.5,1)
#spgplot_lim = spgplot + coord_cartesian(ylim = c(ylim1[1],ylim1[2]*1.25))

fout = paste0("../../plots/clusters_",rfid,"_pam_rf/profiles_remap/annual_",spg,"_",c1,"_prof.png")
ggsave(fout,spgplot_lim,height=5,width=6,units='in')
    }
  }
}
lcmap_col = c("#003300", #evergF
            "#00DD00", #decidF
            "#008800", #mixedF

            #"#336633", #evergW
            #"#66DD66", #decidW
            #"#559955", #mixedW
            "#8AC181", #bogW

            "#845A06", #low shrub
            "#AD3714", #high shrub
            "#AFA377", #sparse shrub
            "#EAC856", #grass
            "#BAA22A", #tussock
            "#DFE5A2", #sparse veg low

            "#4DC183", #Fen
            "#D18523", #Bog
            "#75ACFF", #Shallows

            "#666666", #barren
            "#3333FF") #water

lcmap_leg = c("Everg F",
            "Decid F",
            "Mixed F",

            #"Everg W",
            #"Decid W",
            #"Mixed W",
            "Woodland",

            "Low Shrub",
            "Shrubland",
            "Open Shrubland",
            "Herbaceous",
            "Tussock Tundra",
            "Sparsely Vegetated", 

            "Fen",
            "Bog",
            "Shallows",

            "Barren",
            "Water")

#ndvibox = ggplot(data = clustdt, aes(x = tcCluster) + 
d3evibox = ggplot(data = clustdt, aes(x = as.factor(tcCluster), y = d3_evi)) + geom_boxplot() + 
  #scale_fill_manual(values = lcmap_col, guide = F) + 
  #scale_x_discrete(labels = lcmap_leg) + 
  theme_bw() + 
  ylim(-0.01, 0.01)+
  #theme(axis.text.x = element_text(angle = -20, hjust = 0, vjust = 1)) + 
  ggtitle("D3_EVI by land cover cluster") + xlab("") + ylab("Summer-Peak EVI Change")
ndvibox = ggplot(data = clustdt, aes(x = as.factor(tcCluster), y = max_ndvi)) + geom_boxplot() + 
  #scale_fill_manual(values = lcmap_col, guide = F) + 
  #scale_x_discrete(labels = lcmap_leg) + 
  theme_bw() + 
  #theme(axis.text.x = element_text(angle = -20, hjust = 0, vjust = 1)) + 
  ggtitle("Max NDVI by land cover cluster") + xlab("") + ylab("Max NDVI")
d2nbrboxrm= ggplot(data = clustdt, aes(x = as.factor(newlc), y = d2_nbr, fill = as.factor(newlc))) + geom_boxplot() + 
  scale_fill_manual(values = lcmap_col, guide = F) + 
  scale_x_discrete(labels = lcmap_leg) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -20, hjust = 0, vjust = 1)) + 
  ggtitle("Spring -> Summer change in NBR") + xlab("") + ylab("D2 NBR") + ylim(-0.005,0.015)
d2eviboxrm= ggplot(data = clustdt, aes(x = as.factor(newlc), y = d2_evi, fill = as.factor(newlc))) + geom_boxplot() + 
  scale_fill_manual(values = lcmap_col, guide = F) + 
  scale_x_discrete(labels = lcmap_leg) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -20, hjust = 0, vjust = 1)) + 
  ggtitle("Spring -> Summer change in EVI") + xlab("") + ylab("D2 EVI") + ylim(-0.002,0.01)
ndviboxrm = ggplot(data = clustdt, aes(x = as.factor(newlc), y = max_ndvi, fill = as.factor(newlc))) + geom_boxplot() + 
  scale_fill_manual(values = lcmap_col, guide = F) + 
  scale_x_discrete(labels = lcmap_leg) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -20, hjust = 0, vjust = 1)) + 
  ggtitle("Max NDVI by land cover class") + xlab("") + ylab("Max NDVI")

ggsave("../../plots/clusters_tc_20180416_noGeo_k55_pam_rf/d2nbr_boxplots_clusters.png",
       d2nbrboxrm)
ggsave("../../plots/clusters_tc_20180416_noGeo_k55_pam_rf/d2evi_boxplots_clusters.png",
       d2eviboxrm)
ggsave("../../plots/clusters_tc_20180416_noGeo_k55_pam_rf/d3evi_boxplots_clusters.png",
       d3evibox)
ggsave("../../plots/clusters_tc_20180416_noGeo_k55_pam_rf/maxndvi_boxplots_clusters.png",
       ndvibox)
ggsave("../../plots/clusters_tc_20180416_noGeo_k55_pam_rf/maxndvi_boxplots_rmclasses.png",
       ndviboxrm)

#####
#BIOCLIM
####

    bcvars = c("annMeanTemp", "annPcp","mnDiurnalRange",
               "warmMax", "wrmPcp", "wrmTemp",
               "coldMin", "cldPcp", "cldTemp",
               "wetPcp", "wetTemp","dryPcp", "dryTemp",
               "pcpSeasonality", "tempSeasonality", "isothermality", "annRange",
               "PZI")

  bcdt = clustdt[,c("tcCluster","newlc",bcvars),with=F]
  # to get data on similar sclaes
  bcdt[, annPcp := annPcp/10]
  bcdt[,tempSeasonality := tempSeasonality/100]
  bcmelt = melt(bcdt, id.vars = c("tcCluster", "newlc"))
  for(c1 in seq(1,55,5)){
    c2 = c1 + 4
  bcplot = ggplot(data = bcmelt[tcCluster %in% c1:c2,], aes(x = variable, y = value)) + geom_boxplot() +
    theme(axis.text.x = element_text(angle=-20,vjust=1,hjust=0)) + xlab("") + ylim(-50, 300) + 
    facet_grid(tcCluster~.)
  ggsave(paste0("../../plots/clusters_tc_20180416_noGeo_k55_pam_rf/profiles/bioclim_",c1,"_prof.png"),
         bcplot)#,
         #width=6, height=5, units='in')
  }

  for(c1 in seq(1,15,5)){
    c2 = c1 + 4
  bcplot = ggplot(data = bcmelt[newlc %in% c1:c2,], aes(x = variable, y = value)) + geom_boxplot() +
    theme(axis.text.x = element_text(angle=-20,vjust=1,hjust=0)) + xlab("") + ylim(-50, 300) + 
    facet_grid(as.factor(newlc)~.)
  ggsave(paste0("../../plots/clusters_tc_20180416_noGeo_k55_pam_rf/profiles_remap/bioclim_",c1,"_prof.png"),
         bcplot,
         width=6, height=5, units='in')
  }






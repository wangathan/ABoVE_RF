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
library(caret)


rfid = commandArgs(TRUE)[1]
rfid = "tc_20180416_noGeo_k55"

clustdt = get(load(paste0('../../data/rf/clusters/',rfid,'_dt')))


# plot groups
bcolor = c("red", "green", "blue")
binfra = c("nir", "swir1", "swir2")
btassl = c("tcb", "tcg", "tcw")
bcoord = c("rcc", "gcc", "bcc")
bnd    = c("", "ndvi", "ndsi")
bnd2   = c("nbr", "evi", "")
bother = c("nbrevi", "tcwgd", "bt")

plotgroups = c("bcolor", "binfra", "btassl", "bcoord", "bnd", "bnd2", "bother")
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
                                     fill = as.factor(bnd), 
   #                                 color = as.factor(bnd), 
                                     y = value)) + 
  geom_boxplot(outlier.size=0) +
  facet_grid(tcCluster~.) + 
  scale_fill_manual("",
                     labels = c(bndnames[1],bndnames[2],bndnames[3]),
                     values = c("red","darkgreen","blue")) + 
  #scale_color_manual("",
  #                   labels = c(bndnames[1],bndnames[2],bndnames[3]),
  #                   values = c("red","darkgreen","blue")) +
  scale_x_discrete("",
                   breaks = 1:7, labels = c("Winter (5%)",
                                            "Spring (20%)", 
                                            "Early Sum (35%)", 
                                            "Peak (50%)", 
                                            "Late Sum (65%)",
                                            "Fall (80%)",
                                            "Winter (95%)")) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle=-20,vjust=1,hjust=0)) + 
  ylab("") + ggtitle(paste0("Seasonal Values: ", pg))
  

  ylim1 = boxplot.stats(pgmelt$value)$stats[c(1,5)]
  pgplot_lim = pgplot + coord_cartesian(ylim = ylim1*1.15)

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
                                     fill = as.factor(bnd), 
   #                                 color = as.factor(bnd), 
                                     y = value)) + 
  geom_boxplot(outlier.size=0) +
  geom_hline(aes(yintercept=0)) + 
  facet_grid(tcCluster~.) + 
  scale_fill_manual("",
                     labels = c(bndnames[1],bndnames[2],bndnames[3]),
                     values = c("red","darkgreen","blue")) + 
  #scale_color_manual("",
  #                   labels = c(bndnames[1],bndnames[2],bndnames[3]),
  #                   values = c("red","darkgreen","blue")) +
  scale_x_discrete("",
                   breaks = 1:6, labels = c("Winter-Spring",
                                            "Spring-Summer", 
                                            "Summer-Peak", 
                                            "Peak-Summer", 
                                            "Summer-Fall",
                                            "Fall-Winter")) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle=-20,vjust=1,hjust=0)) + 
  ylab("") + ggtitle(paste0("Seasonal Differentials: ",pg))
  

  ylim1 = boxplot.stats(pgmelt$value)$stats[c(1,5)]
  pgplot_lim = pgplot + coord_cartesian(ylim = ylim1*1.15)

  fout = paste0("../../plots/clusters_",rfid,"_pam_rf/profiles/diff_",pg,"_",c1,"_prof.png")
  ggsave(fout,pgplot_lim,height=5,width=6,units='in')
  }
}

slandsat = c("blue", "green", "red", "nir", "swir1", "swir2", "bt")
sindices = c("ndvi", "ndsi", "evi", "nbr", "tcb", "tcg", "tcw")
sother = c("bcc", "gcc", "rcc", "nbrevi", "tcwgd")
btypes = c("min","max","mn","amp","trnd")
spgroups = c("slandsat", "sindices", "sother")

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

  spgplot = ggplot(data = spgmelt, aes(x = as.factor(type), 
                                     fill = as.factor(type), 
                                    color = as.factor(type), 
                                     y = value)) + 
  geom_boxplot(outlier.size=0) +
  facet_grid(tcCluster~as.factor(bnd)) + 
  scale_fill_manual("",
                     labels = c(btypes[1],btypes[2],btypes[3],btypes[4],btypes[5]),
                     values = c("red","darkgreen","blue","black","orange")) + 
  scale_color_manual("",
                     labels = c(btypes[1],btypes[2],btypes[3],btypes[4],btypes[5]),
                     values = c("red","darkgreen","blue","black","orange")) + 
#  scale_color_manual("",
 #                   labels = c(bndnames[1],bndnames[2],bndnames[3]),
  #                   values = c("red","darkgreen","blue")) +
  scale_x_discrete("",
                   breaks = 1:length(bndnames), labels = bndnames)+
  theme_bw()+
  #theme(axis.text.x = element_text(angle=-20,vjust=1,hjust=0)) + 
  ylab("") + ggtitle(paste0("Annual Values: ", spg))
  

  ylim1 = boxplot.stats(spgmelt$value)$stats[c(1,5)]
  spgplot_lim = spgplot + coord_cartesian(ylim = ylim1*1.15)

  fout = paste0("../../plots/clusters_",rfid,"_pam_rf/profiles/annual_",spg,"_",c1,"_prof.png")
  ggsave(fout,spgplot_lim,height=5,width=6,units='in')
  }
}
  # profile elevation - get mean and standard errors of each, over 5
clustdt[,sdElv := sd(elv, na.rm=T), by=tcCluster]
clustdt[,sdAsp := sd(asp, na.rm=T), by=tcCluster]
clustdt[,sdSlp := sd(slp, na.rm=T), by=tcCluster]

clustdt[,mnElv := mean(elv, na.rm=T), by=tcCluster]
clustdt[,mnAsp := mean(asp, na.rm=T), by=tcCluster]
clustdt[,mnSlp := mean(slp, na.rm=T), by=tcCluster]

# plot 
elevdt = unique(clustdt[,.(tcCluster,mnElv,mnAsp,mnSlp,sdElv,sdAsp,sdSlp)])
elevmelt = melt(elevdt,id.vars="tcCluster")

elevplot = ggplot(data=elevmelt[variable %like% "mn",], aes(x = as.factor(tcCluster), y = value, fill = variable)) + geom_bar(stat="identity")

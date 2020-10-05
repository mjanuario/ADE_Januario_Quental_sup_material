#rm(list=ls())
library(cowplot)
library(patchwork)
library(fitdistrplus)
source("~/Desktop/PyRateTools/R/pyrate_tools_pre_package.R")

###especies level
spdt=read.table("~/Downloads/ADE_revised_general/sp.tsv", sep="\t", header=T) #reading data

gndt=read.table("~/Downloads/ADE_revised_general/gen.tsv", sep="\t", header=T) #reading data


###########################################
#ploting and identifying issues:
pcs=c(0,1,2,15,16,17)
cols=c("saddlebrown", "purple", "springgreen4","magenta", "gold1", "darkorange1")
dataset_names=c("S1MA", "S1NW", "S2MA", "G1MA", "G1NW", "G2MA")
windowboundsmax=c(max(spdt$MaxT), 39.5, 12.5, max(spdt$MaxT), max(spdt$MaxT), 9) *-1#80 represetns Inf
windowboundsmin=c(14, 16, 6.5, 14.5, 21.5, 2)*-1

spdt$MinT=spdt$MinT*-1
spdt$MaxT=spdt$MaxT*-1

gndt$MinT=gndt$MinT*-1
gndt$MaxT=gndt$MaxT*-1
###########################################

s1ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_sp_Inf_14/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

s1nw=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_sp_395_16/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

s2ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_sp_125_65/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

g1ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_gen_Inf_145/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

g1nw=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_gen_Inf_215/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

g2ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_gen_9_2/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

datasets=list(s1ma$mcmc, s1nw$mcmc, s2ma$mcmc, g1ma$mcmc, g1nw$mcmc, g2ma$mcmc)


#############################
# sup fig 4 - 9
#############################



for(dt in 1:3){
  
  spdt$included="None"
  
  aux=get.durations(datasets[[dt]], na.rm = F)
  not_all_windows=aux$lineage[is.na(aux$duration)]
  all_windows=aux$lineage[!(is.na(aux$duration))]
  
  spdt$included[spdt$Species %in% not_all_windows]="Some"
  spdt$included[spdt$Species %in% all_windows]="All"
  
  
  spdt=spdt[order((spdt$MaxT), decreasing = F),]
  spdt$id=1:nrow(spdt) #aux variable
  
  if(dt %in% c(3,6)){
    xop=20
  }else{
    xop=7.5  
  }
  
  xmax=windowboundsmax[dt]
  xmin=windowboundsmin[dt]
  
  subplot=ggplot(spdt[spdt$included %in% c("All", "Some"),])+
    geom_segment(aes(x=MinT, xend=MaxT, y=1:nrow(spdt[spdt$included %in% c("All", "Some"),]), yend=1:nrow(spdt[spdt$included %in% c("All", "Some"),]), col=included))+
    ggtitle("zoomed subplot")+
    scale_color_manual(values = c("black", "red"))+
    xlab("Time (Mya)")+
    ylab("Fossil occurrences") + 
    scale_y_continuous()+
    scale_x_continuous(breaks = c(-40, -30, -20, -10,0), labels = c(40, 30,20,10,0))+
    guides(color=guide_legend(title="Adequate resolution"))+
    #scale_x_reverse()+
    theme(legend.position="none")
  
  
  ggplot(spdt)+
    geom_segment(aes(x=MinT, xend=MaxT, y=id, yend=id, col=included))+
    scale_color_manual(values = c("black", "grey75", "red"))+
    xlab("Time (Mya)")+
    ylab("Fossil occurrences") + 
    scale_y_continuous()+
    guides(color=guide_legend(title="Included in replicates"))+
    theme(legend.position="bottom")+
    #scale_x_reverse()+
    scale_x_continuous(breaks = c(-40, -30, -20, -10,0), labels = c(40, 30,20,10,0))+
    geom_rect(data=data.frame(
      xmin=c(windowboundsmin[dt]),
      xmax=c(windowboundsmax[dt]),
      ymin=.9*nrow(spdt),
      ymax=nrow(spdt)),
      mapping=aes(xmin=xmin, xmax=xmax, ymin= ymin, ymax=ymax), fill=cols[dt])+
    annotate("text", x=((xmax-xmin)/2)+xmin, y = .95*nrow(spdt), size=8, label=dataset_names[dt], col="white")+
    annotation_custom(ggplotGrob(subplot), xmin = -45, xmax = -20, ymin = 1000, ymax = 6500)
  
  ggsave(filename = paste0("~/Desktop/", dataset_names[dt],"_occs_used.png"), width = 15, units = "in", height = 10.3)
  
}


for(dt in 4:6){
  
  gndt$included="None"
  
  aux=get.durations(datasets[[dt]], na.rm = F)
  not_all_windows=aux$lineage[is.na(aux$duration)]
  all_windows=aux$lineage[!(is.na(aux$duration))]
  
  gndt$included[gndt$Species %in% not_all_windows]="Some"
  gndt$included[gndt$Species %in% all_windows]="All"
  
  
  gndt=gndt[order((gndt$MaxT), decreasing = F),]
  gndt$id=1:nrow(gndt) #aux variable
  
  if(dt %in% c(3,6)){
    xop=20
  }else{
    xop=7.5  
  }
  
  xmax=windowboundsmax[dt]
  xmin=windowboundsmin[dt]
  
  subplot=ggplot(gndt[gndt$included %in% c("All", "Some"),])+
    geom_segment(aes(x=MinT, xend=MaxT, y=1:nrow(gndt[gndt$included %in% c("All", "Some"),]), yend=1:nrow(gndt[gndt$included %in% c("All", "Some"),]), col=included))+
    ggtitle("zoomed subplot")+
    scale_color_manual(values = c("black", "red"))+
    xlab("Time (Mya)")+
    ylab("Fossil occurrences") + 
    scale_y_continuous()+
    scale_x_continuous(breaks = c(-40, -30, -20, -10,0), labels = c(40, 30,20,10,0))+
    guides(color=guide_legend(title="Adequate resolution"))+
    #scale_x_reverse()+
    theme(legend.position="none")
  
  
  ggplot(gndt)+
    geom_segment(aes(x=MinT, xend=MaxT, y=id, yend=id, col=included))+
    scale_color_manual(values = c("black", "grey75", "red"))+
    xlab("Time (Mya)")+
    ylab("Fossil occurrences") + 
    scale_y_continuous()+
    guides(color=guide_legend(title="Included in replicates"))+
    theme(legend.position="bottom")+
    #scale_x_reverse()+
    scale_x_continuous(breaks = c(-40, -30, -20, -10,0), labels = c(40, 30,20,10,0))+
    geom_rect(data=data.frame(
      xmin=c(windowboundsmin[dt]),
      xmax=c(windowboundsmax[dt]),
      ymin=.9*nrow(gndt),
      ymax=nrow(gndt)),
      mapping=aes(xmin=xmin, xmax=xmax, ymin= ymin, ymax=ymax), fill=cols[dt])+
    annotate("text", x=((xmax-xmin)/2)+xmin, y = .95*nrow(gndt), size=8, label=dataset_names[dt], col="white")+
    annotation_custom(ggplotGrob(subplot), xmin = -45, xmax = -20, ymin = 1000, ymax = 6500)
  
  ggsave(filename = paste0("~/Desktop/", dataset_names[dt],"_occs_used.png"), width = 15, units = "in", height = 10.3)
  
}



#############################
# sup fig 10
#############################


ESS_mod=function(x){
  if(sum(is.na(x))>0){
    return(NA)
  }else{
    return(LaplacesDemon::ESS(x))
  }
}

emp_ess=data.frame()
for(i in 1:6){
  aux=aggregate(datasets[[i]][,1:30], by=list(datasets[[i]]$replica), FUN=ESS_mod)
  aux$dataset=dataset_names[i]
  head(aux)
  emp_ess=plyr::rbind.fill(emp_ess, aux)
}

#separating the role of parameters
mcmc_pars=c("posterior", "prior", "PP_lik", "BD_lik", "tot_length")
preserv_pars=c(paste0("q_", 0:10), "alpha")
clad_prig_pars="root_age"
ade_pars=c("w_shape", "w_scale_0", "mean_longevity_0")

emp_mod=melt(emp_ess, id.vars = "dataset")
emp_mod$cat=NA
emp_mod$cat[emp_mod$variable %in% mcmc_pars]="MCMC"
emp_mod$cat[emp_mod$variable %in% preserv_pars]="Preservation parameters"
emp_mod$cat[emp_mod$variable %in% clad_prig_pars]="Clade origin"
emp_mod$cat[emp_mod$variable %in% ade_pars]="ADE model"

#cleaning parameters which are not relevant:
emp_mod=emp_mod[-which(is.na(emp_mod$cat)),]

#changing parameter names:
unique(emp_mod$variable)
emp_mod$variable=as.character(emp_mod$variable)
emp_mod$variable[which(emp_mod$variable=="root_age")]="root \n age"
emp_mod$variable[which(emp_mod$variable=="w_shape")]="shape"
emp_mod$variable[which(emp_mod$variable=="w_scale_0")]="scale"
emp_mod$variable[which(emp_mod$variable=="mean_longevity_0")]="mean \n longevity"
emp_mod$variable[which(emp_mod$variable=="tot_length")]="tot \n length"
emp_mod$variable=as.factor(emp_mod$variable)
emp_mod$variable=factor(emp_mod$variable, levels=c("posterior","prior","PP_lik","BD_lik","tot \n length","q_0","q_1","q_2","q_3","alpha","root \n age","shape","scale","mean \n longevity"))

ggplot(emp_mod[emp_mod$dataset %in% c("S1MA","S1NW","S2MA"),], aes(x=variable, col=cat, y=value))+
  geom_boxplot()+
  facet_wrap(~ dataset, ncol=1)+
  scale_color_manual(values=c("#AD723A", "#749F35", "#25606B", "#92315D"))+
  xlab("PyRate Parameter")+
  theme(legend.position="bottom")+
  ylab("Effective sample size")+
  geom_abline(slope = 0, intercept = 200, linetype=2, alpha=.5, col="red", size=1.5)+
  labs(col = "Parameter Role")

ggsave(filename = "~/Desktop/ESS_sp_revised.png", width = 10, units = "in", height = 12.3)

ggplot(emp_mod[emp_mod$dataset %in% c("G1MA","G1NW","G2MA"),], aes(x=variable, col=cat, y=value))+
  geom_boxplot()+
  facet_wrap(~ dataset, ncol=1)+
  scale_color_manual(values=c("#AD723A", "#749F35", "#25606B", "#92315D"))+
  xlab("PyRate Parameter")+
  theme(legend.position="bottom")+
  ylab("Effective sample size")+
  geom_abline(slope = 0, intercept = 200, linetype=2, alpha=.5, col="red", size=1.5)+
  labs(col = "Parameter Role")

ggsave(filename = "~/Desktop/ESS_gen_revised.png", width = 10, units = "in", height = 12.3)

quantile(s2ma$mcmc$w_shape, probs = .93)

##### sup fig 17-22
emp.hpd.mod=function(x){
  x=x[!(is.na(x))]
  return(c(TeachingDemos::emp.hpd(x), median(x)))
}

for(i in 1:length(datasets)){
  dt=datasets[[i]]
  
  cols=colnames(dt)[colnames(dt) %in% c("w_shape", "replica")]
  
  hpds=aggregate(dt[,cols], by=list(dt$replica), FUN=emp.hpd.mod)
  hpds=hpds[order(hpds$w_shape[,3]),]
  hpds$order=1:nrow(hpds)
  
  png(filename = paste0("~/Desktop/sup_fig_", 16+i, ".png"), width = 2100, height = 1100)
  
  par(cex=3)
  
  plot(x=hpds$order, y=hpds$w_shape[,3], xaxt="n", xlab="Replicates", ylab="Shape value",main=dataset_names[i], ylim=c(min(hpds$w_shape), max(hpds$w_shape)), pch=16, cex=1.2)
  
  abline(h=1, lwd=2, col="red")
  segments(x0 = hpds$order, x1 = hpds$order, y0 = hpds$w_shape[,1], y1=hpds$w_shape[,2], lwd=3)
  
  dev.off()
}


##### sup fig 23
png(filename = paste0("~/Desktop/sup_fig_23.png"), width = 800, height =400 )
par(cex=12, mfrow=c(1,3))

for(i in 1:3){
  dt=datasets[[i]]
  
  cols=colnames(dt)[colnames(dt) %in% c("w_shape", "replica")]
  
  hpds=aggregate(dt[,cols], by=list(dt$replica), FUN=emp.hpd.mod)
  hpds=hpds[order(hpds$w_shape[,3]),]
  hpds$order=1:nrow(hpds)
  
  plot(x=hpds$order, y=hpds$w_shape[,3], xaxt="n", xlab="Replicates", ylab="Shape value",main=dataset_names[i], ylim=c(0.3, 1.4), pch=16, cex=1.5)
  
  abline(h=1, lwd=2, col="red")
  segments(x0 = hpds$order, x1 = hpds$order, y0 = hpds$w_shape[,1], y1=hpds$w_shape[,2], lwd=3)
  

}
dev.off()

##### sup fig 24
png(filename = paste0("~/Desktop/sup_fig_24.png"), width = 800, height =400 )
par(cex=12, mfrow=c(1,3))

for(i in 4:6){
  dt=datasets[[i]]
  
  cols=colnames(dt)[colnames(dt) %in% c("w_shape", "replica")]
  
  hpds=aggregate(dt[,cols], by=list(dt$replica), FUN=emp.hpd.mod)
  hpds=hpds[order(hpds$w_shape[,3]),]
  hpds$order=1:nrow(hpds)
  
  plot(x=hpds$order, y=hpds$w_shape[,3], xaxt="n", xlab="Replicates", ylab="Shape value",main=dataset_names[i], ylim=c(0.4, 3), pch=16, cex=1.5)
  
  abline(h=1, lwd=2, col="red")
  segments(x0 = hpds$order, x1 = hpds$order, y0 = hpds$w_shape[,1], y1=hpds$w_shape[,2], lwd=3)
  
  
}
dev.off()


#########################################
#sup fig 26

i=4
dt=datasets[[i]]
idw=which(colnames(dt) %in% "w_shape")
qs=paste0("q_", 0:10)
ids=which(colnames(dt) %in% qs)
length(ids)

png(filename = paste0("~/Desktop/",dataset_names[i], "_q_vs_shape.png"), width = 2100, height = 1100)

par(mfrow=c(2,2))
for(j in 1:length(ids)){
  LSD::heatscatter(x = dt[,ids[j]], y=dt[,idw], xlab=qs[j], ylab="Shape Parameter", main="", cex.axis=2, cex.lab=1.5)
  abline(h = 1, lty=2, lwd=3)
}
dev.off()


#########################################
#sup fig 27

i=5
dt=datasets[[i]]
idw=which(colnames(dt) %in% "w_shape")
qs=paste0("q_", 0:10)
ids=which(colnames(dt) %in% qs)
length(ids)

png(filename = paste0("~/Desktop/",dataset_names[i], "_q_vs_shape.png"), width = 2100, height = 1100)

par(mfrow=c(2,2))
for(j in 1:length(ids)){
  LSD::heatscatter(x = dt[,ids[j]], y=dt[,idw], xlab=qs[j], ylab="Shape Parameter", main="", cex.axis=2, cex.lab=1.5)
  abline(h = 1, lty=2, lwd=3)
}
dev.off()

#########################################
#sup fig 28

i=6
dt=datasets[[i]]
idw=which(colnames(dt) %in% "w_shape")
qs=paste0("q_", 0:10)
ids=which(colnames(dt) %in% qs)
length(ids)

png(filename = paste0("~/Desktop/",dataset_names[i], "_q_vs_shape.png"), width = 2100, height = 1100)

par(mfrow=c(2,2))
for(j in 1:length(ids)){
  LSD::heatscatter(x = dt[,ids[j]], y=dt[,idw], xlab=qs[j], ylab="Shape Parameter", main="", cex.axis=2, cex.lab=1.5)
  abline(h = 1, lty=2, lwd=3)
}
dev.off()


#########################################
#sup fig 25

mono=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_mono_gen_Inf_145/pyrate_mcmc_logs/", print_progress = T)
poli=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_poli_gen_Inf_145/pyrate_mcmc_logs/", print_progress = T)

mono_mod=melt(mono$mcmc[,which(colnames(mono$mcmc) %in% c(paste0("q_",0:8), "w_shape", "w_scale_0", "mean_longevity_0"))])
mono_mod$level="Monotypic"

poli_mod=melt(poli$mcmc[,which(colnames(poli$mcmc) %in% c(paste0("q_",0:8), "w_shape", "w_scale_0", "mean_longevity_0"))])
poli_mod$level="Politypic"

reps=unique(mono$mcmc$replica)
emp.hpd.mod=function(x){
  x=x[!(is.na(x))]
  return(c(TeachingDemos::emp.hpd(x), median(x)))
}

mono_hpd=apply(mono$mcmc[,c(6:9,13:15)], 2, emp.hpd.mod)
poli_hpd=apply(poli$mcmc[,c(6:9,13:15)], 2, emp.hpd.mod)
head(mono_hpd)

vars=unique(colnames(mono_hpd), colnames(poli_hpd))

png(filename = "~/Desktop/sup_fig_25.png", width = 2100, height = 1100)
par(cex=3)
plot(x=NA, y=NA, xlim=c(1,14), ylim=c(0,12.5), xaxt="n", xlab="Parameters", ylab="Parameter value", main="Mono vs Polytypic Genera")
text(x=seq(from=1.5, to=length(vars)*2, by=2),  par("usr")[3], labels = vars,pos = 1, xpd = TRUE)
abline(h=1, lwd=4, lty=2)

xx=as.numeric(seq(from=1, to=length(vars)*2, by=2))
segments(x0=xx, x1 = xx,y1=mono_hpd[1,],y0=mono_hpd[2,],col="red", lwd=3)
segments(x0=xx+1, x1 = xx+1,y1=poli_hpd[1,],y0=poli_hpd[2,],col="blue", lwd=3)
points(x=xx, y=mono_hpd[3,], pch=16, col="red", cex=1.2)
points(x=xx+1, y=poli_hpd[3,], pch=16, col="blue", cex=1.2)
dev.off()





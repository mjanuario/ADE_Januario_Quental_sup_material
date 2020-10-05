pcs=c(0,1,2,15,16,17)
cols=c("saddlebrown", "purple", "springgreen4","magenta", "gold1", "darkorange1")

library(tidyverse)
library(egg)
library(cowplot)
library(patchwork)
source("~/Desktop/PyRateTools/R/pyrate_tools_pre_package.R")

s1ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_sp_Inf_14/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

s1nw=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_sp_395_16/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

s2ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_sp_125_65/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

g1ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_gen_Inf_145/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

g1nw=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_gen_Inf_215/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

g2ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_gen_9_2/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

##########################################
##############     S1MA     ##############
##########################################

#setting dataset:
dtst=s1ma$mcmc

#calculating durations:
aux=get.durations(dtst, na.rm = F)
NAs=is.na(aux$duration)
durs=get.durations(dtst, na.rm=T)
durs$all_replicates=!(NAs)
head(durs)

#graph parameters:
bw1=.5
ids_weibull=sample(1:nrow(dtst), size = 500)
n1=nrow(durs)
max_durs=max(durs$duration)

#graph code:
p1<-ggplot()+
  
  #histogram:
  geom_histogram(data=durs, aes(fill=all_replicates, x=duration, y=(..count..)/(n1*bw1), col="dummy"),  position="stack", binwidth = bw1, origin=0) +
  
  #fixed aesthetic:
  scale_fill_manual(values = c("white", "grey75")) + 
  scale_color_manual(values = c("grey75")) + 
  theme(legend.position="none") +
  guides(fill=guide_legend(title="all occurrences within window"))+
  #xlim(c(0,max_durs))+
  ylim(c(0,.6))+
  ylab("Density")+
  xlab("Duration (My)")+
  
  #weibull:
  mapply(function(shape, scale, col) {
    geom_line(
      data=data.frame(x=seq(.1, max_durs, by=.1), y=dweibull(seq(.1, max_durs, by=.1), shape = shape, scale = scale)),
      mapping = aes(x=x, y=y), color="red", alpha=.025)
  }, 
  # parameters and colors here
  shape = dtst$w_shape[ids_weibull], 
  scale = dtst$w_scale_0[ids_weibull], 
  col = rep("red", times=length(ids_weibull))
  )+
  
  #especific from this dataset:
  #especific from this dataset:
  annotate("text", x=max(durs$duration)*.6, y=.4, size=20, label="A")+
  annotate("text", x=(max(durs$duration)*.85), y = .4, size=10, label="S1MA", col="saddlebrown")+
  ggtitle("Species, Regime 1 - Main Analysis")
  
##########################################
##############     S1NW     ##############
##########################################

#setting dataset:
dtst2=s1nw$mcmc

#calculating durations:
aux=get.durations(dtst2, na.rm = F)
NAs=is.na(aux$duration)
durs2=get.durations(dtst2, na.rm=T)
durs2$all_replicates=!(NAs)
head(durs2)

#graph parameters:
bw2=.5
ids_weibull=sample(1:nrow(dtst2), size = 500)
n2=nrow(durs2)
max_durs2=max(durs2$duration)

#graph code:
p2<-ggplot()+
  
  #histogram:
  geom_histogram(data=durs2, aes(fill=all_replicates, x=duration, y=(..count..)/(n2*bw2), col="dummy"),  position="stack", binwidth = bw2, origin=0) +
  
  #fixed aesthetic:
  scale_fill_manual(values = c("white", "grey75")) + 
  scale_color_manual(values = c("grey75")) + 
  theme(legend.position="none") +
  guides(fill=guide_legend(title="all occurrences within window"))+
  #xlim(c(0,max_durs))+
  ylim(c(0,.6))+
  ylab("Density")+
  xlab("Duration (My)")+
  
  #weibull:
  mapply(function(shape, scale, col) {
    geom_line(
      data=data.frame(x=seq(.1, max_durs2, by=.1), y=dweibull(seq(.1, max_durs2, by=.1), shape = shape, scale = scale)),
      mapping = aes(x=x, y=y), color="red", alpha=.025)
  }, 
  # parameters and colors here
  shape = dtst2$w_shape[ids_weibull], 
  scale = dtst2$w_scale_0[ids_weibull], 
  col = rep("red", times=length(ids_weibull))
  )+
  
  #especific from this dataset:
  annotate("text", x=max(durs2$duration)*.6, y=.4, size=20, label="B")+
  
  annotate("text", x=(max(durs2$duration)*.85), y = .4, size=10, label="S1NW", col="purple")+
  ggtitle("Species, Regime 1 - Narrow Window")

##########################################
##############     S2MA     ##############
##########################################

#setting dataset:
dtst3=s2ma$mcmc

#calculating durations:
aux=get.durations(dtst3, na.rm = F)
NAs=is.na(aux$duration)
durs3=get.durations(dtst3, na.rm=T)
durs3$all_replicates=!(NAs)
head(durs3)

#graph parameters:
bw3=.5
ids_weibull=sample(1:nrow(dtst3), size = 500)
n3=nrow(durs3)
max_durs3=max(durs3$duration)

#graph code:
p3<-ggplot()+
  
  #histogram:
  geom_histogram(data=durs3, aes(fill=all_replicates, x=duration, y=(..count..)/(n3*bw3), col="dummy"),  position="stack", binwidth = bw3, origin=0) +
  
  #fixed aesthetic:
  scale_fill_manual(values = c("white", "grey75")) + 
  scale_color_manual(values = c("grey75")) + 
  theme(legend.position="none") +
  guides(fill=guide_legend(title="all occurrences within window"))+
  #xlim(c(0,max_durs))+
  ylim(c(0,.6))+
  ylab("Density")+
  xlab("Duration (My)")+
  
  #weibull:
  mapply(function(shape, scale, col) {
    geom_line(
      data=data.frame(x=seq(.1, max_durs3, by=.1), y=dweibull(seq(.1, max_durs3, by=.1), shape = shape, scale = scale)),
      mapping = aes(x=x, y=y), color="red", alpha=.025)
  }, 
  # parameters and colors here
  shape = dtst3$w_shape[ids_weibull], 
  scale = dtst3$w_scale_0[ids_weibull], 
  col = rep("red", times=length(ids_weibull))
  )+
  
  #especific from this dataset:
  annotate("text", x=max(durs3$duration)*.625, y=.4, size=20, label="C")+
  
  annotate("text", x=(max(durs3$duration)*.85), y = .4, size=10, label="S2MA", col="springgreen4")+
  
  #geom_point(aes(x=(max(durs3$duration)*.85), y = .4), size=10,stroke=5,colour="springgreen4",shape=2)+
  ggtitle("Species, Regime 2 - Main Analysis")


#####################################################
#################### GENUS LEVEL ####################
#####################################################

#genus mono/polytypic
spps=read.table("~/Downloads/ADE_revised_general/sp.tsv", sep="\t", header = T)
n_spp_per_genus=table(gsub("_.*", "", unique(spps$Species))) #number of
#####################################################

##########################################
##############     G1MA     ##############
##########################################

#setting dataset:
dtst4=g1ma$mcmc

#calculating durations:
aux=get.durations(dtst4, na.rm = F)
NAs=is.na(aux$duration)
durs4=get.durations(dtst4, na.rm=T)
durs4$all_replicates=!(NAs)
head(durs4)

durs4$monotypic=TRUE
durs4$monotypic[durs4$lineage %in% names(which(n_spp_per_genus>1))]=FALSE

#graph parameters:
bw4=1
ids_weibull=sample(1:nrow(dtst4), size = 500)
n4=nrow(durs4)
max_durs4=max(durs4$duration)


#graph code:
subplot4=ggplot(data=durs4[!(durs4$monotypic),]) + 
  geom_histogram(aes(fill=monotypic, x=duration, y=(..count..)/(n4*bw4)),  position="stack", binwidth = bw4, origin=0)+
  scale_fill_manual(values = c("grey25"))+
  ylim(c(0, 0.07))+
  xlim(c(0, 23))+
  ylab("Density")+
  theme(legend.position="none") +
  ggtitle("Polytypic only")+
  xlab("Duration (My)")


p4<-ggplot()+
  
  #histogram:
  geom_histogram(data=durs4, aes(fill=monotypic, x=duration, y=(..count..)/(n4*bw4)),  position="stack", binwidth = bw4, origin=0) +
  
  #fixed aesthetic:
  scale_fill_manual(values = c("grey25", "grey75")) + 
  #scale_color_manual(values = c("black", "red")) + 
  theme(legend.position="none") +
  guides(fill=guide_legend(title="all occurrences within window"))+
  #xlim(c(-1,max_durs*1.1))+
  coord_cartesian(xlim = c(0, max_durs4*1.1))+
  ylim(c(0,.3))+
  ylab("Density")+
  xlab("Duration (My)")+
  
  #weibull:
  mapply(function(shape, scale, col) {
    geom_line(
      data=data.frame(x=seq(.1, max_durs4, by=.1), y=dweibull(seq(.1, max_durs4, by=.1), shape = shape, scale = scale)),
      mapping = aes(x=x, y=y), color="red", alpha=.025)
  }, 
  # parameters and colors here
  shape = dtst4$w_shape[ids_weibull], 
  scale = dtst4$w_scale_0[ids_weibull], 
  col = rep("red", times=length(ids_weibull))
  )+
  
  #subplot of polytypic genera:
  annotation_custom(
    ggplotGrob(subplot4), 
    xmin = max_durs4*.5, xmax = max_durs4*1.05, ymin = .1, ymax = .3
  )+
  
  #especific from this dataset:
  annotate("text", x=max_durs4*.12, y=.27, size=20, label="A")+
  
  annotate("text", x=max_durs4*.35, y = .25, size=10, label="G1MA", col="magenta")+
  
  #geom_point(aes(x=max_durs4*.4, y = .25), size=10,stroke=5,colour="magenta",shape=15)+
  ggtitle("Genus, Regime 1 - Main Analysis")

##########################################
##############     G1NW     ##############
##########################################

#setting dataset:
dtst5=g1nw$mcmc

#calculating durations:
aux=get.durations(dtst5, na.rm = F)
NAs=is.na(aux$duration)
durs5=get.durations(dtst5, na.rm=T)
durs5$all_replicates=!(NAs)
head(durs5)

durs5$monotypic=TRUE
durs5$monotypic[durs5$lineage %in% names(which(n_spp_per_genus>1))]=FALSE

#graph parameters:
bw5=1
ids_weibull=sample(1:nrow(dtst5), size = 500)
n5=nrow(durs5)
max_durs5=max(durs5$duration)

#graph code:

subplot5=ggplot(data=durs5[!(durs5$monotypic),]) + 
  geom_histogram(aes(fill=monotypic, x=duration, y=(..count..)/(n5*bw5)),  position="stack", binwidth = bw5, origin=0)+
  scale_fill_manual(values = c("grey25"))+
  ylim(c(0, 0.06))+
  xlim(c(0, 19))+
  ylab("Density")+
  theme(legend.position="none") +
  ggtitle("Polytypic only")+
  xlab("Duration (My)")


p5<-ggplot()+
  
  #histogram:
  geom_histogram(data=durs5, aes(fill=monotypic, x=duration, y=(..count..)/(n5*bw5)),  position="stack", binwidth = bw5, origin=0) +
  
  #fixed aesthetic:
  scale_fill_manual(values = c("grey25", "grey75")) + 
  #scale_color_manual(values = c("blue", "white")) + 
  theme(legend.position="none") +
  guides(fill=guide_legend(title="all occurrences within window"))+
  coord_cartesian(xlim = c(0, max_durs5*1.1))+
  #xlim(c(0,max_durs))+
  ylim(c(0,.3))+
  ylab("Density")+
  xlab("Duration (My)")+
  
  #weibull:
  mapply(function(shape, scale, col) {
    geom_line(
      data=data.frame(x=seq(.1, max_durs5, by=.1), y=dweibull(seq(.1, max_durs5, by=.1), shape = shape, scale = scale)),
      mapping = aes(x=x, y=y), color="red", alpha=.025)
  }, 
  # parameters and colors here
  shape = dtst5$w_shape[ids_weibull], 
  scale = dtst5$w_scale_0[ids_weibull], 
  col = rep("red", times=length(ids_weibull))
  )+
  
  #subplot of polytypic genera:
  annotation_custom(
    ggplotGrob(subplot5), 
    xmin = 10, xmax = 17, ymin = .1, ymax = .3
  )+
  
  #especific from this dataset:
  annotate("text", x=max_durs5*.12, y=.27, size=20, label="B")+
  annotate("text", x=max_durs5*.35, y = .25, size=10, label="G1NW", col="gold1")+
  
  ggtitle("Genus, Regime 1 - Narrow Window")

##########################################
##############     G2MA     ##############
##########################################

#setting dataset:
dtst6=g2ma$mcmc

#calculating durations:
aux=get.durations(dtst6, na.rm = F)
NAs=is.na(aux$duration)
durs6=get.durations(dtst6, na.rm=T)
durs6$all_replicates=!(NAs)
head(durs6)

durs6$monotypic=TRUE
durs6$monotypic[durs6$lineage %in% names(which(n_spp_per_genus>1))]=FALSE

#graph parameters:
bw6=1
ids_weibull=sample(1:nrow(dtst6), size = 500)
n6=nrow(durs6)
max_durs6=max(durs6$duration)

#graph code:

subplot6=ggplot(data=durs6[!(durs6$monotypic),]) + 
  geom_histogram(aes(fill=monotypic, x=duration, y=(..count..)/(n6*bw6)),  position="stack", binwidth = bw6, origin=0)+
  scale_fill_manual(values = c("grey25"))+
  ylim(c(0, 0.15))+
  xlim(c(0, 6))+
  ylab("Density")+
  theme(legend.position="none") +
  ggtitle("Polytypic only")+
  xlab("Duration (My)")

p6<-ggplot()+
  
  #histogram:
  geom_histogram(data=durs6, aes(fill=monotypic, x=duration, y=(..count..)/(n6*bw6)),  position="stack", binwidth = bw6, origin=0) +
  
  #fixed aesthetic:
  scale_fill_manual(values = c("grey25", "grey75")) + 
  #scale_color_manual(values = c("blue", "white")) + 
  theme(legend.position="none") +
  guides(fill=guide_legend(title="all occurrences within window"))+
  coord_cartesian(xlim = c(0, max_durs6*1.1))+
  #xlim(c(0,max_durs))+
  ylim(c(0,1))+
  ylab("Density")+
  xlab("Duration (My)")+
  
  #weibull:
  mapply(function(shape, scale, col) {
    geom_line(
      data=data.frame(x=seq(.1, max_durs6, by=.01), y=dweibull(seq(.1, max_durs6, by=.01), shape = shape, scale = scale)),
      mapping = aes(x=x, y=y), color="red", alpha=.025)
  }, 
  # parameters and colors here
  shape = dtst6$w_shape[ids_weibull], 
  scale = dtst6$w_scale_0[ids_weibull], 
  col = rep("red", times=length(ids_weibull))
  )+
  
  #subplot of polytypic genera:
  annotation_custom(
    ggplotGrob(subplot6), 
    xmin = 3, xmax = 5, ymin = .2, ymax = .8
  )+
  
  #especific from this dataset:
  annotate("text", x=max_durs6*.12, y=.95, size=20, label="C")+
  annotate("text", x=max_durs6*.45, y = .83, size=10, label="G2MA", col="darkorange1")+
  
  #geom_point(aes(x=max_durs6*.4, y = .83), size=10,stroke=5,colour="darkorange1",shape=17)+
  ggtitle("Genus, Regime 2 - Main Analysis")

#making panel:
(p1 ) / (p2) / (p3)

ggsave(filename = "~/Desktop/Fig_2.png", width = 10, units = "in", height = 15.3)

#making panel:
(p4 ) / (p5) / (p6)

ggsave(filename = "~/Desktop/Fig_4.png", width = 10, units = "in", height = 15.3)



########################
# ALTERNATIVE WAY TO DO THAT (not finished)
########################


dtsts=list(s1ma$mcmc, s1nw$mcmc, s2ma$mcmc, g1ma$mcmc, g1nw$mcmc, g2ma$mcmc)


makeTransparent<-function(someColor, alpha=25)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(
    red=curcoldata[1],
    green=curcoldata[2],
    blue=curcoldata[3],
    alpha=alpha, maxColorValue=255)})
}


#setting dataset:

png(filename = "~/Desktop/fig_2_alt.png", width = 880, height = 1320)
par(mfrow=c(3,1), cex=1.5)#mar=c(4,5,1,1), 

for(i in 1:3){
  dt=dtsts[[i]]
  aux=get.durations(dt, na.rm = F)
  NAs=is.na(aux$duration)
  durs=get.durations(dt, na.rm=T)
  durs$all_replicates=!(NAs)
  head(durs)
  
  hist(durs$duration, probability = T, main=dt_names[i], breaks = seq(from=0, to=30, by=.5), xlim=c(0, max(durs$duration+5)), xlab="Duration (My)")
  
  
  h1 <- hist(durs$duration[durs$all_replicates], probability = T, main=dt_names[i], breaks = seq(from=0, to=30, by=.5), xlim=c(0, max(durs$duration+5)), xlab="Duration (My)")
  h2 <- hist(durs$duration[!(durs$all_replicates)], probability = T, main=dt_names[i], breaks = seq(from=0, to=30, by=.5), xlim=c(0, max(durs$duration+5)), xlab="Duration (My)")
  
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  
  plot(h1, col = "Red", xlim = c(0, 30), xlab = "Weight", main = NULL)
  plot(h2, col = makeTransparent("Blue", 50), xlim = c(0, 30), add = T)
  
  
  
  its=sample(1:nrow(dt), size = 600)
  
  for(it in 1:length(its)){
    xx=seq(from = 0.01, to = max(durs$duration+2), by=.01)
    lines(x=xx, y=dweibull(xx, shape=dt$w_shape[it], scale=dt$w_scale_0[it]), col=makeTransparent("red", alpha=5))
  }
  
}

dev.off()

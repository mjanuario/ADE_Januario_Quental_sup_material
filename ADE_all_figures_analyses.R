# #################################################################
# #####             GENERATING PYRATE INPUT               #####
# #################################################################
# 
# 
# setwd("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR/")
 source("~/Desktop/PyRateTools/R/pyrate_tools_pre_package.R")
# 
# 
# #resampling
# extract.ages(file="OUR_sp.tsv", replicates = 100)
# extract.ages(file="OUR_gen.tsv", replicates = 100)
# 
# 
# #genera level
# parallel.pyrate(command_line = "python PyRate.py OUR_gen_PyRate.py -N 79 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 1000 -b 1000000 -n 26000000 -p 1000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter Inf 10.5", replicates = 1:100, n_cores = 20, folder = "~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR/")
# 
# 
# #sensibility
# parallel.pyrate(command_line = "python PyRate.py OUR_sp_PyRate.py -N 197 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 1000 -b 1000000 -n 26000000 -p 1000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter Inf 6.5", replicates = 1:50, n_cores = 50, folder = "~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR/")
# 
# parallel.pyrate(command_line = "python PyRate.py OUR_sp_PyRate.py -N 197 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 1000 -b 1000000 -n 26000000 -p 1000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter Inf 6.5", replicates = 51:100, n_cores = 50, folder = "~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR/")
# 
# 
# #conservative
# parallel.pyrate(command_line = "python PyRate.py OUR_sp_PyRate.py -N 197 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 1000 -b 1000000 -n 26000000 -p 1000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter 39.5 8", replicates = 1:50, n_cores = 50, folder = "~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR/")
# 
# parallel.pyrate(command_line = "python PyRate.py OUR_sp_PyRate.py -N 197 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 1000 -b 1000000 -n 26000000 -p 1000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter 39.5 8", replicates = 51:100, n_cores = 50, folder = "~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR/")
# 
# 
# #################################################################
# #####             MONO and POLITYPIC RUNS               #####
# #################################################################
# 
# 
# 
# setwd("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR/")
# 
# gen=read.table("OUR_gen.tsv", sep="\t", header = T, as.is = T)
# dim(gen)
# 
# 
# 
# 
# setwd("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR_gen_mono_1/")
# 
# t=read.table("OUR_sp.tsv", sep="\t", header = T, as.is = T)
# spp=unique(t$Species)
# 
# tab=table(sub(pattern = "_.*", "", spp))
# mono=names(tab)[tab==1]
# poli=names(tab)[tab>1]
# 
# 
# gen=read.table("OUR_gen.tsv", sep="\t", header = T, as.is = T)
# gen=gen[gen$Species %in% mono,]
# unique(gen$Species)
# dim(gen)
# 
# write.table(gen, "OUR_gen.tsv",quote = F, sep = "\t", row.names = F)
# 
# extract.ages(file="OUR_gen.tsv", replicates = 100)
# 
# 
# parallel.pyrate(command_line = "python PyRate.py OUR_gen_PyRate.py -N 79 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 1000 -b 1000000 -n 26000000 -p 1000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter Inf 10.5", replicates = 51:100, n_cores = 50, folder = "~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR_gen_mono_2/")
# 
# 
# setwd("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR_gen_poli_1/")
# 
# gen=read.table("OUR_gen.tsv", sep="\t", header = T, as.is = T)
# gen=gen[gen$Species %in% poli,]
# unique(gen$Species)
# 
# write.table(gen, "OUR_gen.tsv",quote = F, sep = "\t", row.names = F)
# 
# extract.ages(file="OUR_gen.tsv", replicates = 100)
# 
# parallel.pyrate(command_line = "python PyRate.py OUR_gen_PyRate.py -N 79 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 1000 -b 1000000 -n 26000000 -p 1000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter Inf 10.5", replicates = 51:100, n_cores = 50, folder = "~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR_gen_poli_2/")
# 
# 
# 


#################################################################
#####             READING MCMC FILES               #####
#################################################################


gen=read.table("~/Downloads/ADE_final/mcmc_gen.tsv", header = T, sep="\t", as.is = T)
dim(gen)
gen$replica=rep(1:100, each=1000)

spc=read.table("~/Downloads/ADE_final/mcmc_spc.tsv", header = T, sep="\t", as.is = T)
dim(spc)
spc$replica=rep(1:100, each=1000)

sps=read.table("~/Downloads/ADE_final/mcmc_sps.tsv", header = T, sep="\t", as.is = T)
dim(sps)
sps$replica=rep(1:100, each=1000)

#################################################################
#####             MAIN FIGURES AND ANALYSES               #####
#################################################################

################################
### BEGIN figure 1
################################

res=data.frame(
  Shape=c( gen$w_shape,
           spc$w_shape,
           sps$w_shape),
  
  dataset=rep(
    c("Genera", "Species - conservative", "Species - sensibility"),
    times=c(length(gen$w_shape),
            length(spc$w_shape),
            length(sps$w_shape))
  ))



ggplot(data=res, aes(x=dataset, y=Shape, fill=dataset, col=dataset))+
  geom_violin()+
  ylab("Shape Value") +
  xlab("Dataset") +
  scale_colour_manual(values = c("black", "grey50", "grey75"))+
  scale_fill_manual(values = c("black", "grey50", "grey75"))+
  geom_abline(slope = 0, intercept = 1, linetype=2, alpha=.5)+
  theme(legend.position="none")
ggsave(filename = "~/Desktop/Fig_1.png", width = 10, units = "in", height = 5.3)


################################
### END figure 1
################################

################################
### BEGIN figure 2
################################

#read data
our_sp=read.table("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR_spc_1/OUR_sp.tsv", sep="\t", header=T)


durspc=get.durations(spc, output = "stats", na.rm=T)
durspc$lineage=as.character(durspc$lineage)

dursps=get.durations(sps, output = "stats", na.rm=T)
dursps$lineage=as.character(dursps$lineage)


#calculating lineages wich are not fully within our time-window
aux1=aggregate(x = our_sp$MinT, by=list(our_sp$Species), FUN = min)
colnames(aux1)=c("lineage", "min")
aux2=aggregate(x = our_sp$MaxT, by=list(our_sp$Species), FUN = max)
colnames(aux2)=c("lineage", "max")
raw_dur=merge.data.frame(aux1, aux2)
raw_dur$windowspc="no"
raw_dur$windowsps="no"

raw_dur$windowspc[which(raw_dur$min>8 & raw_dur$max<39.5)]="yes" #conservative window
raw_dur$windowsps[which(raw_dur$min>6.5)]="yes" #wider window


durspc$window=NA
dursps$window=NA

for(i in 1:nrow(durspc)){
  durspc$window[i] = raw_dur$windowspc[which(raw_dur$lineage==durspc$lineage[i])]
  
  #window[i]=raw_dur$window[which(raw_dur$lineage==dur$lineage[i])] #remove if everything seems ok
}
for(i in 1:nrow(dursps)){
  dursps$window[i] = raw_dur$windowsps[which(raw_dur$lineage==dursps$lineage[i])]
}

#transforming in factor and adjusting levels order
durspc$window=factor(x = durspc$window, levels = c("no", "yes"))
dursps$window=factor(x = dursps$window, levels = c("no", "yes"))


############# plot A


library(cowplot)
bw=1.2
dat=durspc
n=nrow(dat)
ids_weibull=sample(1:nrow(sps), size = 600)
ggplot()+
  geom_histogram(data=dat, aes(fill=window, x=duration, y=(..count..)/(n*bw)),  position="stack", binwidth = bw) + 
  scale_fill_manual(values = c("grey25", "grey75")) + 
  scale_color_manual(values = c("blue", "white")) + 
  theme(legend.position="none") +
  guides(fill=guide_legend(title="all occurrences within window"))+
  #xlim(c(0,22))+
  ylim(c(0,.6))+
  mapply(function(shape, scale, col) {
    geom_line(
      data=data.frame(x=seq(.1, 25, by=.1), y=dweibull(seq(.1, 25, by=.1), shape = shape, scale = scale)),
      mapping = aes(x=x, y=y), color="red", alpha=.01)
  }, 
  # parameters and colors here
  shape = spc$w_shape[ids_weibull], 
  scale = spc$w_scale_0[ids_weibull], 
  col = rep("red", times=length(ids_weibull))
  )+
  ylab("Density")+
  xlab("Duration (My)")+
  annotate("text", x=20, y=.5, label="A", size=20)+
  ggtitle("Species - wider window") #Including window crossers

ggsave(filename = "~/Desktop/sampled_spc_weibulls.png", width = 10, units = "in", height = 5.3)


############# plot B


library(cowplot)
bw=1.2
dat=dursps
n=nrow(dat)
ids_weibull=sample(1:nrow(sps), size = 600)
ggplot()+
  geom_histogram(data=dat, aes(fill=window, x=duration, y=(..count..)/(n*bw)),  position="stack", binwidth = bw) + 
  scale_fill_manual(values = c("grey25", "grey75")) + 
  scale_color_manual(values = c("blue", "white")) + 
  theme(legend.position="none") +
  guides(fill=guide_legend(title="all occurrences within window"))+
  #xlim(c(0,22))+
  ylim(c(0,.6))+
  mapply(function(shape, scale, col) {
    geom_line(
      data=data.frame(x=seq(.1, 25, by=.1), y=dweibull(seq(.1, 25, by=.1), shape = shape, scale = scale)),
      mapping = aes(x=x, y=y), color="red", alpha=.01)
  }, 
  # parameters and colors here
  shape = sps$w_shape[ids_weibull], 
  scale = sps$w_scale_0[ids_weibull], 
  col = rep("red", times=length(ids_weibull))
  )+
  ylab("Density")+
  xlab("Duration (My)")+
  annotate("text", x=20, y=.5, label="B", size=20)+
  ggtitle("Species - wider window") #Including window crossers

ggsave(filename = "~/Desktop/sampled_sps_weibulls.png", width = 10, units = "in", height = 5.3)


################################
### END figure 2
################################

################################
### BEGIN figure 3
################################

w_sh_PRT=vector()
w_sh_FDP=vector()
w_sc_PRT=vector()
w_sc_FDP=vector()
replica=vector()
d_aic_FDP=vector()
dtst=vector()
dtsts=c("Genera", "Conservative", "Sensibility")





for(i in 1:100){
  aux = gen[gen$replica==i,]
  w_sh_PRT[i] = median(aux$w_shape)
  w_sc_PRT[i] = median(aux$w_scale_0)
  
  dur=get.durations(aux)
  dur=dur[!is.na(dur$duration),]
  w_sh_FDP[i]=fitdistrplus::fitdist(dur$duration, "weibull")$estimate[1]
  w_sc_FDP[i]=fitdistrplus::fitdist(dur$duration, "weibull")$estimate[2]
  replica[i]=i
  dtst[i]="Genera"
  
  aicwei=fitdist(dur$duration, "weibull")$aic
  aicexp=fitdist(dur$duration, "exp")$aic
  d_aic_FDP[i]=aicexp-aicwei
  
  
  
  aux = spc[spc$replica==i,]
  w_sh_PRT[100+i] = median(aux$w_shape)
  w_sc_PRT[100+i] = median(aux$w_scale_0)
  
  dur=get.durations(aux)
  dur=dur[!is.na(dur$duration),]
  w_sh_FDP[100+i]=fitdistrplus::fitdist(dur$duration, "weibull")$estimate[1]
  w_sc_FDP[100+i]=fitdistrplus::fitdist(dur$duration, "weibull")$estimate[2]
  replica[100+i]=i
  dtst[100+i]="Species - conservative window"
  
  aicwei=fitdist(dur$duration, "weibull")$aic
  aicexp=fitdist(dur$duration, "exp")$aic
  d_aic_FDP[100+i]=aicexp-aicwei
  
  
  
  aux = sps[sps$replica==i,]
  w_sh_PRT[200+i] = median(aux$w_shape)
  w_sc_PRT[200+i] = median(aux$w_scale_0)
  
  dur=get.durations(aux)
  dur=dur[!is.na(dur$duration),]
  w_sh_FDP[200+i]=fitdistrplus::fitdist(dur$duration, "weibull")$estimate[1]
  w_sc_FDP[200+i]=fitdistrplus::fitdist(dur$duration, "weibull")$estimate[2]
  replica[200+i]=i
  dtst[200+i]="Species - wider window"  
  
  aicwei=fitdist(dur$duration, "weibull")$aic
  aicexp=fitdist(dur$duration, "exp")$aic
  d_aic_FDP[200+i]=aicexp-aicwei
  
  
  
  print(i)
}



res=data.frame(value= c(w_sh_PRT, w_sh_FDP, w_sc_PRT, w_sc_FDP),
               est  = rep(c("PyRate", "FitDistrPlus"), each=300),
               par  = rep(c("Shape", "Scale"), each=600),
               dtst = rep(dtsts, each= 100),
               rep  = rep(1:100, times=12),
               d_aic= c(rep(NA, times=300), d_aic_FDP)
)


res$model[res$d_aic < 2]="Exponential"
res$model[res$d_aic >= 2]="Weibul"
res$model[is.na(res$d_aic)]="PyRate estimate"


res$beg=c(w_sh_PRT, w_sh_PRT, w_sc_PRT, w_sc_PRT)
res$end=c(w_sh_FDP, w_sh_FDP, w_sc_FDP, w_sc_FDP)
res$groups=paste0(res$dtst, res$rep, res$par)

res$est=factor(res$est, levels=c("PyRate", "FitDistrPlus"))
res$dtst=factor(res$dtst, levels=c("Genera", "Conservative", "Sensibility"))



res$dev=rep(rnorm(300), ach=300)
dodges=position_jitter(width = .02, h=0, seed = 100)



ggplot(data=res[res$par=="Shape",], aes(x=est, y=value))+
  #geom_boxplot(aes(col=model))+
  scale_shape_manual(values=c(1,2))+
  geom_abline(slope = 0, intercept = 1, linetype=2, alpha=.5, col="#1b7837", size=1.5)+
  scale_x_discrete()+
  geom_segment(aes(x=1, xend=2, y=beg, yend=end, group=groups), alpha=.1)+
  geom_point(aes(col=model, shape=est), alpha=1, size=2.5, position = dodges)+
  facet_grid(~dtst)+
  scale_color_manual(values=c("#1b7837", "black", "#762a83"))+
  xlab("Parameter estimation")+
  theme(legend.position="bottom")+
  ylab("Shape Value")+
  labs(col = "Selected model", shape="Estimation method")

ggsave(filename = "~/Desktop/Fig_5.png", width = 10, units = "in", height = 5.3)


table(res[res$dtst=="Genera",]$model)


################################
### END figure 3
################################

################################
### BEGIN figure 4
################################

#read data
spp=read.table("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR_spc_1/OUR_sp.tsv", sep="\t", header=T)
rich_tab=as.data.frame(table(sub("_.*", "", unique(spp$Species))))


dur=get.durations(gen)

#move to "dur" object
dur$lineage=as.character(dur$lineage)
rich=vector()
for(i in 1:nrow(dur)){
  rich[i]=rich_tab[which(rich_tab$Var1==dur$lineage[i]),2]   
}
dur$rich=rich

#calculating monotypic genera in the distribution of lineage durations:
dur$monotypic="no"
dur$monotypic[dur$rich==1]="yes"


#ploting
dat=dur
#dat=subset(dur, subset=dur$window=="in")

#calculating histogram binwidth:
n=nrow(dat)
bw=1

#ploting
ggplot()+
  geom_histogram(data=dat, aes(x=duration, y=(..count..)/(n*bw), fill=monotypic), position="stack", binwidth = bw) + 
  scale_fill_manual(values = c("grey25", "grey75"), labels=c("Politypic", "Monotypic")) + 
  scale_color_manual(values = c("blue", "white")) + 
  theme(legend.position="bottom") +
  guides(fill=guide_legend(title="Genus Class"))+
  #xlim(c(0,22))+
  ylim(c(0,0.25))+
  mapply(function(shape, scale, col) {
    geom_line(
      data=data.frame(x=seq(.1, 25, by=.1), y=dweibull(seq(.1, 25, by=.1), shape = shape, scale = scale)),
      mapping = aes(x=x, y=y), color="red", alpha=.1)
  }, 
  # parameters and colors here
  shape = gen$w_shape[ids_weibull], 
  scale = gen$w_scale_0[ids_weibull], 
  col = rep("red", times=length(ids_weibull))
  )+
  ylab("Density")+
  xlab("Duration (My)")+
  ggtitle("Genera level") #Including window crossers
ggsave(filename = "~/Desktop/sampled_genera_weibulls.png", width = 10, units = "in", height = 5.3)

################################
### END figure 4
################################


################################
### BEGIN figure 5
################################

########################
# TIME-WINDOW 
########################
#calculating lineages wich are not fully within our time-window

#read data
our_gen=read.table("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR_gen_1/OUR_gen.tsv", sep="\t", header=T)

#calculation
aux1=aggregate(x = our_gen$MinT, by=list(our_gen$Species), FUN = min)
colnames(aux1)=c("lineage", "min")
aux2=aggregate(x = our_gen$MaxT, by=list(our_gen$Species), FUN = max)
colnames(aux2)=c("lineage", "max")
raw_dur=merge.data.frame(aux1, aux2)
raw_dur$window="in"
raw_dur$window[which(raw_dur$min<10.5)]="out"

#move to "dur" object:
window=vector()
for(i in 1:nrow(dur)){
  window[i]=raw_dur$window[which(raw_dur$lineage==dur$lineage[i])]
}
dur$window=window



#R-squared function:
rsq <- function (x, y) cor(x, y) ^ 2

model=lm(duration~0+rich, data=dur, subset = dur$window=="in")
summary(model)

rsq_emp=rsq(x=dur$rich[dur$window=="in"], y=dur$duration[dur$window=="in"])

dur[,6:8]=predict(model, newdata=dur, interval="confidence")
colnames(dur)[6:8]=c("fit", "lwr", "upr")

dur$rich_alt=dur$rich+rnorm(sd = .1, n=nrow(dur))

ggplot(data=dur, aes(x=rich, y=duration, ymin=lwr, ymax=upr))+
  #geom_point(aes(color=window))+
  scale_x_continuous(breaks=1:10)+
  geom_jitter(aes(color=window), width = 0.25)+
  scale_color_manual(values = c("red", "black")) + 
  geom_ribbon(alpha=.3, fill="red")+
  geom_line(aes(x=rich, y=fit), col="red")+
  ylab("Duration (My)")+
  xlab("Genera richness")+
  ggtitle("Genus level")
ggsave(filename = "~/Desktop/richness_vs_duration_gen.png", width = 10, units = "in", height = 5.3)
dur=dur[,-6:-8]


################################
### END figure 5
################################


################################
### BEGIN table 2
################################

cv=function(x){
  return(((sd(x)/mean(x))))
}


emp.hpd(spc$w_shape, conf = .95)
median(spc$w_shape)
cv(aggregate(spc$w_shape, by=list(spc$replica), FUN=median)[,2])

emp.hpd(spc$w_scale_0, conf = .95)
median(spc$w_scale_0)
cv(aggregate(spc$w_scale_0, by=list(spc$replica), FUN=median)[,2])

emp.hpd(spc$mean_longevity_0, conf = .95)
median(spc$mean_longevity_0)
cv(aggregate(spc$mean_longevity_0, by=list(spc$replica), FUN=median)[,2])

#SP SENSIBILITY
emp.hpd(sps$w_shape, conf = .95)
median(sps$w_shape)
cv(aggregate(sps$w_shape, by=list(sps$replica), FUN=median)[,2])

emp.hpd(sps$w_scale_0, conf = .95)
median(sps$w_scale_0)
cv(aggregate(sps$w_scale_0, by=list(sps$replica), FUN=median)[,2])

emp.hpd(sps$mean_longevity_0, conf = .95)
median(sps$mean_longevity_0)
cv(aggregate(sps$mean_longevity_0, by=list(sps$replica), FUN=median)[,2])


#GENERA
emp.hpd(gen$w_shape, conf = .95)
median(gen$w_shape)
cv(aggregate(gen$w_shape, by=list(gen$replica), FUN=median)[,2])

emp.hpd(gen$w_scale_0, conf = .95)
median(gen$w_scale_0)
cv(aggregate(gen$w_scale_0, by=list(gen$replica), FUN=median)[,2])

emp.hpd(gen$mean_longevity_0, conf = .95)
median(gen$mean_longevity_0)
cv(aggregate(gen$mean_longevity_0, by=list(gen$replica), FUN=median)[,2])


################################
### END table 2
################################


#################################################################
#####               SUPPLEMENTARY FIGURES                 #####
#################################################################


################################
### BEGIN sup figure 1
################################

png(filename = "~/Desktop/ext_behavior.png", width = 2100, height = 1100)

par(mfrow=c(1,2),cex=3)
plot(NA, ylim=c(0, .5), xlim=c(0, 10), ylab="Expected distribution of lineages", xlab="Duration (My)")
curve(dweibull(x, shape = 1, scale = 3), add=T, lwd=5)
curve(dweibull(x, shape = .5, scale = 3), add=T, lwd=5, col="grey75", lty=2)
curve(dweibull(x, shape = 1.45, scale = 3), add=T, lwd=5, col="grey75")
text(x=9.3, y=.47, labels = "A", cex = 2)

scale=3
dur=seq(from=0, to=10, by=.1)
plot(NA, ylim=c(0, .8), xlim=c(0, 10), ylab="Extinction rate", xlab="Lineage age (My)")

shape=1
eP=(shape/scale)*((dur/scale)^(shape-1)) #follwoing eq 1 Hagen et al 2018
lines(x = dur, y=eP, lwd=5)

shape=.5
eP=(shape/scale)*((dur/scale)^(shape-1))
lines(x=dur, y=eP, lwd=5, col="grey75", lty=2 )

shape=1.45
eP=(shape/scale)*((dur/scale)^(shape-1))
lines(x=dur, y=eP, lwd=5, col="grey75")

text(x=9.3, y=.75, labels = "B", cex = 2)

dev.off()

################################
### END sup figure 1
################################


################################
### BEGIN sup figure 2
################################

link: https://docs.google.com/drawings/d/1QSpcdvuietrafzVIeUPGaledQJnVZbm4k2vhMwK-vBw/edit

################################
### END sup figure 2
################################


################################
### BEGIN sup figure 3
################################

pre_analis=merge.pyrate(folder = "~/Desktop/for_ADE/sp/", prunning = 100, log = "rates", print_progress = T)
binned_ex=make.RTT(logRJ = pre_analis$ex_rates, max_time = 80, min_time = 0, resolution = .1)
hpd=make.HPD.RTT(binned_ex, density = .95)


png(filename = "~/Desktop/sp_windows.png", width = 2100, height = 1100)

plot.pyrate(HPD=list(hpd), qShift = c(56, 33.9, 23.03, 15.97, 11.65, 5.3, 2.58, 1.8), xlim=c(60, 0), ylim=c(0, 1.3), col=c("red"), alpha=c(.5,.5), lwd=4, ylab="Rate", xlab="mya",  qShift_alpha=.2, main="Species level")
add.shifts.HDR(pre_analis$ex_rates, HDR = hpd, col = "red", y = -0.02)

rect(xleft = c(70, 70), xright = c(6.5,6.5), ytop =1.3, ybottom = 1.1, lwd = 3, border = "white", col= "red")
text(x = 27, y=1.2, labels = "Wider window", col="white", cex = 1)
            
rect(xleft = c(39.5, 39.5), xright = c(8,8), ytop =1.0, ybottom = 0.8, lwd = 3, border = "white", col= "red")
text(x = 27, y=0.9, labels = "Conservative window", col="white", cex = 1)

dev.off()
            
################################
### END sup figure 3
################################


################################
### BEGIN sup figure 4
################################

pre_gen_analis=merge.pyrate(folder = "~/Desktop/for_ADE/gen/", prunning = 100, log = "rates", print_progress = T)
binned_ex=make.RTT(logRJ = pre_gen_analis$ex_rates, max_time = 80, min_time = 0, resolution = .1)
hpd=make.HPD.RTT(binned_ex, density = .95)


png(filename = "~/Desktop/gen_windows.png", width = 2100, height = 1100)

plot.pyrate(HPD=list(hpd), qShift = c(56, 33.9, 23.03, 15.97, 11.65, 5.3, 2.58, 1.8), xlim=c(60, 0), ylim=c(0, .8), col=c("red"), alpha=c(.5,.5), lwd=4, ylab="Rate", xlab="mya",  qShift_alpha=.2, main="Genus level")
add.shifts.HDR(pre_gen_analis$ex_rates, HDR = hpd, col = "red", y = 0)

rect(xleft = c(70, 70), xright = c(10.5,10.5), ytop =.8, ybottom = .7, lwd = 3, border = "white", col= "red")
text(x = 27, y=.75, labels = "Genus window", col="white", cex = 1)


dev.off()


################################
### END sup figure 4
################################


################################
### BEGIN sup figure 5
################################

png(filename = "~/Desktop/sp_cons_dataset.png", width = 1050, height = 550)

dataset=our_sp[order(our_sp$MinT, decreasing = T),]
dataset=dataset[order(dataset$MaxT, decreasing=T),]
plot(x=NA, y=NA, xlim = c(60,0), ylim=c(0, 7900), main = "Species - Conservative Analysis", xlab="Mya", ylab="Occurrences", axes=F, cex=2)
Axis(side=1, labels=T)

cols=rep("red", times=nrow(dataset))

tab=data.frame(sp=sort(unique(dataset$Species)), min=tapply(dataset$MinT, dataset$Species, min), max=tapply(dataset$MaxT, dataset$Species, max))


exclud=as.character(tab$sp[which(tab$min<8)])
exclud2=as.character(tab$sp[which(tab$max>39.5)])
excluded=unique(as.character(c(exclud, exclud2)))

for(i in 1:length(excluded)){
  cols[which(dataset$Species==excluded[i])]="grey75"
}


for(i in 1:7900){
  segments(x0 = dataset$MaxT[i], x1=dataset$MinT[i], y0=i, y1=i, col=cols[i])
}

rect(xleft = c(39.5, 39.5), xright = c(8,8), ytop =8400, ybottom = 7900, lwd = 3, border = "white", col= "red")
text(x = 27, y=8050, labels = "Analysis Time window", col="white", cex = 1)


dev.off()


################################
### END sup figure 5
################################


################################
### BEGIN sup figure 6
################################

png(filename = "~/Desktop/sp_sens_dataset.png", width = 1050, height = 550)

dataset=our_sp[order(our_sp$MinT, decreasing = T),]
dataset=dataset[order(dataset$MaxT, decreasing=T),]
plot(x=NA, y=NA, xlim = c(60,0), ylim=c(0, 7900), main = "Species - Wider Window Analysis", xlab="Mya", ylab="Occurrences", axes=F, cex=2)
Axis(side=1, labels=T)

cols=rep("red", times=nrow(dataset))

tab=data.frame(sp=sort(unique(dataset$Species)), min=tapply(dataset$MinT, dataset$Species, min), max=tapply(dataset$MaxT, dataset$Species, max))


exclud=as.character(tab$sp[which(tab$min<6.5)])
exclud2=as.character(tab$sp[which(tab$max>100)])
excluded=unique(as.character(c(exclud, exclud2)))

for(i in 1:length(excluded)){
  cols[which(dataset$Species==excluded[i])]="grey75"
}


for(i in 1:7900){
  segments(x0 = dataset$MaxT[i], x1=dataset$MinT[i], y0=i, y1=i, col=cols[i])
}

rect(xleft = c(70, 70), xright = c(6.5,6.5), ytop =8400, ybottom = 7900, lwd = 3, border = "white", col= "red")
text(x = 27, y=8050, labels = "Analysis Time window", col="white", cex = 1)


dev.off()


################################
### END sup figure 6
################################


################################
### BEGIN sup figure 7
################################

png(filename = "~/Desktop/gen_dataset.png", width = 1050, height = 550)

dataset=our_gen[order(our_gen$MinT, decreasing = T),]
dataset=dataset[order(dataset$MaxT, decreasing=T),]
plot(x=NA, y=NA, xlim = c(60,0), ylim=c(0, 7900), main = "Genus level", xlab="Mya", ylab="Occurrences", axes=F, cex=2)
Axis(side=1, labels=T)

cols=rep("red", times=nrow(dataset))

tab=data.frame(sp=sort(unique(dataset$Species)), min=tapply(dataset$MinT, dataset$Species, min), max=tapply(dataset$MaxT, dataset$Species, max))


exclud=as.character(tab$sp[which(tab$min<10.5)])
exclud2=as.character(tab$sp[which(tab$max>100)])
excluded=unique(as.character(c(exclud, exclud2)))

for(i in 1:length(excluded)){
  cols[which(dataset$Species==excluded[i])]="grey75"
}


for(i in 1:7900){
  segments(x0 = dataset$MaxT[i], x1=dataset$MinT[i], y0=i, y1=i, col=cols[i])
}

rect(xleft = c(70, 70), xright = c(10.5,10.5), ytop =8400, ybottom = 7900, lwd = 3, border = "white", col= "red")
text(x = 27, y=8050, labels = "Analysis Time window", col="white", cex = 1)


dev.off()

################################
### END sup figure 7
################################


################################
### BEGIN sup figure 8
################################

t=aggregate(spc[,c(2:12, 14:16, which(colnames(spc)=="replica"))], by=list(spc$replica), LaplacesDemon::ESS)
aux=t[,-c(1,16)]
head(aux)
aux=melt(aux)
aux$kind=rep(c("MCMC", "MCMC", "MCMC", "MCMC", "preservation", "preservation", "preservation", "preservation", "preservation", "preservation", "Clade origin", "ADE model", "ADE model", "ADE model"), each=100)
colnames(aux)=c("Parameter", "ESS", "Role")
levels(aux$Parameter)=c(levels(aux$Parameter)[1:11], "shape", "scale", "mean_dur")

ggplot(data=aux, aes(x=Parameter, y=ESS, color=Role))+
  geom_boxplot()+
  scale_color_manual(values=c("#AD723A", "#749F35", "#25606B", "#92315D"))+
  xlab("Parameter")+
  theme(legend.position="bottom")+
  ylab("ESS") +
  geom_abline(slope = 0, intercept = 200, linetype=2, color="black")+
  ggtitle(label = "Species level - Conservative window")
ggsave(filename = "~/Desktop/ESS_spc.png", width = 10, units = "in", height = 5.3)


################################
### END sup figure 8
################################


################################
### BEGIN sup figure 9
################################

t=aggregate(sps[,c(2:12, 14:16, which(colnames(sps)=="replica"))], by=list(sps$replica), LaplacesDemon::ESS)
aux=t[,-c(1,16)]
head(aux)
aux=melt(aux)
aux$kind=rep(c("MCMC", "MCMC", "MCMC", "MCMC", "preservation", "preservation", "preservation", "preservation", "preservation", "preservation", "Clade origin", "ADE model", "ADE model", "ADE model"), each=100)
colnames(aux)=c("Parameter", "ESS", "Role")
levels(aux$Parameter)=c(levels(aux$Parameter)[1:11], "shape", "scale", "mean_dur")

ggplot(data=aux, aes(x=Parameter, y=ESS, color=Role))+
  geom_boxplot()+
  scale_color_manual(values=c("#AD723A", "#749F35", "#25606B", "#92315D"))+
  xlab("Parameter")+
  theme(legend.position="bottom")+
  ylab("ESS") +
  geom_abline(slope = 0, intercept = 200, linetype=2, color="black")+
  ggtitle(label = "Species level - Wider window")
ggsave(filename = "~/Desktop/ESS_sps.png", width = 10, units = "in", height = 5.3)


################################
### END sup figure 9
################################


################################
### BEGIN sup figure 10
################################

t=aggregate(gen[,c(2:12, 14:16, which(colnames(gen)=="replica"))], by=list(gen$replica), LaplacesDemon::ESS)
aux=t[,-c(1,16)]
head(aux)
aux=melt(aux)
aux$kind=rep(c("MCMC", "MCMC", "MCMC", "MCMC", "preservation", "preservation", "preservation", "preservation", "preservation", "preservation", "Clade origin", "ADE model", "ADE model", "ADE model"), each=100)
colnames(aux)=c("Parameter", "ESS", "Role")
levels(aux$Parameter)=c(levels(aux$Parameter)[1:11], "shape", "scale", "mean_dur")

ggplot(data=aux, aes(x=Parameter, y=ESS, color=Role))+
  geom_boxplot()+
  scale_color_manual(values=c("#AD723A", "#749F35", "#25606B", "#92315D"))+
  xlab("Parameter")+
  theme(legend.position="bottom")+
  ylab("ESS") +
  geom_abline(slope = 0, intercept = 200, linetype=2, color="black")+
  ggtitle(label = "Genus level")
ggsave(filename = "~/Desktop/ESS_gen.png", width = 10, units = "in", height = 5.3)


################################
### END sup figure 10
################################


################################
### BEGIN sup figure 11
################################

ggplot(data=spc, aes(y=w_shape, x=reorder(replica, w_shape, FUN=median), group=replica))+
  geom_boxplot()+
  xlab("Replicas")+
  #theme(legend.position="bottom")+
  ylab("Shape value") +
  ggtitle(label = "Species level - Conservative window")+
  geom_abline(slope = 0, intercept = 1, linetype=2, size=2, color="red", alpha=.4)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave(filename = "~/Desktop/shape_per_replica_spc.png", width = 10, units = "in", height = 5.3)


################################
### END sup figure 11
################################


################################
### BEGIN sup figure 12
################################

ggplot(data=sps, aes(y=w_shape, x=reorder(replica, w_shape, FUN=median), group=replica))+
  geom_boxplot()+
  xlab("Replicas")+
  #theme(legend.position="bottom")+
  ylab("Shape value") +
  ggtitle(label = "Species level - Wider window")+
  geom_abline(slope = 0, intercept = 1, linetype=2, size=2, color="red", alpha=.4)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave(filename = "~/Desktop/shape_per_replica_sps.png", width = 10, units = "in", height = 5.3)


################################
### END sup figure 12
################################


################################
### BEGIN sup figure 13
################################

ggplot(data=gen, aes(y=w_shape, x=reorder(replica, w_shape, FUN=median), group=replica))+
  geom_boxplot()+
  xlab("Replicas")+
  #theme(legend.position="bottom")+
  ylab("Shape value") +
  ggtitle(label = "Genus level")+
  geom_abline(slope = 0, intercept = 1, linetype=2, size=2, color="red", alpha=.4)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave(filename = "~/Desktop/shape_per_replica_gen.png", width = 10, units = "in", height = 5.3)


################################
### END sup figure 13
################################


################################
### BEGIN sup figure 14
################################


ggplot(data=gen, aes(y=w_shape, x=q_4))+
  geom_point(alpha=.1)+
  xlab("Mean preservation rate (q_4)")+
  geom_abline(slope = 0, intercept = 1, linetype=2, size=2, color="red", alpha=.4)+
  ylab("Shape value")
ggsave(filename = "~/Desktop/gen_q4_shape.png", width = 10, units = "in", height = 5.3)

################################
### END sup figure 14
################################


################################
### BEGIN sup figure 15
################################

ggplot(data=gen, aes(y=w_shape, x=q_3))+
  geom_point(alpha=.1)+
  xlab("Mean preservation rate (q_3)")+
  geom_abline(slope = 0, intercept = 1, linetype=2, size=2, color="red", alpha=.4)+
  ylab("Shape value")
ggsave(filename = "~/Desktop/gen_q3_shape.png", width = 10, units = "in", height = 5.3)


################################
### END sup figure 15
################################


################################
### BEGIN sup figure 16
################################

ggplot(data=gen, aes(y=w_shape, x=q_2))+
  geom_point(alpha=.1)+
  xlab("Mean preservation rate (q_2)")+
  geom_abline(slope = 0, intercept = 1, linetype=2, size=2, color="red", alpha=.4)+
  ylab("Shape value")
ggsave(filename = "~/Desktop/gen_q2_shape.png", width = 10, units = "in", height = 5.3)

################################
### END sup figure 16
################################


################################
### BEGIN sup figure 17
################################

ggplot(data=gen, aes(y=w_shape, x=q_1))+
  geom_point(alpha=.1)+
  xlab("Mean preservation rate (q_1)")+
  geom_abline(slope = 0, intercept = 1, linetype=2, size=2, color="red", alpha=.4)+
  ylab("Shape value")
ggsave(filename = "~/Desktop/gen_q1_shape.png", width = 10, units = "in", height = 5.3)

################################
### END sup figure 17
################################


################################
### BEGIN sup figure 18
################################

ggplot(data=gen, aes(y=w_shape, x=q_0))+
  geom_point(alpha=.1)+
  xlab("Mean preservation rate (q_0)")+
  geom_abline(slope = 0, intercept = 1, linetype=2, size=2, color="red", alpha=.4)+
  ylab("Shape value")
ggsave(filename = "~/Desktop/gen_q0_shape.png", width = 10, units = "in", height = 5.3)

################################
### END sup figure 18
################################


################################
### BEGIN sup figure 19
################################

mono=read.table("~/Downloads/ADE_final/mcmc_gen_mono.tsv", header = T, sep="\t", as.is = T)

poli=read.table("~/Downloads/ADE_final/mcmc_gen_poli.tsv", header = T, sep="\t", as.is = T)


monopar=melt(mono[,c(6:10, 14:16)])
monopar$Dataset="Monotypic"

polipar=melt(poli[,c(6:10, 14:16)])
polipar$Dataset="Politypic"

pars=rbind(monopar, polipar)

ggplot(pars, aes(x=variable, y=value, col=Dataset))+
  geom_boxplot()+
  geom_abline(slope = 0, intercept = 1, linetype=2, size=2, color="red", alpha=.4)+
  xlab("PyRate parameter")+
  ylab("Parameter value")+
  theme(legend.position="bottom")
ggsave(filename = "~/Desktop/poli_mono_parameters.png", width = 10, units = "in", height = 5.3)


################################
### END sup figure 19
################################


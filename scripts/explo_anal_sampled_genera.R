library(cowplot)
#getting packages and data:
source("~/Desktop/PyRateTools/pyrate_tools_pre_package.R")
mcmc=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/results/resam_gen.tsv", sep="\t", header = T)
mcmc$replica=rep(1:100, each=200)
gen=mcmc

########################
# DURATIONS
########################

#calculatign durations:
dur=get.durations(gen, output = "stats", na.rm=T, gen = T)
head(dur) 

########################
# MONOTYPIC GENERA
########################

#calculating genera richness
spp=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/data/OUR/OUR_sp.tsv", sep="\t", header=T)
rich_tab=as.data.frame(table(sub("_.*", "", unique(spp$Species))))

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

#ploting duration histogram for mono and politypic genera:
dat=dur
ggplot(data=dat, aes(x=duration, y=..density.., fill=monotypic))+
  geom_histogram(position="stack", binwidth = 1) + 
  scale_fill_manual(values = c("Grey25", "Grey75")) + 
  theme(legend.position="bottom") +
  guides(fill=guide_legend(title="Genus monotypic"))+
  ylab("Density")+
  xlab("Duration (My)")+
  ggtitle("Including window crossers")


########################
# TIME-WINDOW 
########################
#calculating lineages wich are not fully within our time-window

#read data
our_gen=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/data/OUR/OUR_gen.tsv", sep="\t", header=T)

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

#ploting duration histogram for mono and politypic genera, excluding genera ou of th window:

dat=subset(dur, subset=dur$window=="in")
#dat=dur
ggplot(data=dat, aes(x=duration, y=..density.., fill=monotypic))+
  geom_histogram(position="stack", binwidth = 1) + 
  scale_fill_manual(values = c("Grey25", "Grey75")) + 
  theme(legend.position="bottom") +
  guides(fill=guide_legend(title="Genus monotypic"))+
  ylab("Density")+
  xlab("Duration (My)")+
  ggtitle("Excluding window crossers")

########################
# ADJUSTING WEIBULLS PER REPLICA
########################

#summarising weibulls per replica
gen$replica=rep(1:100, each=200)
aux1=aggregate(gen$w_shape, by=list(gen$replica), FUN=median, na.rm=T)
colnames(aux1)=c("rep", "shape")
aux2=aggregate(gen$w_scale_0, by=list(gen$replica), FUN=median, na.rm=T)
colnames(aux2)=c("rep", "scale")
sum_wei=merge.data.frame(aux1, aux2)

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
  mapply(function(shape, scale, col) {
    geom_line(
      data=data.frame(x=seq(0, 24.9, by=.1), y=dweibull(seq(.1, 25, by=.1), shape = shape, scale = scale)),
      mapping = aes(x=x, y=y), color="red", alpha=.2)
  }, 
  # parameters and colors here
  shape = sum_wei[,2], 
  scale = sum_wei[,3], 
  col = rep("red", times=100)
  )+
  ylab("Density")+
  xlab("Duration (My)")+
  ggtitle("Genera level") #Including window crossers
ggsave(filename = "~/Desktop/sampled_genera_weibulls.png", width = 10, units = "in", height = 5.3)

########################
# UNDERSTANDING THE POSTERIOR SHAPE DISTRIBUTION
########################

#calculating medians for color in ggplot2:
mcmc$w_median=rep((aggregate(mcmc$w_shape, by=list(mcmc$replica), FUN=median))$x, each=200)

#now calculating a copy for the shape aprameter:
opa=data.frame(shp=mcmc$w_shape)

#now the stats for the boxplot:
whisk <- data.frame(x1 = quantile(mcmc$w_median, probs = .05), x2 = quantile(mcmc$w_median, probs = .95), y1 = 4.2, y2 = 4.2)
box <- data.frame(x1 = quantile(mcmc$w_median)[2], x2 = quantile(mcmc$w_median)[4], y1 = 4.1, y2 = 4.3)
med <- data.frame(x1 = quantile(mcmc$w_median)[3], x2 = quantile(mcmc$w_median)[3], y1 = 4.1, y2 = 4.3)

#ploting:
ggplot()+
  geom_vline(xintercept = 1, colour="black")+
  geom_density(data=mcmc, mapping = aes(x=w_shape, group=replica, colour=w_median), alpha = 0.1)+
  geom_density(data=opa, mapping = aes(x=shp), alpha = 0.1)+
  scale_colour_distiller(palette = "Spectral")+
  xlim(c(.4, 1.9))+
  ylab("Density")+
  xlab("Shape value")+
  labs(colour="Shape median")+
  geom_segment(data=whisk, aes(x = x1, y = y1, xend = x2, yend = y2))+
  geom_rect(data=box, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), colour="black", alpha=1)+
  geom_segment(data=med, aes(x = x1, y = y1, xend = x2, yend = y2), colour="white")
  

########################
# DURATIONS VS RICHNESS
########################

#R-squared function:
rsq <- function (x, y) cor(x, y) ^ 2

model=lm(duration~0+rich, data=dur, subset = dur$window=="in")
summary(model)

rsq_emp=rsq(x=dur$rich[dur$window=="in"], y=dur$duration[dur$window=="in"])

dur[,8:10]=predict(model, newdata=dur, interval="confidence")
colnames(dur)[8:10]=c("fit", "lwr", "upr")

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
  annotate("text", label="B", size=20, x=1.5, y=24)+
  annotate("text", label=paste0("R-sq = ", round(rsq_emp, digits = 2)), size=5, x=1.5, y=20)+
  ggtitle("Genera level")
ggsave(filename = "~/Desktop/richness_vs_duration_gen.png", width = 10, units = "in", height = 5.3)
dur=dur[,-8:-10]

########################
# DURATIONS VS GEOGRAPHIC RANGE
########################

#reading geographic data:
gen_range=read.table("~/Downloads/range_genus.tsv", header=T, sep=" ", as.is=T)
head(gen_range)

#unifying geographic data with the rest:
ranges=vector()
for(i in 1:nrow(dur)){
  ranges[i]=gen_range$range[which(gen_range$taxa==dur$lineage[i])]
}
dur$range=log(ranges, base=2)

#adequating the data:
model=lm(duration~range, data=dur, subset = dur$window=="in")
summary(model)

rsq_emp=rsq(x=dur$range[dur$window=="in"], y=dur$duration[dur$window=="in"])

dur[,9:11]=predict(model, newdata=dur, interval="confidence")
colnames(dur)[9:11]=c("fit", "lwr", "upr")

ggplot(data=dur, aes(x=range, y=duration, ymin=lwr, ymax=upr))+
  geom_point(aes(color=window))+
  scale_color_manual(values = c("red", "black")) + 
  geom_ribbon(alpha=.3, fill="red")+
  geom_line(data=dur, aes(x=range, y=fit), col="red")+
  ylab("Duration (My)")+
  xlab("Geographic range  - Log(Km2)")+
  annotate("text", label="A", size=20, x=10, y=20)+
  annotate("text", label=paste0("R-sq = ", round(rsq_emp, digits = 2)), size=5, x=10, y=16)+
  ggtitle("Genera level")
ggsave(filename = "~/Desktop/range_vs_duration_gen.png", width = 10, units = "in", height = 5.3)


dur=dur[,-9:-11]

########################
# MODEL SELECTION: DURATIONS RICHNESS
########################

full_mod=lm(duration~rich*range, data=dur, subset = dur$window=="in")
summary(full_mod) #range seems to not have effect
mod1=lm(duration~rich, data=dur, subset = dur$window=="in")
anova(full_mod, mod1) #the models seems equal in variation explained. keep the simple one

#calculating RMSE
RSS=c(crossprod(mod1$residuals))
MSE=RSS/length(mod1$residuals)
RMSE=sqrt(MSE)
RMSE #the closest to zero, the better

#comparing to RMSE from
RSS=c(crossprod(full_mod$residuals))
MSE=RSS/length(full_mod$residuals)
RMSE=sqrt(MSE)
RMSE #almost identical to the one with only richness

#But richness is indeed a poor predictor of genera duration:
rsq_emp=rsq(x=dur$rich[dur$window=="in"], y=dur$duration[dur$window=="in"])
rsq_emp

#most variation seems unexplained. This agrees with the results from FInnegan et al (2008).

########################
# Calculating for specific replicas
########################

#### the most positive ADE:
zoomed_rep=15
zoom_gen=mcmc[(zoomed_rep*200):(zoomed_rep*200+200),]

#calculatign durations:
zoom_dur=dur[,c(1,5,6,7)]
aux=get_durations(zoom_gen, output = "stats", na.rm=T)
zoom_dur=merge.data.frame(zoom_dur, aux)
head(zoom_dur)

#adjusting a weibul:
x=seq(0.01, 25, by=.1)
y=dweibull(x, shape = sum_wei[zoomed_rep,2], scale = sum_wei[zoomed_rep,3])
dwb=data.frame(x,y)

#ploting
dat=zoom_dur
dat=subset(zoom_dur, subset=dur$window=="in")

#calculating histogram binwidth:
n=nrow(dat)
bw=.75

#ploting
ggplot()+
  geom_histogram(data=dat, aes(x=duration, y=(..count..)/(n*bw), fill=monotypic), position="stack", binwidth = bw) + 
  scale_fill_manual(values = c("grey25", "grey75")) + 
  scale_color_manual(values = c("blue", "white")) + 
  theme(legend.position="bottom") +
  guides(fill=guide_legend(title="Genus Monotypic"))+
  #xlim(c(0,22))+
  geom_line(data=dwb, mapping = aes(x=x, y=y), colour="red")+
  ylab("Density")+
  xlab("Duration(My)")+
  ggtitle(paste0("Excluding window crossers, rep = ", zoomed_rep, "; shape = ", round(sum_wei[zoomed_rep,2], digits = 2), "; scale = ", round(sum_wei[zoomed_rep,3], digits = 2)))


#### the most negative ADE:
zoomed_rep=85
zoom_gen=mcmc[(zoomed_rep*200):(zoomed_rep*200+200),]

#calculatign durations:
zoom_dur=dur[,c(1,5,6,7)]
aux=get_durations(zoom_gen, output = "stats", na.rm=T)
zoom_dur=merge.data.frame(zoom_dur, aux)
head(zoom_dur)

#adjusting a weibul:
x=seq(0.01, 25, by=.1)
y=dweibull(x, shape = sum_wei[zoomed_rep,2], scale = sum_wei[zoomed_rep,3])
dwb=data.frame(x,y)

#ploting
dat=zoom_dur
dat=subset(zoom_dur, subset=dur$window=="in")

#calculating histogram binwidth:
n=nrow(dat)
bw=.75

#ploting
ggplot()+
  geom_histogram(data=dat, aes(x=duration, y=(..count..)/(n*bw), fill=monotypic), position="stack", binwidth = bw) + 
  scale_fill_manual(values = c("grey25", "grey75")) + 
  scale_color_manual(values = c("blue", "white")) + 
  theme(legend.position="bottom") +
  guides(fill=guide_legend(title="Genus Monotypic"))+
  #xlim(c(0,22))+
  geom_line(data=dwb, mapping = aes(x=x, y=y), colour="red")+
  ylab("Density")+
  xlab("Duration(My)")+
  ggtitle(paste0("Excluding window crossers rep = ", zoomed_rep, "; shape = ", round(sum_wei[zoomed_rep,2], digits = 2), "; scale = ", round(sum_wei[zoomed_rep,3], digits = 2)))

#### the most close to AIE:
zoomed_rep=78
zoom_gen=mcmc[(zoomed_rep*200):(zoomed_rep*200+200),]

#calculatign durations:
zoom_dur=dur[,c(1,5,6,7)]
aux=get_durations(zoom_gen, output = "stats", na.rm=T)
zoom_dur=merge.data.frame(zoom_dur, aux)
head(zoom_dur)

#adjusting a weibul:
x=seq(.01, 25, by=.1)
y=dweibull(x, shape = sum_wei[zoomed_rep,2], scale = sum_wei[zoomed_rep,3])
dwb=data.frame(x,y)

#ploting
dat=zoom_dur
dat=subset(zoom_dur, subset=dur$window=="in")

#calculating histogram binwidth:
n=nrow(dat)
bw=.75

#ploting
ggplot()+
  geom_histogram(data=dat, aes(x=duration, y=(..count..)/(n*bw), fill=monotypic), position="stack", binwidth = bw) + 
  scale_fill_manual(values = c("grey25", "grey75")) + 
  scale_color_manual(values = c("blue", "white")) + 
  theme(legend.position="bottom") +
  guides(fill=guide_legend(title="Genus Monotypic"))+
  #xlim(c(0,22))+
  geom_line(data=dwb, mapping = aes(x=x, y=y), colour="red")+
  ylab("Density")+
  xlab("Duration(My)")+
  ggtitle(paste0("Excluding window crossers rep = ", zoomed_rep, "; shape = ", round(sum_wei[zoomed_rep,2], digits = 2), "; scale = ", round(sum_wei[zoomed_rep,3], digits = 2)))


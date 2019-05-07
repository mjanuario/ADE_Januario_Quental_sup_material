rm(list=ls())
library(ggplot2)
library(cowplot)
source("~/Desktop/PyRateTools/pyrate_tools_pre_package.R")


#conservative analysis
spc=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/results/resam_spc.tsv", sep="\t", header = T)
spcdur=get.durations(spc, output = "stats", na.rm=T)
spcdur$lineage=as.character(spcdur$lineage)

sp_range=read.table("~/Downloads/range_species.tsv", header=T, sep=" ", as.is=T)
sp_range$lineage=rownames(sp_range)

spcdur=merge.data.frame(spcdur, sp_range)

#now, the window
our_sp=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/data/OUR/OUR_sp.tsv", sep="\t", header=T)

#calculation
aux1=aggregate(x = our_sp$MinT, by=list(our_sp$Species), FUN = min)
colnames(aux1)=c("lineage", "min")
aux2=aggregate(x = our_sp$MaxT, by=list(our_sp$Species), FUN = max)
colnames(aux2)=c("lineage", "max")
raw_dur=merge.data.frame(aux1, aux2)
raw_dur$window="no"
raw_dur$window[which(raw_dur$min>8 & raw_dur$max<39.5)]="yes" #conservative




#move to "dur" object:
window=vector()
for(i in 1:nrow(spcdur)){
  window[i]=raw_dur$window[which(raw_dur$lineage==spcdur$lineage[i])]
}
spcdur$window=window
class(spcdur$window)
spcdur$window=factor(x = spcdur$window, levels = c("no", "yes"))

#log-transforming range:
spcdur$range=log(spcdur$range, base=2)

#####################
#now, the linear regression for sensibilty:

#R-squared function:
rsq <- function (x, y) cor(x, y) ^ 2
#####################

spcdur$weel_sampled=spcdur$lineage %in% names(table(our_sp$Species)[table(our_sp$Species)>2])
spcdur=spcdur[spcdur$weel_sampled,]

model=lm(duration~range, data=spcdur[which(spcdur$window=="yes"),])
summary(model)

rsq_emp=rsq(x=spcdur$range, y=spcdur$duration)
spcdur=cbind(spcdur, predict(model, newdata=spcdur, interval="confidence"))


ggplot(data=spcdur, aes(x=range, y=duration, ymin=lwr, ymax=upr))+
  geom_point(aes(color=window))+
  scale_color_manual(values = c("black", "red")) + 
  geom_ribbon(alpha=.3, fill="red")+
  geom_line(data=spcdur, aes(x=range, y=fit), col="red")+
  ylab("Duration (My)")+
  xlab("Geographic range  - Log(Km2)")+
  annotate("text", label="A", size=20, x=4, y=15)+
  annotate("text", label=paste0("R-sq = ", round(rsq_emp, digits = 2)), size=5, x=4, y=12)+
  theme(legend.position="none") +
  ggtitle("Species level - Conservative")

ggsave(filename = "~/Desktop/duration_vs_range_spc.png", width = 10, units = "in", height = 5.3)


##########################################################################
##########################################################################
##########################################################################

#sensibility analysis
sps=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/results/resam_sps.tsv", sep="\t", header = T)
spsdur=get.durations(sps, output = "stats", na.rm=T)
spsdur$lineage=as.character(spsdur$lineage)

sp_range=read.table("~/Downloads/range_species.tsv", header=T, sep=" ", as.is=T)
sp_range$lineage=rownames(sp_range)

spsdur=merge.data.frame(spsdur, sp_range)

#now, the window
our_sp=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/data/OUR/OUR_sp.tsv", sep="\t", header=T)

#calculation
aux1=aggregate(x = our_sp$MinT, by=list(our_sp$Species), FUN = min)
colnames(aux1)=c("lineage", "min")
aux2=aggregate(x = our_sp$MaxT, by=list(our_sp$Species), FUN = max)
colnames(aux2)=c("lineage", "max")
raw_dur=merge.data.frame(aux1, aux2)
raw_dur$window="no"
raw_dur$window[which(raw_dur$min>6.5)]="yes" #wider window

#move to "dur" object:
window=vector()
for(i in 1:nrow(spsdur)){
  window[i]=raw_dur$window[which(raw_dur$lineage==spsdur$lineage[i])]
}
spsdur$window=window
class(spsdur$window)
spsdur$window=factor(x = spsdur$window, levels = c("no", "yes"))

#log-transforming range:
spsdur$range=log(spsdur$range, base=2)

#####################
#now, the linear regression for sensibilty:

#R-squared function:
rsq <- function (x, y) cor(x, y) ^ 2
#####################
spsdur$weel_sampled=spsdur$lineage %in% names(table(our_sp$Species)[table(our_sp$Species)>2])
spsdur=spsdur[spsdur$weel_sampled,]


model=lm(duration~range, data=spsdur[which(spsdur$window=="yes"),])
summary(model)

rsq_emp=rsq(x=spsdur$range, y=spsdur$duration)
spsdur=cbind(spsdur, predict(model, newdata=spsdur, interval="confidence"))


ggplot(data=spsdur, aes(x=range, y=duration, ymin=lwr, ymax=upr))+
  geom_point(aes(color=window))+
  scale_color_manual(values = c("black", "red")) + 
  geom_ribbon(alpha=.3, fill="red")+
  geom_line(data=spsdur, aes(x=range, y=fit), col="red")+
  ylab("Duration (My)")+
  xlab("Geographic range  - Log(Km2)")+
  annotate("text", label="B", size=20, x=2, y=15)+
  annotate("text", label=paste0("R-sq = ", round(rsq_emp, digits = 2)), size=5, x=2, y=12)+
  theme(legend.position="none") +
  ggtitle("Species - wider window")

ggsave(filename = "~/Desktop/duration_vs_range_sps.png", width = 10, units = "in", height = 5.3)



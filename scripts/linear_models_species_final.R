rm(list=ls())
library(cowplot)



#reading data
spc=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/results/resam_spc.tsv", sep="\t", header = T)
spcdur=get.durations(spc, output = "stats", na.rm = T)
spcdur$lineage=as.character(spcdur$lineage)

sp_range=read.table("~/Downloads/range_species.tsv", header=T, sep=" ", as.is = T)
sp_range=sp_range[sp_range$lineage %in% spcdur$lineage,]

#unifying datasets:
spcdur=unique(merge.data.frame(spcdur, sp_range, all=TRUE))
head(spcdur)

#now, the window
our_sp=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/data/OUR/OUR_sp.tsv", sep="\t", header=T)

#calculation
aux1=aggregate(x = our_sp$MinT, by=list(our_sp$Species), FUN = min)
colnames(aux1)=c("lineage", "min")
aux2=aggregate(x = our_sp$MaxT, by=list(our_sp$Species), FUN = max)
colnames(aux2)=c("lineage", "max")
raw_dur=merge.data.frame(aux1, aux2)
raw_dur$window="no"
raw_dur$window[which(raw_dur$min>10.5)]="yes" #genera level

#move to "dur" object:
window=vector()
for(i in 1:nrow(spcdur)){
  window[i]=raw_dur$window[which(raw_dur$lineage==spcdur$lineage[i])]
}

spcdur$window=window


#log-transforming range:
spcdur$range=log(spcdur$range, base=2)


#calculating which species is well sampled:
spp=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/data/OUR/OUR_sp.tsv", sep="\t", header=T)

t=table(spp$Species)
well_Sampled = names(t[t>2])

spcdur$well_sampled= spcdur$lineage %in% well_Sampled
spcdur=spcdur[spcdur$well_sampled,]

#####################
#R-squared function:
rsq <- function (x, y) cor(x, y) ^ 2
#####################

#linear model:
#testing richness infleunce over duration:
model=lm(duration~range, data=spcdur, subset = spcdur$window=="yes")
summary(model)

rsq_emp=rsq(x=spcdur$range[spcdur$window=="yes"], y=spcdur$duration[spcdur$window=="yes"])

spcdur[,8:10]=predict(model, newdata=spcdur, interval="confidence")
colnames(spcdur)[8:10]=c("fit", "lwr", "upr")

ggplot(data=spcdur, aes(x=range, y=duration, ymin=lwr, ymax=upr))+
  geom_point(aes(color=window))+
  scale_color_manual(values = c("black", "red")) + 
  geom_ribbon(alpha=.3, fill="red")+
  geom_line(data=spcdur, aes(x=range, y=fit), col="red")+
  ylab("Duration (My)")+
  xlab("Geographic range  - Log(Km2)")+
  annotate("text", label="A", size=20, x=5, y=15)+
  annotate("text", label=paste0("R-sq = ", round(rsq_emp, digits = 2)), size=5, x=5, y=12)+
  theme(legend.position="none") +
  ggtitle("Species level - Conservative")
spcdur=spcdur[,-8:-10]
ggsave(filename = "~/Desktop/duration_vs_range_spc.png", width = 10, units = "in", height = 5.3)




##########################################
##########################################
##########################################

#reading data
spc=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/results/resam_sps.tsv", sep="\t", header = T)
spcdur=get.durations(spc, output = "stats", na.rm = T)
spcdur$lineage=as.character(spcdur$lineage)

sp_range=read.table("~/Downloads/range_species.tsv", header=T, sep=" ", as.is = T)
sp_range=sp_range[sp_range$lineage %in% spcdur$lineage,]

#unifying datasets:
spcdur=unique(merge.data.frame(spcdur, sp_range, all=TRUE))
head(spcdur)

#now, the window
our_sp=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/data/OUR/OUR_sp.tsv", sep="\t", header=T)

#calculation
aux1=aggregate(x = our_sp$MinT, by=list(our_sp$Species), FUN = min)
colnames(aux1)=c("lineage", "min")
aux2=aggregate(x = our_sp$MaxT, by=list(our_sp$Species), FUN = max)
colnames(aux2)=c("lineage", "max")
raw_dur=merge.data.frame(aux1, aux2)
raw_dur$window="no"
raw_dur$window[which(raw_dur$min>10.5)]="yes" #genera level

#move to "dur" object:
window=vector()
for(i in 1:nrow(spcdur)){
  window[i]=raw_dur$window[which(raw_dur$lineage==spcdur$lineage[i])]
}

spcdur$window=window


#log-transforming range:
spcdur$range=log(spcdur$range, base=2)


#calculating which species is well sampled:
spp=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/data/OUR/OUR_sp.tsv", sep="\t", header=T)

t=table(spp$Species)
well_Sampled = names(t[t>2])

spcdur$well_sampled= spcdur$lineage %in% well_Sampled
spcdur=spcdur[spcdur$well_sampled,]

#####################
#R-squared function:
rsq <- function (x, y) cor(x, y) ^ 2
#####################

#linear model:
#testing richness infleunce over duration:
model=lm(duration~range, data=spcdur, subset = spcdur$window=="yes")
summary(model)

rsq_emp=rsq(x=spcdur$range[spcdur$window=="yes"], y=spcdur$duration[spcdur$window=="yes"])

spcdur[,8:10]=predict(model, newdata=spcdur, interval="confidence")
colnames(spcdur)[8:10]=c("fit", "lwr", "upr")

ggplot(data=spcdur, aes(x=range, y=duration, ymin=lwr, ymax=upr))+
  geom_point(aes(color=window))+
  scale_color_manual(values = c("black", "red")) + 
  geom_ribbon(alpha=.3, fill="red")+
  geom_line(data=spcdur, aes(x=range, y=fit), col="red")+
  ylab("Duration (My)")+
  xlab("Geographic range  - Log(Km2)")+
  annotate("text", label="B", size=20, x=3, y=15)+
  annotate("text", label=paste0("R-sq = ", round(rsq_emp, digits = 2)), size=5, x=3, y=12)+
  theme(legend.position="none") +
  ggtitle("Species level - wider window")
spcdur=spcdur[,-8:-10]
ggsave(filename = "~/Desktop/duration_vs_range_sps.png", width = 10, units = "in", height = 5.3)



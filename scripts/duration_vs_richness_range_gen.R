rm(list=ls())
library(cowplot)
source("~/Desktop/anal_mestrado/scripts_dados/pyrate_tools.R")


#reading data
spc=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/results/resam_gen.tsv", sep="\t", header = T)
spcdur=get.durations(spc, output = "stats", gen = T, na.rm = T)
head(spcdur)
sp_range=read.table("~/Downloads/range_genus.tsv", header=T, sep=" ", as.is=T)
colnames(sp_range)=c("lineage", "range")

spcdur=merge.data.frame(spcdur, sp_range)
spcdur$lineage=as.character(spcdur$lineage)


#now, the window
our_sp=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/data/OUR/OUR_gen.tsv", sep="\t", header=T)

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
spcdur$range=log(spcdur$range)


#calculating genera richness
spp=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/data/OUR/OUR_sp.tsv", sep="\t", header=T)
rich_tab=as.data.frame(table(sub("_.*", "", unique(spp$Species))))

#move to "dur" object
spcdur$lineage=as.character(spcdur$lineage)
rich=vector()
for(i in 1:nrow(spcdur)){
  rich[i]=rich_tab[which(rich_tab$Var1==spcdur$lineage[i]),2]   
}
spcdur$rich=rich

#####################
#R-squared function:
rsq <- function (x, y) cor(x, y) ^ 2
#####################

#linear model:
#testing richness infleunce over duration:
model=lm(duration~0+rich, data=spcdur, subset = spcdur$window=="yes")
summary(model)

rsq_emp=rsq(x=spcdur$rich[spcdur$window=="yes"], y=spcdur$duration[spcdur$window=="yes"])

spcdur[,8:10]=predict(model, newdata=spcdur, interval="confidence")
colnames(spcdur)[8:10]=c("fit", "lwr", "upr")

ggplot(data=spcdur, aes(x=rich, y=duration, ymin=lwr, ymax=upr))+
  scale_x_continuous(breaks=1:10)+
  geom_jitter(aes(color=window), width = 0.25)+
  scale_color_manual(values = c("black", "red")) + 
  geom_ribbon(alpha=.3, fill="red")+
  geom_line(aes(x=rich, y=fit), col="red")+
  ylab("Duration (My)")+
  xlab("Genera richness")+
  theme(legend.position="none") +
  annotate("text", label="B", size=20, x=1.5, y=26)+
  annotate("text", label=paste0("R-sq = ", round(rsq_emp, digits = 2)), size=5, x=1.5, y=22)+
  ggtitle("Genera level")
ggsave(filename = "~/Desktop/richness_vs_duration_gen.png", width = 10, units = "in", height = 5.3)
spcdur=spcdur[,-8:-10]

#now the relationship between duration and range

#removing genera with less than 2 occurrences:
t=table(our_sp$Species)
well_Sampled=names(t[t>2])
dim(spcdur)
spcdur=spcdur[spcdur$lineage %in% well_Sampled,]
dim(spcdur)

model_rang=lm(duration~range, data=spcdur, subset = spcdur$window=="yes")
summary(model_rang)  

rsq_emp=rsq(x=spcdur$range[spcdur$window=="yes"], y=spcdur$duration[spcdur$window=="yes"])


spcdur=cbind(spcdur, predict(model_rang, newdata=spcdur, interval="confidence"))
head(spcdur)
ggplot(data=spcdur, aes(x=range, y=duration, ymin=lwr, ymax=upr))+
  geom_point(aes(color=window))+
  scale_color_manual(values = c("black", "red")) + 
  geom_ribbon(alpha=.3, fill="red")+
  geom_line(data=spcdur, aes(x=range, y=fit), col="red")+
  ylab("Duration (My)")+
  xlab("Geographic range  - Log(Km2)")+
  annotate("text", label="A", size=20, x=6.5, y=20)+
  annotate("text", label=paste0("R-sq = ", round(rsq_emp, digits = 2)), size=5, x=6.5, y=17)+
  theme(legend.position="none") +
  ggtitle("Genera level")
spcdur=spcdur[,-8:-10]

ggsave(filename = "~/Desktop/duration_vs_range_gen.png", width = 10, units = "in", height = 5.3)

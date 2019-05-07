rm(list=ls())
source("~/Desktop/PyRateTools/pyrate_tools_pre_package.R")


spc=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/results/resam_sps.tsv", sep="\t", header = T)
dur=get.durations(spc, output = "stats", na.rm=T)
dur$lineage=as.character(dur$lineage)
########################
# TIME-WINDOW 
########################
#calculating lineages wich are not fully within our time-window

#read data
our_sp=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/data/OUR/OUR_sp.tsv", sep="\t", header=T)

#calculation
aux1=aggregate(x = our_sp$MinT, by=list(our_sp$Species), FUN = min)
colnames(aux1)=c("lineage", "min")
aux2=aggregate(x = our_sp$MaxT, by=list(our_sp$Species), FUN = max)
colnames(aux2)=c("lineage", "max")
raw_dur=merge.data.frame(aux1, aux2)
raw_dur$window="no"
#raw_dur$window[which(raw_dur$min>8 & raw_dur$max<39.5)]="yes" #conservative
raw_dur$window[which(raw_dur$min>6.5)]="yes" #wider window

#move to "dur" object:
window=vector()
for(i in 1:nrow(dur)){
  window[i]=raw_dur$window[which(raw_dur$lineage==dur$lineage[i])]
}
dur$window=window
class(dur$window)
dur$window=factor(x = dur$window, levels = c("no", "yes"))



bw=1
dat=dur
n=nrow(dat)
ids_weibull=sample(1:nrow(spc), size = 600)
ggplot()+
  geom_histogram(data=dat, aes(x=duration, y=(..count..)/(n*bw), fill=window), position="stack", binwidth = bw) + 
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
  annotate("text", x=20, y=.5, label="B", size=20)+
  ggtitle("Species - wider window") #Including window crossers

ggsave(filename = "~/Desktop/sampled_sps_weibulls.png", width = 10, units = "in", height = 5.3)



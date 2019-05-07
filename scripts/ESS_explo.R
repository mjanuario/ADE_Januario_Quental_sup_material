rm(list=ls())
library(cowplot)
library(reshape2)

t=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/results/ESS_spc.tsv", sep="\t", header = T, as.is=T)
aux=t[,c(2:12, 14:16)]
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
  ggtitle(label = "Species level - Conservative")
ggsave(filename = "~/Desktop/ESS_spc.png", width = 10, units = "in", height = 5.3)

##################################################################

t=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/results/ESS_sps.tsv", sep="\t", header = T, as.is=T)
aux=t[,c(2:12, 14:16)]
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

##################################################################

t=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/results/ESS_gen.tsv", sep="\t", header = T, as.is=T)
aux=t[,c(2:12, 14:16)]
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
  ggtitle(label = "Genera level")
ggsave(filename = "~/Desktop/ESS_gen.png", width = 10, units = "in", height = 5.3)

##################################################################

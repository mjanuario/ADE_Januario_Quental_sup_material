files=c("~/Downloads/ADE_Januario_Quental_supplementar_material/results/resam_gen.tsv",  "~/Downloads/ADE_Januario_Quental_supplementar_material/results/resam_sps.tsv", "~/Downloads/ADE_Januario_Quental_supplementar_material/results/resam_spc.tsv")

names=c("Genera", "Species - sensibility", "Species - conservative")

library(LaplacesDemon)
library(bbmle)
library(ggplot2)
library(cowplot)
library(TeachingDemos)
source("~/Desktop/PyRateTools/pyrate_tools_pre_package.R")


################################
# Weibull shape for all models:
################################

res=data.frame(Shape=NA, dataset=NA)
for(i in 1:3){
  t=read.table(files[i], sep="\t", header=T)
  aux=data.frame(Shape=t$w_shape, dataset=names[i]) 
  res=rbind(res, aux)
}
res=res[-1,]
ggplot(data=res, aes(x=dataset, y=Shape, fill=dataset, col=dataset))+
geom_violin()+
#scale_colour_manual(values = c("#d73027", "#4575b4", "#74add1"))+
scale_colour_manual(values = c("black", "grey50", "grey75"))+
#scale_fill_manual(values = c("#d73027", "#4575b4", "#74add1"))+
scale_fill_manual(values = c("black", "grey50", "grey75"))+
geom_abline(slope = 0, intercept = 1, linetype=2)+
theme(legend.position="none")
ggsave(filename = "~/Desktop/3_datasets_ADE_weibulls.png", width = 10, units = "in", height = 5.3)


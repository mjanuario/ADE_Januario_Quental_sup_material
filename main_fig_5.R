rm(list=ls())
library(cowplot)
library(patchwork)
library(fitdistrplus)
source("~/Desktop/PyRateTools/R/pyrate_tools_pre_package.R")

#genus richness
spps=read.table("~/Downloads/ADE_revised_general/sp.tsv", sep="\t", header = T)
n_spp_per_genus=table(gsub("_.*", "", unique(spps$Species))) #number of
#####################################################

###########################################
#ploting and identifying issues:
pcs=c(15,16,17)
cols=c("magenta", "gold1", "darkorange1")
dataset_names=c("G1MA", "G1NW", "G2MA")
###########################################

g1ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_gen_Inf_145/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

g1nw=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_gen_Inf_215/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

g2ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_gen_9_2/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

datasets=list(g1ma$mcmc, g1nw$mcmc, g2ma$mcmc)

#R-squared function:
rsq <- function (x, y) cor(x, y) ^ 2
rsq_emp=vector()

data=data.frame()
for(j in 1:3){
  aux=get.durations(datasets[[j]], na.rm = F)
  all_reps=!(is.na(aux$duration))
  durs=get.durations(datasets[[j]])
  durs$all_reps=all_reps
  durs$dataset=dataset_names[j]
  durs$rich=n_spp_per_genus[match(durs$lineage, names(n_spp_per_genus))]
  
  model=lm(duration~0+rich, data=durs, subset = durs$all_reps==TRUE)
  summary(model)
  
  rsq_emp=c(rsq_emp, rsq(x=durs$rich[durs$all_reps], y=durs$duration[durs$all_reps]))
  
  durs[,6:8]=predict(model, newdata=durs, interval="confidence")
  colnames(durs)[6:8]=c("fit", "lwr", "upr")
  
  durs$rich_alt=durs$rich+rnorm(sd = .1, n=nrow(durs))
  durs$rsq=rsq(x=durs$rich[durs$all_reps], y=durs$duration[durs$all_reps])
  
  data=rbind(data,durs)
}

head(data)


ggplot(data=data, aes(x=rich, y=duration, ymin=lwr, ymax=upr))+
  #geom_point(aes(color=window))+
  scale_x_continuous(breaks=1:10)+
  scale_shape_manual(values=c(1,19))+
  geom_jitter(aes(shape=all_reps), width = 0.25, alpha=.5)+
  scale_color_manual(values = c("black", "red")) + 
  geom_ribbon(alpha=.3, fill="black")+
  geom_line(aes(x=rich, y=fit), col="black")+
  ylab("Duration (My)")+
  theme(legend.position="none")+
  xlab("Genera richness")+
  geom_text(aes(x=3, y=21, label=paste0("R-squared = ", round(rsq, digits = 3))))+
  facet_wrap(~ dataset, scales="free_x")+
  ggtitle("Genera richness vs duration")

ggsave(filename = "~/Desktop/main_Fig_5.png", width = 10, units = "in", height = 5.3)

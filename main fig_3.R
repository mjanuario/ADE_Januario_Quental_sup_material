rm(list=ls())
library(cowplot)
library(patchwork)
library(fitdistrplus)
source("~/Desktop/PyRateTools/R/pyrate_tools_pre_package.R")

###########################################
#ploting and identifying issues:
pcs=c(0,1,2,15,16,17)
cols=c("saddlebrown", "purple", "springgreen4","magenta", "gold1", "darkorange1")
dataset_names=c("S1MA", "S1NW", "S2MA", "G1MA", "G1NW", "G2MA")
###########################################

s1ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_sp_Inf_14/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

s1nw=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_sp_395_16/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

s2ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_sp_125_65/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

g1ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_gen_Inf_145/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

g1nw=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_gen_Inf_215/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

g2ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_gen_9_2/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

datasets=list(s1ma$mcmc, s1nw$mcmc, s2ma$mcmc, g1ma$mcmc, g1nw$mcmc, g2ma$mcmc)



#function with run analyses:
run_analisis=function(mcmc){
  
  mcmc=mcmc[,apply(mcmc, 2, function(x){sum(is.na(x))<1})]
  aux=get.durations(mcmc)
  exp=fitdist(aux$duration, "exp")
  wei=fitdist(aux$duration, "weibull")
  
  res=vector()
  if(exp$aic-wei$aic >2){
    res=c("fitdistplus (best model = Weibull)")
  }else{
    res=c("fitdistplus (best model = Exponential)")
  }
  
  res=data.frame(best_dist=as.character(res),
                 shape=wei$estimate[1],
                 scale=wei$estimate[2],
                 rate=exp$estimate)
  return(res)
}

#running analyses:
stats=data.frame(matrix(NA, ncol = 6, nrow = 0))
colnames(stats)=c("best_dist", "shape", "scale", "rate", "model")
for(j in 1:length(datasets)){
  for(i in unique(datasets[[j]]$replica)){
    
    lines = datasets[[j]]$replica %in% i
    aux=run_analisis(datasets[[j]][lines,])
    aux$model="fitdistrplus"
    aux$rep=i
    aux$dtst=dataset_names[j]

    aux
    aux2=data.frame(best_dist="PyRate", 
        shape=median(datasets[[j]][lines,]$w_shape),
        scale=median(datasets[[j]][lines,]$w_scale_0),
        rate=NA,
        model="PyRate",
        rep=i,
        dtst=dataset_names[j], stringsAsFactors = F)
    
    stats=rbind(stats, aux, aux2)
  }
  print(j)
}

beg=rep(stats$shape[seq(from=1, to=nrow(stats), by=2)], each=2)
end=rep(stats$shape[seq(from=2, to=nrow(stats), by=2)], each=2)
dodges=position_jitter(width = .02, h=0, seed = 100)

head(stats)

aggregate(stats$best_dist, by=list(stats$model, stats$dtst), table)

aux=stats[stats$model=="fitdistrplus",]
stats$model[stats$model=="fitdistrplus"]="fitdistr"

stats$dtst=factor(stats$dtst, levels=c("S1MA", "S1NW", "S2MA", "G1MA", "G1NW", "G2MA"))

ggplot(data=stats, aes(x=model, y=shape))+
  #geom_boxplot(aes(col=model))+
  scale_shape_manual(values=c(3,5,3))+
  geom_abline(slope = 0, intercept = 1, linetype=2, alpha=.5, col="maroon", size=1.5)+
  scale_x_discrete()+
  geom_segment(aes(x=1, xend=2, y=beg, yend=end, group=model), alpha=.1)+
  geom_point(aes(col=best_dist, shape=best_dist), alpha=1, size=2.5, position = dodges)+
  facet_grid(~dtst)+
  #facet_wrap(~ dtst, ncol=3)+
  scale_color_manual(values=c("maroon", "black", "turquoise3"))+
  xlab("Parameter estimation")+
  theme(legend.position="bottom", legend.title = element_blank())+
  ylab("Shape Value")

ggsave(filename = "~/Desktop/Fig_3.png", width = 10, units = "in", height = 5.3)





####playgronund:

aa=data.frame(stats$shape[seq(1, nrow(stats), by=2)]-stats$shape[seq(2, nrow(stats), by=2)])
aa$dataset=stats$dtst[seq(1, nrow(stats), by=2)]
colnames(aa)=c("diff", "dtst")
head(aa)



datasets[[1]]=s1ma$mcmc
i=1

qs=data.frame()
for(i in 1:6){
  ids=which(colnames(datasets[[i]]) %in% c(paste0("q_", 0:10)))
  aux=aggregate(datasets[[i]][ids], by=list(datasets[[i]]$replica), FUN=median)
  colnames(aux)[1]="replica"
  aux$dtst=dataset_names[i]
  
  qs=rbind.fill(qs, aux)
}

dt=merge(aa, qs)

foo()


dt=melt(dt, id.vars=c("replica", "dtst"))

dt$kind="data1"#Preservation rate"
dt$kind[dt$variable=="diff"]="data2"#Shape diff"
head(dt)

mycolors <- c("data1"="blue", "data2"="red")


ggplot(dt, aes(x=dtst, y=value, col=kind)) +
  geom_boxplot() +
  scale_y_continuous(name="data1", sec.axis = sec_axis(~ .25*., name="data2")) +
  scale_color_manual(name="kind", values = mycolors) +
  theme(
    axis.title.y = element_text(color = mycolors["data1"]),
    axis.text.y = element_text(color = mycolors["data1"]),
    axis.title.y.right = element_text(color = mycolors["data2"]),
    axis.text.y.right = element_text(color = mycolors["data2"])
  )

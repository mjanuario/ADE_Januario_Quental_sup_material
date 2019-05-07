
files=c("~/Downloads/trials_ade/OUR_gen_99_ADE_gen_0099_G_ADEBD1-1_mcmc.log", "~/Downloads/trials_ade/OUR_sp_99_ADE_sps_0099_G_ADEBD1-1_mcmc.log", "~/Downloads/trials_ade/OUR_sp_99_ADE_spc_0099_G_ADEBD1-1_mcmc.log")
names=c("Genera", "Species - sensibility", "Species - conservative")

library(LaplacesDemon)
library(bbmle)
library(ggplot2)
library(cowplot)
library(TeachingDemos)
col_conv=function(x)sapply(strsplit(x, " "), function(x)return(rgb(x[1], x[2], x[3], maxColorValue=255)))
colo=adjustcolor(col_conv(c("255 0 0")), alpha.f = .1)

#colnames(t)
#apply(t, 2, ESS)[1:16]
#hist(t$w_shape)
#hist(t$w_scale_0)

for(j in 1:3){
t=read.table(files[j], sep="\t", header=T)
  
curve(dweibull(x, shape = t$w_shape[1], scale = t$w_scale_0[1]), from=0, to=25, col=colo, ylim=c(0,.6),  main=names[j], ylab="Probability", xlab="Duration", axes=F)
Axis(side=1, labels = T)
Axis(side=2, labels = T)

print(paste0(files[j]))
print("shape HDR 99% = ")
print(emp.hpd(t$w_shape, conf = .99))
print("shape median = ")
print(median(t$w_shape))

print("scale HDR 99% = ")
print(emp.hpd(t$w_scale_0, conf = .99))
print("scale median = ")
print(median(t$w_scale_0))

print("species mean duration HDR 99% = ")
print(emp.hpd(t$mean_longevity_0, conf = .99))
print("species mean duration median = ")
print(median(t$mean_longevity_0))


curve(dweibull(x, shape = 1, scale = median(t$w_scale_0)), from=0, to=25, col="blue", add=T, lwd=2, lty=2)

for(i in 2:nrow(t)){
  curve(dweibull(x, shape = t$w_shape[i], scale = t$w_scale_0[i]), from=0, to=25, col=colo, add=T)
}

not_times=c(
which(colnames(t)=="it"),
which(colnames(t)=="posterior"),                           
which(colnames(t)=="prior"),                               
which(colnames(t)=="PP_lik"),                              
which(colnames(t)=="BD_lik"),                              
which(colnames(t)=="q_0"),                                 
which(colnames(t)=="q_1"),                                 
which(colnames(t)=="q_2"),                                 
which(colnames(t)=="q_3"),                                 
which(colnames(t)=="q_4"),                                 
which(colnames(t)=="alpha"),                               
which(colnames(t)=="root_age"),                            
which(colnames(t)=="death_age"),                           
which(colnames(t)=="w_shape"),                             
which(colnames(t)=="w_scale_0"),                           
which(colnames(t)=="mean_longevity_0"),                    
which(colnames(t)=="tot_length")) 

t=t[,-not_times];rm(not_times)
durs=vector()
laps=(ncol(t)/2)
for(i in 1:laps){
  durs=c(durs, t[,i]-t[,i+laps])
}

hist(durs, breaks = 500, probability = T, add=T)

#adjust a weibull without the PyRate ADE model:
#log-lik function:
nllweibull = function(escala, forma, x=durs){
  -sum(dweibull(x, shape=forma, scale=escala, log=TRUE))
}
#Weibull-fit with mle2:
parag.wei = mle2(nllweibull, start=list(escala=20, forma=1))


#ploting:
#curve(dweibull(x, shape = parag.wei@coef[2], scale = parag.wei@coef[1]), from=0, to=25, col="darkgreen", add=T, lwd=2, lty=2)
#text(x = 10, y=.4, paste0("MLE shap =", parag.wei@coef[2]))

}

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

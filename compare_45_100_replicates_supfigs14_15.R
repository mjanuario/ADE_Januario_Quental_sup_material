source("~/Desktop/PyRateTools/R/pyrate_tools_pre_package.R")

gen=merge.pyrate(folder = "~/Downloads/ADE_revised_results/ADE_revised_gen_Inf_145/pyrate_mcmc_logs/", log = "mcmc", print_progress = T)
length(unique(gen$mcmc$replica))

gen2=merge.pyrate(folder = "~/Downloads/ADE_revised_results/ADE_revised_gen_Inf_145/pyrate_mcmc_logs_2/", log = "mcmc", print_progress = T)
gen2$mcmc$replica=gen2$mcmc$replica+45

gen=plyr::rbind.fill(gen$mcmc, gen2$mcmc)

x=apply(gen, 2, function(x){sum(is.na(x))})
ids=which(x>0)
gen=gen[,-c(ids)] #remove species which are absent in at least one replicate

ref=get.durations(gen, gen=T)
hist(ref$duration, breaks=30)

size=100 #n de sorteios

reff=ref

wei=data.frame(matrix(nrow=100, ncol = 6))
colnames(wei)=c("sh0.05", "shmed", "sh0.95", "sc0.05", "scmed", "sc0.95")



plot(NA, xlim=c(0, 100), ylim = c(.7, 1.5))
points(x=0, y=median(gen$w_shape, na.rm = T), col="red")
segments(x0 = 0, x1=0, y0=quantile(gen$w_shape, probs = .95, na.rm = T),y1=quantile(gen$w_shape, probs = .05, na.rm = T), col="red")

for(s in 1:size){
  ids=sample(1:100, size=45)
  
  aux=get.durations(gen[gen$replica %in% ids,], gen = T)
  aux2=merge(x=ref, y=aux, by = "lineage")
  aux2$diff= aux2$duration.x-aux2$duration.y
  aux2=aux2[,c(1,4)]
  colnames(aux2)=c("lineage", paste0("s_", s))
  
  wei[s,1]=quantile(gen$w_shape[gen$replica %in% ids], probs = .05, na.rm = T)
  wei[s,2]=quantile(gen$w_shape[gen$replica %in% ids], probs = .5, na.rm = T)
  wei[s,3]=quantile(gen$w_shape[gen$replica %in% ids], probs = .95, na.rm = T)
  
  
  wei[s,4]=quantile(gen$w_scale_0[gen$replica %in% ids], probs = .05, na.rm = T)
  wei[s,5]=quantile(gen$w_scale_0[gen$replica %in% ids], probs = .5, na.rm = T)
  wei[s,6]=quantile(gen$w_scale_0[gen$replica %in% ids], probs = .95, na.rm = T)
  
  points(x=s, y=wei[s,2])
  segments(x0 = s, x1=s, y0=wei[s,1], y1=wei[s,3])
  
  reff=merge(reff, aux2, by="lineage")
  print(paste0("made ", (s/size)*100, " of the sampling"))
}

makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}


png("~/Desktop/duration_diffs.png", width = 721, height = 371)
hist(apply(reff[,-c(1,2)], 2, median), col=makeTransparent("black", 75), xlim=c(-0.08, 0.08), xlab = "Differences in duration (my)", main="", cex.lab=1.35)
hist(apply(reff[,-c(1,2)], 2, mean), breaks=5, col=makeTransparent("red", 75), add=T)
dev.off()

######3

sum(apply(reff[,-c(1,2)], 2, mean)>.05)

png("~/Desktop/pars_diffs.png", width = 721, height = 742)

par(mfrow=c(2,1))

plot(NA, xlim=c(0, 100), ylim = c(.7, 1.5), ylab="Shape value", xlab="sample", frame.plot = F, cex.lab=1.35)
points(x=0, y=median(gen$w_shape, na.rm = T), col="red", main="Shape parameter")
segments(x0 = 0, x1=0, y0=quantile(gen$w_shape, probs = .95, na.rm = T),y1=quantile(gen$w_shape, probs = .05, na.rm = T), col="red")
for(s in 1:nrow(wei)){
  points(x=s, y=wei[s,2])
  segments(x0 = s, x1=s, y0=wei[s,1], y1=wei[s,3])
}
text(x = 10, y = 1.3, labels = "A", cex=3)
#dev.off()

#png("~/Desktop/scale_diffs.png", width = 721, height = 371)
plot(NA, xlim=c(0, 100), ylim = c(4.4, 8.5), ylab="Scale value", xlab="sample", frame.plot = F, cex.lab=1.35, main="Scale parameter")
points(x=0, y=median(gen$w_scale_0, na.rm = T), col="red")
segments(x0 = 0, x1=0, y0=quantile(gen$w_scale_0, probs = .95, na.rm = T),y1=quantile(gen$w_scale_0, probs = .05, na.rm = T), col="red")
for(s in 1:nrow(wei)){
  points(x=s, y=wei[s,5])
  segments(x0 = s, x1=s, y0=wei[s,4], y1=wei[s,6])
}
text(x = 10, y = 7.5, labels = "B", cex=3)
dev.off()

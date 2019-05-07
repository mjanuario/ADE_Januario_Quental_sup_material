require(plyr)
library(LaplacesDemon)

lista_arq=paste0("/Volumes/Macintosh HD 2/logs_sps/sps_new/OUR_sp_", 1:100, "_ADE_sps_", sprintf("%0.4d", 1:100), "_G_ADEBD1-1_mcmc.log")

tabs=lapply(as.list(lista_arq), function(x){tryCatch(read.table(x, sep = "\t", header=T), error=function(y){return(NA)})})


mcmc=data.frame()
ESS=data.frame()
opa=unlist(lapply(tabs, function(x){return(dim(x)[1])}))

for(i in 1:100){
  if(class(tabs[[i]])=="data.frame"){
    #if(nrow(tabs[[i]])==1999){
    t=tabs[[i]]
    ess=as.data.frame(t(as.data.frame(apply(t, 2, ESS))))
    ess$rep=i
    ess$nrow=nrow(t)
    t=t[sample(1:nrow(t), size = 200),]
    mcmc=rbind.fill(mcmc, t)
    ESS=rbind.fill(ESS, ess)
    #}
  }
}

dim(mcmc)
dim(ESS)
write.table(mcmc, "/Users/mjanuario/Desktop/resam_sps.tsv", sep = "\t", row.names = F, col.names = T)
write.table(ESS, "/Users/mjanuario/Desktop/ESS_sps.tsv", sep = "\t", row.names = F, col.names = T)



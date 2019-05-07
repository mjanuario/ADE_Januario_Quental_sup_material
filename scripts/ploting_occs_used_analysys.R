png(filename = "~/Desktop/ADE_sp_cons_occs.png", width = 2100, height = 1100)


ours=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/data/OUR/OUR_sp.tsv", sep="\t", header=T)
dim(ours)
head(ours)
par(cex=1.5)
plot(x=NA, y=NA, xlim = c(60,0), ylim=c(0, 7900), main = "Tax 1 sp - Conservative", xlab="Mya", ylab="Occurrences", axes=F)
Axis(side=1, labels=T)
cols=rep("#00441b", times=nrow(ours))

#fazendo tabela por especie
tab=data.frame(sp=sort(unique(ours$Species)), min=tapply(ours$MinT, ours$Species, min), max=tapply(ours$MaxT, ours$Species, max))


exclud=as.character(tab$sp[which(tab$min<8)])
exclud2=as.character(tab$sp[which(tab$max>39.5)])
excluded=unique(as.character(c(exclud, exclud2)))

for(i in 1:length(excluded)){
  cols[which(ours$Species==excluded[i])]="#a1d99b"
}


for(i in 1:7900){
  segments(x0 = ours$MaxT[i], x1=ours$MinT[i], y0=i, y1=i, col=cols[i])
}

rect(xleft = c(39.5, 39.5), xright = c(6.5,6.5), ytop =8400, ybottom = 7900, lwd = 3, border = "white", col= "#00B454")
text(x = 27, y=8050, labels = "Analysis Time window", col="white", cex = 1)

#data:
table(cols) #occs
data.frame(incl=length(unique(ours$Species))-length(excluded), excl=length(excluded) )


dev.off()
#######################################################################
#######################################################################
#######################################################################

png(filename = "~/Desktop/ADE_sp_sens_occs.png", width = 2100, height = 1100)


ours=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/data/OUR/OUR_sp.tsv", sep="\t", header=T)
dim(ours)
head(ours)
par(cex=1.5)
plot(x=NA, y=NA, xlim = c(60,0), ylim=c(0, 7900), main = "Tax 1 sp - Sensibility", xlab="Mya", ylab="Occurrences", axes=F)
Axis(side=1, labels=T)
cols=rep("#00441b", times=nrow(ours))

#fazendo tabela por especie
tab=data.frame(sp=sort(unique(ours$Species)), min=tapply(ours$MinT, ours$Species, min), max=tapply(ours$MaxT, ours$Species, max))


exclud=as.character(tab$sp[which(tab$min<6.5)])
exclud2=as.character(tab$sp[which(tab$max>1000)])
excluded=unique(as.character(c(exclud, exclud2)))

for(i in 1:length(excluded)){
  cols[which(ours$Species==excluded[i])]="#a1d99b"
}


for(i in 1:7900){
  segments(x0 = ours$MaxT[i], x1=ours$MinT[i], y0=i, y1=i, col=cols[i])
}

rect(xleft = c(100, 100), xright = c(6.5,6.5), ytop =8400, ybottom = 7900, lwd = 3, border = "white", col= "#00B454")
text(x = 27, y=8050, labels = "Analysis Time window", col="white", cex = 1)

#data:
table(cols) #occs
data.frame(incl=length(unique(ours$Species))-length(excluded), excl=length(excluded) )


dev.off()
#######################################################################
#######################################################################
#######################################################################

png(filename = "~/Desktop/ADE_gn_occs.png", width = 2100, height = 1100)


ours=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/data/OUR/OUR_gen.tsv", sep="\t", header=T)
dim(ours)
head(ours)
par(cex=1.5)
plot(x=NA, y=NA, xlim = c(60,0), ylim=c(0, 7900), main = "Tax 1 sp - Sensibility", xlab="Mya", ylab="Occurrences", axes=F)
Axis(side=1, labels=T)
cols=rep("#00441b", times=nrow(ours))

#fazendo tabela por especie
tab=data.frame(sp=sort(unique(ours$Species)), min=tapply(ours$MinT, ours$Species, min), max=tapply(ours$MaxT, ours$Species, max))


exclud=as.character(tab$sp[which(tab$min<10.5)])
exclud2=as.character(tab$sp[which(tab$max>1000)])
excluded=unique(as.character(c(exclud, exclud2)))

for(i in 1:length(excluded)){
  cols[which(ours$Species==excluded[i])]="#a1d99b"
}


for(i in 1:7900){
  segments(x0 = ours$MaxT[i], x1=ours$MinT[i], y0=i, y1=i, col=cols[i])
}

rect(xleft = c(100, 100), xright = c(6.5,6.5), ytop =8400, ybottom = 7900, lwd = 3, border = "white", col= "#00B454")
text(x = 27, y=8050, labels = "Analysis Time window", col="white", cex = 1)

#data:
table(cols) #occs
data.frame(incl=length(unique(ours$Species))-length(excluded), excl=length(excluded) )


dev.off()

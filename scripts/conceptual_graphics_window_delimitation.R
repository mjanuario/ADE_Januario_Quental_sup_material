png(filename = "~/Desktop/ADE_concept_sp.png", width = 850, height = 400)
source("~/Desktop/anal_mestrado/scripts_dados/pyrate_tools.R")

raw.data2=read.table("~/Downloads/resampling_results/ex_8.tsv", header=T, sep="\t")
raw.data2=raw.data2[,-ncol(raw.data2)] #removing the replcia column
proc.data=make.RTT.pyrate(data=raw.data2, max.time=100, resolution=.1, min.time=0)
hpds2=make.HPD.RTT(proc.data, prob = .95, max = 2)


####################################     PLOTING HPDs
plot.pyrate.HPD(HPD=list(hpds2), xlim=c(57,0), ylim=c(-0.05,1.45), col=c("#00B454"), alpha=c(.5,.5, .5), lwd=4, ylab="Rate", xlab="mya", qShift=c(56, 33.9, 23.03, 15.97, 11.65, 5.33, 2.58, 1.8),  qShift_alpha=.2, main="Species level analysis")
add_shifts_HDR(raw.data2, hpds2, col = "#00B454", y = -.03)

rect(xleft = c(60, 60), xright = c(6.5,6.5), ytop =1.3, ybottom = 1.4, lwd = 3, border = "white", col= "#00B454")
text(x = 30, y=1.35, labels = "Sensibility analysis", col="white", cex = .8)

rect(xleft = c(39.5, 39.5), xright = c(8,8), ytop =1.2, ybottom = 1.3, lwd = 3, border = "white", col= "#00B454")
text(x = 30, y=1.25, labels = "Conservative analysis", col="white", cex = .8)
dev.off()

png(filename = "~/Desktop/ADE_concept_gen.png", width = 850, height = 400)
source("~/Desktop/anal_mestrado/scripts_dados/pyrate_tools.R")

raw.data2=read.table("~/Downloads/resampling_results/ex_20.tsv", header=T, sep="\t")
raw.data2=raw.data2[,-ncol(raw.data2)] #removing the replcia column
proc.data=make.RTT.pyrate(data=raw.data2, max.time=100, resolution=.1, min.time=0)
hpds2=make.HPD.RTT(proc.data, prob = .95, max = 2)


####################################     PLOTING HPDs
plot.pyrate.HPD(HPD=list(hpds2), xlim=c(57,0), ylim=c(-0.05,1.45), col=c("#00B454"), alpha=c(.5,.5, .5), lwd=4, ylab="Rate", xlab="mya", qShift=c(56, 33.9, 23.03, 15.97, 11.65, 5.33, 2.58, 1.8),  qShift_alpha=.2, main="Genera level analysis")
add_shifts_HDR(raw.data2, hpds2, col = "#00B454", y = -.03)

rect(xleft = c(60, 60), xright = c(10.5,10.5), ytop =1.3, ybottom = 1.4, lwd = 3, border = "white", col= "#00B454")
text(x = 30, y=1.35, labels = "Analysis", col="white", cex = .8)


dev.off()
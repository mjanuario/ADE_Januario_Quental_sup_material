source("~/Desktop/PyRateTools/R/pyrate_tools_pre_package.R")

#species data
sp=merge.pyrate(folder = "~/Downloads/ADE_revised_results/sp_preana/", log="rates", print_progress = T, prunning = 100, burnin = 100000)#, prunning = 5000)

#genus data:
gen=merge.pyrate(folder = "/Volumes/mjanuario/pesquisa/mestrado/ADE_final/revisao/ADE_revised_gen_preanalise/pyrate_mcmc_logs/", log="rates", print_progress = T, prunning = 100, burnin = 100000) #anterior: prun=100, burn=100000


pdf("~/Desktop/shifts_both_levels.pdf", width = 20, height = 20)

par(mar=c(4,5,1,1), mfrow=c(2,1), cex=2)

plot.shifts(sp$ex_rates,col=c("red"), max_time = 50, min_time=0, resolution = .1, alpha = .5, main="Species level", ylim=c(0,0.1), xlim=c(43,0))

plot.shifts(sp$sp,col=c("blue"), max_time = 50, min_time=0, resolution = .1, alpha = .5, add=T)


add.shifts.HDR(sp$ex_rates, hpd2, y=0.06, col=c("red"), max_time = 50, min_time=0, resolution = .1)
add.shifts.HDR(sp$sp_rates, hpd2, y=0.065, col=c("blue"), max_time = 50, min_time = 0, resolution = .1)

text(y=0.045, x=33, labels = "high significance shift", cex=.5)
text(y=0.01, x=33, labels = "low significance shift", cex=.5)

rect(xleft = c(80, 80), xright = c(14,14), ytop = 0.08, ybottom = 0.07, lwd = 3, border = "white", col= "saddlebrown")
text(x = 27, y=0.075, labels = "S1MA", col="white", cex = 1)

rect(xleft = c(39.5, 39.5), xright = c(16,16), ytop = 0.09, ybottom = 0.08, lwd = 3, border = "white", col= "purple")
text(x = 27, y=0.085, labels = "S1NA", col="white", cex = 1)
#points(y=-0.075, x=15.5, col=cols[2], pch=pcs[2], lwd=4)

rect(xleft = c(12.5, 12.5), xright = c(6.5,6.5), ytop = 0.08, ybottom = 0.07, lwd = 3, border = "white", col= "springgreen4")
text(x = 9.5, y=0.075, labels = "S2MA", col="white", cex = 0.9)

abline(v=21.5, col="darkgoldenrod1", lty=2, lwd=5)

plot.shifts(gen$ex_rates,col=c("red"), max_time = 50, min_time=0, resolution = .1, alpha = .5, main="Genus level", ylim=c(0,0.1), xlim=c(43,0))

text(y=0.05, x=33, labels = "high significance shift", cex=0.5)
text(y=0.01, x=33, labels = "low significance shift", cex=.5)


add.shifts.HDR(gen$ex_rates, hpd2, y=0.055, col=c("red"), max_time = 50, min_time = 0, resolution = .1)

rect(xleft = c(80, 80), xright = c(14.5,14.5), ytop = 0.07, ybottom = 0.06, lwd = 3, border = "white", col= "magenta")
text(x = 27, y=0.065, labels = "G1MA", col="white", cex = 1)
#points(y=-0.025, x=14, col=cols[4], pch=pcs[4])

rect(xleft = c(80, 80), xright = c(21.5,21.5), ytop = 0.07, ybottom = 0.08, lwd = 3, border = "white", col= "darkgoldenrod1")
text(x = 37, y=0.075, labels = "G1NW", col="white", cex = 1)
#points(y=-0.075, x=21.5, col=cols[5], pch=pcs[5])

rect(xleft = c(9, 9), xright = c(2,2), ytop = 0.07, ybottom = 0.06, lwd = 3, border = "white", col= "darkorange1")
text(x = 5.5, y=0.065, labels = "G2MA", col="white", cex = 1)


abline(v=21.5, col="darkgoldenrod1", lty=2, lwd=5)
dev.off()

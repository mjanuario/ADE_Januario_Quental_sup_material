rm(list=ls())

names=c("spp regime 1 \n main analysis", "spp regime 1 \n narrow window",  "spp regime 2 \n main analisis", #"spp regime 2 \n narrow window",
        "gen regime 1 \n main analysis", "gen regime 1 \n narrow window",  "gen regime 2 \n main analisis")
namesw=c("S1MA", "S1NW", "S2MA", "G1MA", "G1NW", "G2MA")
pcs=c(0,1,2,15,16,17)
cols=c("saddlebrown", "purple", "springgreen4",# "turquoise3",
       "magenta", "gold1", "darkorange1")


setwd("~/Desktop")
source("~/Desktop/PyRateTools/R/pyrate_tools_pre_package.R")

#species data
sp=merge.pyrate(folder = "~/Downloads/ADE_revised_results/sp_preana/", log="rates", print_progress = T, prunning = 100, burnin = 100000)#, prunning = 5000)

#genus data:
gen=merge.pyrate(folder = "/Volumes/mjanuario/pesquisa/mestrado/ADE_final/revisao/ADE_revised_gen_preanalise/pyrate_mcmc_logs/", log="rates", print_progress = T, prunning = 100, burnin = 100000) #anterior: prun=100, burn=100000


#ploting:


pdf("~/Desktop/sp_cons_dataset.pdf", width = 20, height = 25)

rates=make.RTT(sp$ex_rates, max_time = 58, min_time = 0, resolution = 0.1)
rates2=make.RTT(sp$sp_rates, max_time = 65.2, min_time = 0, resolution = 0.1)
hpd=make.HPD.RTT(rates, density = 0.95, max = 2)
hpd2=make.HPD.RTT(rates2, density = 0.95, max = 2)

#plot.shifts(sp$ex_rates, col="red", max_time = max(hpd2$mya), min_time = 0, resolution = 1, plot = T, xlim=c(43,0))

par(mar=c(4,5,1,1), mfrow=c(3,1), cex=2) #par(mfrow=c(2,1))

plot.pyrate(HPD=list(hpd, hpd2), c("red", "blue"), xlim = c(43,0), ylim=c(-0.12,0.9), alpha = c(0.5, 0.5), main="", qShift = c(1.8, 2.58, 5.333, 11.65, 15.97, 23.03, 33.9, 56.0 ), xlab = "Time (Mya)")

#add.shifts.HDR(sp$ex_rates, hpd2, y=0.05, col=c("red"))
add.shifts.HDR(sp$ex_rates, hpd2, y=0.05, col=c("red"), max_time = 50, min_time=0)
add.shifts.HDR(sp$sp_rates, hpd2, y=0.02, col=c("blue"), max_time = 50)

#species windows:
text(x = 40, y=0.75, labels = "A",cex = 5)

rect(xleft = c(80, 80), xright = c(14,14), ytop = 0, ybottom = -0.075, lwd = 3, border = "white", col= "saddlebrown")
text(x = 27, y=-0.0375, labels = "S1MA", col="white", cex = 1)
#points(y=-0.025, x=13.5, col=cols[1], pch=pcs[1], lwd=4)

rect(xleft = c(39.5, 39.5), xright = c(16,16), ytop = -0.075, ybottom = -0.15, lwd = 3, border = "white", col= "purple")
text(x = 27, y=-0.1125, labels = "S1NA", col="white", cex = 1)
#points(y=-0.075, x=15.5, col=cols[2], pch=pcs[2], lwd=4)

rect(xleft = c(12.5, 12.5), xright = c(6.5,6.5), ytop = 0, ybottom = -0.075, lwd = 3, border = "white", col= "springgreen4")
text(x = 9.5, y=-0.0375, labels = "S2MA", col="white", cex = 0.9)
#points(y=-0.025, x=5.5, col=cols[3], pch=pcs[3], lwd=4)

#rect(xleft = c(11, 11), xright = c(8,8), ytop = -0.1, ybottom = -0.2, lwd = 3, border = "white", col= "turquoise3")
#text(x = 9.5, y=-0.15, labels = "Reg. 2 \n narrow \n window", col="white", cex = .75)

abline(v=21.5, col="darkgoldenrod1", lty=2, lwd=5)


rates=make.RTT(gen$ex_rates, max_time = 58, min_time = 0, resolution = 0.1)
rates2=make.RTT(gen$sp_rates, max_time = 65.2, min_time = 0, resolution = 0.1)
hpd3=make.HPD.RTT(rates, density = 0.95, max = 2)
hpd4=make.HPD.RTT(rates2, density = 0.95, max = 2)


plot.pyrate(HPD=list(hpd3), c("red", "blue"), xlim = c(43,0), ylim=c(-0.1,0.85), alpha = c(0.5, 0.5), main="", qShift = c(1.8, 2.58, 5.333, 11.65, 15.97, 23.03, 33.9, 56.0 ), xlab = "Time (Mya)")
add.shifts.HDR(gen$ex_rates, hpd2, y=0.05, col=c("red"), max_time = 50, min_time = 0)
#add.shifts.HDR(gen$sp_rates, hpd2, y=0.02, col=c("blue"), max_time = 50, min_time = 0)


#genus windows:
text(x = 40, y=0.75, labels = "B",cex = 5)

rect(xleft = c(80, 80), xright = c(14.5,14.5), ytop = 0, ybottom = -0.075, lwd = 3, border = "white", col= "magenta")
text(x = 27, y=-0.0375, labels = "G1MA", col="white", cex = 1)
#points(y=-0.025, x=14, col=cols[4], pch=pcs[4])

rect(xleft = c(80, 80), xright = c(21.5,21.5), ytop = -0.075, ybottom = -0.15, lwd = 3, border = "white", col= "darkgoldenrod1")
text(x = 37, y=-0.1125, labels = "G1NW", col="white", cex = 1)
#points(y=-0.075, x=21.5, col=cols[5], pch=pcs[5])

rect(xleft = c(9, 9), xright = c(2.1,2.1), ytop = 0, ybottom = -0.075, lwd = 3, border = "white", col= "darkorange1")
text(x = 6, y=-0.0375, labels = "G2MA", col="white", cex = 1)
#points(y=-0.025, x=1.5, col=cols[6], pch=pcs[6])

abline(v=21.5, col="darkgoldenrod1", lty=2, lwd=5)

#plot.shifts(gen$ex_rates, max_time = 67,resolution = 0.1, min_time = 0, xlim = c(67,0), col = "red", plot = T, ylim=c(0, 0.1))

#rm(sp)
s1ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_sp_Inf_14/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

s1nw=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_sp_395_16/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

s2ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_sp_125_65/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

g1ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_gen_Inf_145/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

g1nw=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_gen_Inf_215/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")

g2ma=merge.pyrate("~/Downloads/ADE_revised_results/ADE_revised_gen_9_2/pyrate_mcmc_logs/", print_progress = T, log = "mcmc")



library(vioplot)
dt=data.frame(S1MA=s1ma$mcmc$w_shape,
              S1NW=s1nw$mcmc$w_shape,
              S2MA=s2ma$mcmc$w_shape,
              G1MA=g1ma$mcmc$w_shape,
              G1NW=g1nw$mcmc$w_shape,
              G2MA=g2ma$mcmc$w_shape)
head(dt)


hpd_front=apply(dt, 2, emp.hpd)


vioplot(dt, col=cols,ylab = "Shape value", border = "black", colMed = "white", lineCol = NA, rectCol = NA, ylim=c(0,3), xlab="Dataset")
text(x=1, y=2.4, labels = "C", cex = 4)
abline(h=1, lty=1, col="red", lwd=2)
segments(x0 = 1:6, x1 = 1:6, y0=hpd_front[1,], y1=hpd_front[2,], col="white", lwd=2)
#points(x=1:6, y=apply(dt, 2, median), col=c("white","white","white","black","black","black"), pch=4)

dev.off()


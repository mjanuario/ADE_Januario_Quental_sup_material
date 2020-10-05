png(filename = "~/Desktop/ext_behavior.png", width = 2100, height = 1100)

par(mfrow=c(1,2),cex=3)
plot(NA, ylim=c(0, .5), xlim=c(0, 10), ylab="Expected distribution of lineages", xlab="Duration (My)")
curve(dweibull(x, shape = 1, scale = 3), add=T, lwd=5)
curve(dweibull(x, shape = .5, scale = 3), add=T, lwd=5, col="grey75", lty=2)
curve(dweibull(x, shape = 1.45, scale = 3), add=T, lwd=5, col="grey75")
text(x=9.3, y=.47, labels = "A", cex = 2)

scale=3
dur=seq(from=0, to=10, by=.1)
plot(NA, ylim=c(0, .8), xlim=c(0, 10), ylab="Extinction rate", xlab="Lineage age (My)")

shape=1
eP=(shape/scale)*((dur/scale)^(shape-1)) #follwoing eq 1 Hagen et al 2018
lines(x = dur, y=eP, lwd=5)

shape=.5
eP=(shape/scale)*((dur/scale)^(shape-1))
lines(x=dur, y=eP, lwd=5, col="grey75", lty=2 )

shape=1.45
eP=(shape/scale)*((dur/scale)^(shape-1))
lines(x=dur, y=eP, lwd=5, col="grey75")

text(x=9.3, y=.75, labels = "B", cex = 2)

dev.off()


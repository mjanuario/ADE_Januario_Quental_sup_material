
#age vector
max_age=6
ages=seq(0, max_age, by=.001)


#different preservation rates
q=c(.1, .5, 1, 5, 10)
colfunc <- colorRampPalette(c("#b2182b", "#2166ac"))
cols=colfunc(length(q))

cols=c("#B00400", "#FF0F08", "#FFC908", "#55ACC9", "#03536E")

png("~/Desktop/Hagen_eq7.png", width = 1442, height = 742)
#pdf(file = "~/Desktop/Hagen_eq7.pdf", width = 14, height = 7.4)
par(mfrow=c(2,2), cex.lab=1.5, cex.axis=2)

#Preservation model
ymax=1
plot(NA, xlim=c(0,max_age), ylim=c(0, ymax), ylab="P sampling at least one occ.", xlab="Lineage age (My)", yaxt="n")

for(i in 1:length(q)){
  lines(x=ages, y=1-exp(-q[i]*ages), col=cols[i], lwd=3) # (Eq5) in Hagen et al2018  
}
lines(x=ages, y=rep(1, times=length(ages)), lwd=4, lty=4)
text(x = 5, y=ymax*.8, labels = "A", cex = 4)



#Expectated under negADE
ymax=0.65
plot(NA, xlim=c(0,max_age), ylim=c(0, ymax), ylab="Density", xlab="Lineage age (My)", yaxt="n")

#weibull density
wei=dweibull(ages, 0.5, 3)
for(i in 1:length(q)){
  lines(x=ages, y=wei*(1-exp(-q[i]*ages)), col=cols[i], lwd=3) # (Eq5) in Hagen et al2018  
}
lines(x=ages, y=wei, lwd=4, lty=4)
text(x = 5, y=ymax*.8, labels = "B", cex = 4)

# legend(1, 0.4, 
#        legend=c("Unbiased record", paste0("q = ", as.character(q))),
#       col=c("black", cols), lty=c(4, rep(1, times=length(q))),
#       title = "Preservation rate (q) \n (occurrences per lineage per My)",
#       lwd=4, ncol = 2, box.lty=0, bg=NA, text.width = 3)


#Expectated under AIE
ymax=0.35
plot(NA, xlim=c(0,max_age), ylim=c(0, ymax), ylab="Density", xlab="Lineage age (My)", yaxt="n")

#weibull density
wei=dweibull(ages, 1, 3)
for(i in 1:length(q)){
  lines(x=ages, y=wei*(1-exp(-q[i]*ages)), col=cols[i], lwd=3) # (Eq5) in Hagen et al2018  
}
lines(x=ages, y=wei, lwd=4, lty=4)
text(x = 5, y=ymax*.8, labels = "C", cex = 4)

#Expectated under posADE
ymax=0.3
plot(NA, xlim=c(0,max_age), ylim=c(0, ymax), ylab="Density", xlab="Lineage age (My)", yaxt="n")

#weibull density
wei=dweibull(ages, 1.5, 3)
for(i in 1:length(q)){
  lines(x=ages, y=wei*(1-exp(-q[i]*ages)), col=cols[i], lwd=3) # (Eq5) in Hagen et al2018  
}
lines(x=ages, y=wei, lwd=4, lty=4)
text(x = 5, y=ymax*.8, labels = "D", cex = 4)
dev.off()
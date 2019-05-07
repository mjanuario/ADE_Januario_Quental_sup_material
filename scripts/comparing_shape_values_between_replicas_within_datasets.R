library(TeachingDemos)
cv=function(x){
  return(((sd(x)/mean(x))))
}

t=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/results/resam_spc.tsv", header=T, sep="\t", as.is = T)
t$replica=rep(1:100, each=200)

emp.hpd(t$w_scale_0, conf = .95)
median(t$w_scale_0)
ggplot(data=t, aes(y=t$BD_lik, x=it, color=replica))+geom_line()

cv(aggregate(t$w_scale_0, by=list(t$replica), FUN=mean)[,2])

ggplot(data=t, aes(y=w_shape, x=reorder(replica, w_shape, FUN=median), group=replica))+
  geom_boxplot()+
  xlab("Replicas")+
  #theme(legend.position="bottom")+
  ylab("Shape value") +
  ggtitle(label = "Species level - Conservative")+
  geom_abline(slope = 0, intercept = 1, linetype=2, size=2, color="red", alpha=.4)+
theme(axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
ggsave(filename = "~/Desktop/shape_per_replica_spc.png", width = 10, units = "in", height = 5.3)

##########################################
##########################################

t=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/results/resam_sps.tsv", header=T, sep="\t", as.is = T)
t$replica=rep(1:100, each=200)

emp.hpd(t$w_scale_0, conf = .95)
median(t$w_scale_0)
ggplot(data=t, aes(y=t$BD_lik, x=it, color=replica))+geom_line()

cv(aggregate(t$w_scale_0, by=list(t$replica), FUN=mean)[,2])

ggplot(data=t, aes(y=w_shape, x=reorder(replica, w_shape, FUN=median), group=replica))+
  geom_boxplot()+
  xlab("Replicas")+
  #theme(legend.position="bottom")+
  ylab("Shape value") +
  ggtitle(label = "Species level - Wider window")+
  geom_abline(slope = 0, intercept = 1, linetype=2, size=2, color="red", alpha=.4)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave(filename = "~/Desktop/shape_per_replica_sps.png", width = 10, units = "in", height = 5.3)

##########################################
##########################################

t=read.table("~/Downloads/ADE_Januario_Quental_supplementar_material/results/resam_gen.tsv", header=T, sep="\t", as.is = T)
t$replica=rep(1:100, each=200)

emp.hpd(t$w_scale_0, conf = .95)
median(t$w_scale_0)
ggplot(data=t, aes(y=t$BD_lik, x=it, color=replica))+geom_line()

cv(aggregate(t$w_scale_0, by=list(t$replica), FUN=mean)[,2])

ggplot(data=t, aes(y=w_shape, x=reorder(replica, w_shape, FUN=median), group=replica))+
  geom_boxplot()+
  xlab("Replicas")+
  #theme(legend.position="bottom")+
  ylab("Shape value") +
  ggtitle(label = "Genera level")+
  geom_abline(slope = 0, intercept = 1, linetype=2, size=2, color="red", alpha=.4)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave(filename = "~/Desktop/shape_per_replica_gen.png", width = 10, units = "in", height = 5.3)

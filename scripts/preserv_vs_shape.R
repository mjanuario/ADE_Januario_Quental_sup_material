mcmc=read.table("~/Downloads/aaaa/resam_gen.tsv", sep="\t", header = T)
rsq <- function (x, y) cor(x, y) ^ 2


ggplot(data=mcmc, aes(x=q_0, y=w_shape))+geom_hline(yintercept = 1,linetype=2)+geom_point(alpha=.1, color="red")+ylab("Shape")+xlab("Mean preservation rate (q_0)")
ggsave(filename = "~/Desktop/shape_vs_q0.png", width = 10, units = "in", height = 5.3)

ggplot(data=mcmc, aes(x=q_1, y=w_shape))+geom_hline(yintercept = 1,linetype=2)+geom_point(alpha=.1, color="red")+ylab("Shape")+xlab("Mean preservation rate (q_1)")
ggsave(filename = "~/Desktop/shape_vs_q1.png", width = 10, units = "in", height = 5.3)

ggplot(data=mcmc, aes(x=q_2, y=w_shape))+geom_hline(yintercept = 1,linetype=2)+geom_point(alpha=.1, color="red")+ylab("Shape")+xlab("Mean preservation rate (q_2)")
ggsave(filename = "~/Desktop/shape_vs_q2.png", width = 10, units = "in", height = 5.3)

ggplot(data=mcmc, aes(x=q_3, y=w_shape))+geom_hline(yintercept = 1,linetype=2)+geom_point(alpha=.1, color="red")+ylab("Shape")+xlab("Mean preservation rate (q_3)")
ggsave(filename = "~/Desktop/shape_vs_q3.png", width = 10, units = "in", height = 5.3)

ggplot(data=mcmc, aes(x=q_4, y=w_shape))+geom_hline(yintercept = 1,linetype=2)+geom_point(alpha=.1, color="red")+ylab("Shape")+xlab("Mean preservation rate (q_4)")
ggsave(filename = "~/Desktop/shape_vs_q4.png", width = 10, units = "in", height = 5.3)


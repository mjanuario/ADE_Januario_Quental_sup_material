#########################
#curating data
#########################

setwd("~/Downloads/ADE_revised_general/") 

#reading taxonomically revised data
fr=read.table("~/Downloads/ADE_revised_general/all_occ_sp.tsv", sep="\t", header=T, as.is = T)

#calculating occ timespan, and also tyding data
fr$occ_timespan=fr$MaxT-fr$MinT
fr$mid=fr$MinT+(0.5*fr$occ_timespan)
#fr=fr[fr$occ_timespan<=15,] #common filter to all Periods

#atributing Period
fr$Period="Paleogene"
fr[fr$mid<23, 7]="Neogene"

#will occ be excluded?
fr$included=TRUE
fr$included[fr$Period=="Paleogene" & fr$occ_timespan>10]=FALSE
fr$included[fr$Period=="Neogene" & fr$occ_timespan>5]=FALSE

#removed data:
sum(fr$included==FALSE)/nrow(fr) *100 # in percentage (%)

#ploting dataset:
library(cowplot)
fr=fr[order((fr$MaxT), decreasing = T),]
fr$id=1:nrow(fr) #aux variable
ggplot(fr)+
  geom_segment(aes(x=MinT, xend=MaxT, y=id, yend=id, col=included))+
  scale_color_manual(values = c("red", "black"))+
  xlab("Time (Mya)")+
  ylab("Fossil occurrences") + 
  scale_y_continuous(breaks=NULL)+
  guides(color=guide_legend(title="Adequate resolution"))+
  theme(legend.position="bottom")+
  scale_x_reverse()

fr1=fr[fr$included,]

ggplot(fr1)+
  geom_histogram(aes(x=occ_timespan, fill=Period),origin=0)+
  scale_fill_manual(values = c("#FFE600", "#fd9a52"))+
  xlab("Timespan (My)")+
  ylab("Frequency of fossil occurrences") + 
  guides(color=guide_legend(title="Sufficient resolution"))+
  theme(legend.position="bottom")

ggsave("~/Desktop/occ_timespan.png",width = 300, height = 150, units = "mm")


#removing occurrences w/ low temporal resolution:
fr=fr[fr$included, ]
nrow(fr)

#########################
#writing datasets as .tsv:
#########################

#species level
write.table(fr[,1:4], file = "~/Downloads/ADE_revised_general/sp.tsv", row.names = F, quote = F, sep = "\t")

#genus level
gen=fr
gen$Species = gsub("_.*", "", gen$Species)
library(ape)
trees=read.nexus("~/Downloads/ADE_revised_general/Cantalapiedra_etal_ruminantia_trees.nex")

t=trees[[1]]
living_gen=gsub("_.*", "", t$tip.label)

gen$Status = gen$Species %in% living_gen
gen$Status[gen$Status==FALSE]="extinct"
gen$Status[gen$Status==TRUE]="extant"
head(gen)
write.table(gen[,1:4], file = "~/Downloads/ADE_revised_general/gen.tsv", row.names = F, quote = F, sep = "\t")

###########################
#setting PyRate analysis:
###########################

#loading auxiliary functions:
source("~/Desktop/PyRateTools/R/pyrate_tools_pre_package.R")

#resampling occurrences:
extract.ages(file="sp.tsv", replicates = 100)
extract.ages(file="gen.tsv", replicates = 100)

#paralel computation:

################################
#genus level:
################################

#preliminar analysis:
parallel.pyrate(command_line = "python PyRate.py gen_PyRate.py -N 79 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 10000 -b 100000 -n 60100000 -p 10000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0", replicates = 1:100, n_cores = 50, folder = "~/Downloads/ADE_revised_general/")

#Genera regime 1 (main)
parallel.pyrate(command_line = "python PyRate.py gen_PyRate.py -N 79 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 10000 -b 100000 -n 60100000 -p 10000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter Inf 14.5", replicates = 1:100, n_cores = 50, folder = "~/Downloads/ADE_revised_general/") #done

#Genera regime 1 (narrow)
parallel.pyrate(command_line = "python PyRate.py gen_PyRate.py -N 79 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 10000 -b 100000 -n 60100000 -p 10000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter Inf 21.5", replicates = 1:100, n_cores = 50, folder = "~/Downloads/ADE_revised_general/") #running on jabba

#Genera regime 2 (main)
parallel.pyrate(command_line = "python PyRate.py gen_PyRate.py -N 79 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 10000 -b 100000 -n 60100000 -p 10000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter 9 2", replicates = seq(1, 100, by=2), n_cores = 25, folder = "~/Downloads/ADE_revised_general/") #running (25 per time) in leia

################################
#species level
################################

#preliminar analysis:
parallel.pyrate(command_line = "python PyRate.py sp_PyRate.py -N 197 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 10000 -b 100000 -n 60100000 -p 10000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0", replicates = 1:100, n_cores = 50, folder = "~/Downloads/ADE_revised_general/")

#Species regime 1 (main)
parallel.pyrate(command_line = "python PyRate.py sp_PyRate.py -N 197 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 10000 -b 100000 -n 60100000 -p 10000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter Inf 14", replicates = 1:100, n_cores = 50, folder = "~/Downloads/ADE_revised_general/") #running on c3po

#Species regime 1 (narrow)
parallel.pyrate(command_line = "python PyRate.py sp_PyRate.py -N 197 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 10000 -b 100000 -n 60100000 -p 10000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter 39.5 16", replicates = 1:100, n_cores = 50, folder = "~/Downloads/ADE_revised_general/") #finished 50 (impares)

#Species regime 2 (main)
parallel.pyrate(command_line = "python PyRate.py sp_PyRate.py -N 197 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 10000 -b 100000 -n 60100000 -p 10000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter 12.5 6.5", replicates = 1:100, n_cores = 50, folder = "~/Downloads/ADE_revised_general/") #running on r2d2

#Species regime 2 (narrow)
parallel.pyrate(command_line = "python PyRate.py sp_PyRate.py -N 197 -mG -A4 -pP 1.5 1.5 -s 10000 -b 100000 -n 60100000 -p 10000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter 11 8", replicates = seq(1, 100, by=2), n_cores = 25, folder = "~/Downloads/ADE_revised_general/") #running (25 per time) in leia


########################################
#setting mono/politypic PyRate analysis:
########################################

#loading auxiliary functions:
source("~/Desktop/PyRateTools/R/pyrate_tools_pre_package.R")

data=read.table("~/Downloads/ADE_revised_general/sp.tsv", sep="\t", header=T)
head(data)

data$spp=data$Species #duplicate spp colmun
data$Species= gsub("_.*", "", data$Species) #remove specific
head(data) #checking

tab=table(gsub("_.*", "", unique(data$spp))) #number of sampled species per genus

mono=names(tab[which(tab==1)]) #monotypic
data=read.table("~/Downloads/ADE_revised_general/gen.tsv", sep="\t", header=T) #reading genus data

head(data) #checking

monodt=data[data$Species %in% mono,] #monotypic dataset
polidt=data[!(data$Species %in% mono),] #politypic dataset

head(monodt);dim(monodt)
head(polidt);dim(polidt)


write.table(monodt, file = "~/Downloads/ADE_revised_general/mono_gen.tsv", row.names = F, quote = F, sep = "\t")

write.table(polidt, file = "~/Downloads/ADE_revised_general/poli_gen.tsv", row.names = F, quote = F, sep = "\t")

setwd("~/Downloads/ADE_revised_general/")
extract.ages(file="mono_gen.tsv", replicates = 50)
extract.ages(file="poli_gen.tsv", replicates = 50)

#setting the python analysis from bash:
parallel.pyrate(command_line = "python PyRate.py mono_gen_PyRate.py -N 79 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 10000 -b 100000 -n 60100000 -p 10000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter Inf 14.5", replicates = seq(1,100,by=2), n_cores = 50, folder = "~/Downloads/ADE_revised_general/")

parallel.pyrate(command_line = "python PyRate.py poli_gen_PyRate.py -N 79 -mG -qShift intervals.txt -A4 -pP 1.5 1.5 -s 10000 -b 100000 -n 60100000 -p 10000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 -filter Inf 14.5", replicates = seq(1,100,by=2), n_cores = 50, folder = "~/Downloads/ADE_revised_general/")

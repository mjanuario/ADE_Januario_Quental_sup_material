#writing slaves
replica=1:100
analysis=rep(c("spc", "sps", "gen"), each=100)
filters=rep(c("-filter 39.5 8", "-filter inf 6.5", "-filter inf 10.5"), each=100)
datasets=rep(c("sp", "sp", "gen"), each=100)

runs=paste("python PyRate.py OUR_", datasets, "_PyRate.py -N 197 -j ", replica, 
      paste(" -out _ADE_", analysis, "_", sprintf('%0.4d', replica), sep=""),
      " -mG -qShift ",
      "intervals.txt ",
      "-A4 -pP 1.5 1.5 -s 25000 -b 1000 -n 50000000 -p 1000000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 -ADE 1 ",
      filters, " ", sep="")
#test:
runs[c(1,101,201)]


for(i in seq(1, 300, by=2)){
  write.table(x = c("cd OUR", runs[c(i, i+1)]), file = paste0("~/Downloads/ADE_Januario_Quental_supplementar_material/data/slave", i, ".sh"), quote = F, row.names = F, col.names = F)
}

#writing masters
masters=paste("nohup sh slave", seq(1, 300, by=2),".sh > erro_slave",seq(1, 300, by=2), ".txt &" , sep="")
write.table(x=masters[1:50], file = paste0("~/Downloads/ADE_Januario_Quental_supplementar_material/data/master1.sh") , quote = F, row.names = F, col.names = F)
write.table(x=masters[51:100], file = paste0("~/Downloads/ADE_Januario_Quental_supplementar_material/data/master2.sh") , quote = F, row.names = F, col.names = F)
write.table(x=masters[101:150], file = paste0("~/Downloads/ADE_Januario_Quental_supplementar_material/data/master3.sh") , quote = F, row.names = F, col.names = F)
write.table(x=masters[c(50,100,150)], file = paste0("~/Downloads/ADE_Januario_Quental_supplementar_material/data/master_test.sh") , quote = F, row.names = F, col.names = F)


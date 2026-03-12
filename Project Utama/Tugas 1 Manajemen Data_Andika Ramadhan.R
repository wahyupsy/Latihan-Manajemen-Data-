library(haven)
TIMSS_Indonesia_2015<-read_sav("TIMSS_Indonesia_2015")
data<-TIMSS_Indonesia_2015
data
rm(TIMSS_Indonesia_2015)
rm
ncol(data)
colnames(data) [, 89:152]
#Panggil nama kolom dari data
data_btbm<-data[, grep("^BTBM", colnames(data))]
data_btbm

#Tugas Manajemen Data
data_unik<-unique(substr(colnames(data),1,5))
data_unik

#Penamaan data unik
dt_bsmnu<-data[, grep("BSMNU", names(data), value = T)]
dt_bssea<-data[, grep("BSSEA", names(data), value = T)]
dt_bsmge<-data[, grep("BSMGE", names(data), value = T)]
dt_bssch<-data[, grep("BSSCH", names(data), value=T)]
dt_bsmal<-data[, grep("BSMAL", names(data), value = T)]
dt_bssph<-data[, grep("BSSPH", names(data), value = T)]
dt_bsmkn<-data[, grep("BSMKN", names(data), value = T)]
dt_bsmda<-data[, grep("BSMDA", names(data), value = T)]
dt_bsmre<-data[, grep("BSMRE", names(data), value = T)]
dt_bssap<-data[, grep("BSSAP", names(data), value = T)]

#list data
list_data<-list(dt_bsmnu,dt_bssea,dt_bsmge,dt_bssch,dt_bsmal,dt_bssph,
                dt_bsmkn,dt_bsmda,dt_bsmre,dt_bssap)

#Korelasi/Corrplot
matcor_bssmnu<-cor(dt_bsmnu)
matcor_bssea<-cor(dt_bssea)
matcor_bsmge<-cor(dt_bsmge)
matcor_bssch<-cor(dt_bssch)
matcor_bsmal<-cor(dt_bsmal)
matcor_bssph<-cor(dt_bssph)
matcor_bsmkn<-cor(dt_bsmkn)
matcor_bsmda<-cor(dt_bsmda)
matcor_bsmre<-cor(dt_bsmre)
matcor_bssap<-cor(dt_bssap)

#corrplot
corrplot::corrplot(matcor_bssmnu,
                  method = "shade",
                  type = "full",
                  til.col = 'darkblue',
                  til.srt = 45)
corrplot::corrplot(matcor_bssea,
                   method = "circle",
                   type = "full",
                   til.col = 'darkblue',
                   til.srt = 45)
corrplot::corrplot(matcor_bsmge,
                   method = "number",
                   type = "full",
                   til.col = 'darkblue',
                   til.srt = 45)
corrplot::corrplot(matcor_bssch,
                   method = "pie",
                   type = "full",
                   til.col = 'darkblue',
                   til.srt = 45)
corrplot::corrplot(matcor_bsmal,
                   method = "square",
                   type = "full",
                   til.col = 'darkblue',
                   til.srt = 45)
corrplot::corrplot(matcor_bssph,
                   method = "shade",
                   type = "full",
                   til.col = 'darkblue',
                   til.srt = 45)
corrplot::corrplot(matcor_bsmkn,
                   method = "circle",
                   type = "full",
                   til.col = 'darkblue',
                   til.srt = 45)
corrplot::corrplot(matcor_bsmda,
                   method = "number",
                   type = "full",
                   til.col = 'darkblue',
                   til.srt = 45)
corrplot::corrplot(matcor_bsmre,
                   method = "pie",
                   type = "full",
                   til.col = 'darkblue',
                   til.srt = 45)
corrplot::corrplot(matcor_bssap,
                   method = "square",
                   type = "full",
                   til.col = 'darkblue',
                   til.srt = 45)

#Looping
for(m in 1:ncol(dt_bsmnu)){
  colname<-colnames(dt_bsmnu)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_bsmnu[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="blue",
       border="darkblue")
  dev.off()
}
for(m in 1:ncol(dt_bssea)){
  colname<-colnames(dt_bssea)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_bssea[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="steelblue4",
       border="navy")
  dev.off()
}
for(m in 1:ncol(dt_bsmge)){
  colname<-colnames(dt_bsmge)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_bsmge[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="midnightblue",
       border="black")
  dev.off()
}
for(m in 1:ncol(dt_bssch)){
  colname<-colnames(dt_bssch)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_bssch[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="darkolivegreen3",
       border="darkgreen")
  dev.off()
}
for(m in 1:ncol(dt_bsmal)){
  colname<-colnames(dt_bsmal)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_bsmal[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="mediumpurple4",
       border="purple4")
  dev.off()
}
for(m in 1:ncol(dt_bssch)){
  colname<-colnames(dt_bssch)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_bssch[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="gray40",
       border="gray10")
  dev.off()
}
for(m in 1:ncol(dt_bsmal)){
  colname<-colnames(dt_bsmal)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_bsmal[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="dodgerblue3",
       border="dodgerblue4")
  dev.off()
}
for(m in 1:ncol(dt_bssph)){
  colname<-colnames(dt_bssph)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_bssph[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="darkolivegreen3",
       border="darkolivegreen4")
  dev.off()
}
for(m in 1:ncol(dt_bsmkn)){
  colname<-colnames(dt_bsmkn)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_bsmkn[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="plum3",
       border="plum4")
  dev.off()
}
for(m in 1:ncol(dt_bsmda)){
  colname<-colnames(dt_bsmda)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_bsmda[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="indianred2",
       border="indianred4")
  dev.off()
}
for(m in 1:ncol(dt_bsmre)){
  colname<-colnames(dt_bsmre)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_bsmre[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="slategray3",
       border="slategray4")
  dev.off()
}
for(m in 1:ncol(dt_bssap)){
  colname<-colnames(dt_bssap)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_bssap[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="seagreen3",
       border="seagreen4")
  dev.off()
}

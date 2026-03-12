data<-TIMSS_Indonesia_2015
rm(TIMSS_Indonesia_2015)

#TUGAS PERTEMUAN 4 (Kode Unik, List, Looping, Histogram dan Corrplot)

#Lihat kode unik
data_unik<-unique(substr(colnames(data),1,5))
data_unik

#ambil data berdasarkan kodenya (10 data)
dt_BSMAL<-data[,grep("BSMAL", names(data), value = TRUE)]
dt_BSMNU<-data[,grep("BSMNU", names(data), value = TRUE)]
dt_BSMGE<-data[,grep("BSMGE", names(data), value = TRUE)]
dt_BSSCH<-data[,grep("BSSCH", names(data), value = TRUE)]
dt_BSSEA<-data[,grep("BSSEA", names(data), value = TRUE)]
dt_BSMKN<-data[,grep("BSMKN", names(data), value = TRUE)]
dt_BSMAP<-data[,grep("BSMAP", names(data), value = TRUE)]
dt_BSMRE<-data[,grep("BSMRE", names(data), value = TRUE)]
dt_BSSKN<-data[,grep("BSSKN", names(data), value = TRUE)]
dt_BSSAP<-data[,grep("BSSAP", names(data), value = TRUE)]

#bikin list data (gabungin 10 data dalam bentuk list)
list_data<-list(dt_BSMAL,dt_BSMAP,dt_BSMGE,dt_BSMKN,dt_BSMNU,
                dt_BSMRE,dt_BSSAP,dt_BSSCH,dt_BSSEA,dt_BSSKN)

#KORELASI 
matcor_BSMAL<-cor(dt_BSMAL)
matcor_BSMAP<-cor(dt_BSMAP)
matcor_BSMGE<-cor(dt_BSMGE)
matcor_BSMKN<-cor(dt_BSMKN)
matcor_BSMNU<-cor(dt_BSMNU)
matcor_BSMRE<-cor(dt_BSMRE)
matcor_BSSAP<-cor(dt_BSSAP)
matcor_BSSCH<-cor(dt_BSSCH)
matcor_BSSEA<-cor(dt_BSSEA)
matcor_BSSKN<-cor(dt_BSSKN)

#corrplot
corrplot::corrplot(matcor_BSMAL, 
                   method = "shade", 
                   type = "full", 
                   tl.col = 'black',
                   tl.srt=45)
corrplot::corrplot(matcor_BSMAP, 
                   method = "circle", 
                   type = "full", 
                   tl.col = 'black',
                   tl.srt=45)
corrplot::corrplot(matcor_BSMGE, 
                   method = "number", 
                   type = "full", 
                   tl.col = 'black',
                   tl.srt=45)
corrplot::corrplot(matcor_BSMKN, 
                   method = "pie", 
                   type = "full", 
                   tl.col = 'black',
                   tl.srt=45)
corrplot::corrplot(matcor_BSMNU, 
                   method = "square", 
                   type = "full", 
                   tl.col = 'black',
                   tl.srt=45)
corrplot::corrplot(matcor_BSMRE, 
                   method = "shade", 
                   type = "full", 
                   tl.col = 'black',
                   tl.srt=45)
corrplot::corrplot(matcor_BSSAP, 
                   method = "circle", 
                   type = "full", 
                   tl.col = 'black',
                   tl.srt=45)
corrplot::corrplot(matcor_BSSCH, 
                   method = "number", 
                   type = "full", 
                   tl.col = 'black',
                   tl.srt=45)
corrplot::corrplot(matcor_BSSEA, 
                   method = "pie", 
                   type = "full", 
                   tl.col = 'black',
                   tl.srt=45)
corrplot::corrplot(matcor_BSSKN, 
                   method = "square", 
                   type = "full", 
                   tl.col = 'black',
                   tl.srt=45)


#Looping to histogram PNG
for(m in 1:ncol(dt_BSMAL)){
  colname<-colnames(dt_BSMAL)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_BSMAL[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="rosybrown2",
       border="white")
  dev.off()
}

for(m in 1:ncol(dt_BSMAP)){
  colname<-colnames(dt_BSMAP)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_BSMAP[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="rosybrown2",
       border="white")
  dev.off()
}

for(m in 1:ncol(dt_BSMGE)){
  colname<-colnames(dt_BSMGE)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_BSMGE[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="rosybrown2",
       border="white")
  dev.off()
}

for(m in 1:ncol(dt_BSMKN)){
  colname<-colnames(dt_BSMKN)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_BSMKN[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="rosybrown2",
       border="white")
  dev.off()
}

for(m in 1:ncol(dt_BSMNU)){
  colname<-colnames(dt_BSMNU)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_BSMNU[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="rosybrown2",
       border="white")
  dev.off()
}

for(m in 1:ncol(dt_BSMRE)){
  colname<-colnames(dt_BSMRE)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_BSMRE[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="rosybrown2",
       border="white")
  dev.off()
}

for(m in 1:ncol(dt_BSSAP)){
  colname<-colnames(dt_BSSAP)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_BSSAP[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="rosybrown2",
       border="white")
  dev.off()
}

for(m in 1:ncol(dt_BSSCH)){
  colname<-colnames(dt_BSSCH)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_BSSCH[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="rosybrown2",
       border="white")
  dev.off()
}

for(m in 1:ncol(dt_BSSEA)){
  colname<-colnames(dt_BSSEA)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_BSSEA[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="rosybrown2",
       border="white")
  dev.off()
}

for(m in 1:ncol(dt_BSSKN)){
  colname<-colnames(dt_BSSKN)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(dt_BSSKN[[m]],
       main = paste("Histogram of", colname),
       xlab=colname,
       col="rosybrown2",
       border="white")
  dev.off()
}

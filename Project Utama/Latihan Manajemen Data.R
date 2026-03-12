data_new
data_unik<-unique(substr(colnames(data_new),1,5))
data_unik
data_BSMAL<-data[,grep("BSMAL", names(data), value = TRUE)]
data_BSMAL
data_BSMNU<-data[,grep("BSMNU", names(data), value = TRUE)]
data_BSMNU
data_BSMGE<-data[,grep("BSMGE", names(data), value = TRUE)]
data_BSMGE
data_BSSCH<-data[,grep("BSSCH", names(data), value = TRUE)]
data_BSSCH
data_BSSEA<-data[,grep("BSSEA", names(data), value = TRUE)]
data_BSSEA
data_BSMKN<-data[,grep("BSMKN", names(data), value = TRUE)]
data_BSMKN
data_BSMAP<-data[,grep("BSMAP", names(data), value = TRUE)]
data_BSMAP
data_BSMRE<-data[,grep("BSMRE", names(data), value = TRUE)]
data_BSMRE
data_BSSKN<-data[,grep("BSSKN", names(data), value = TRUE)]
data_BSSKN
data_BSSAP<-data[,grep("BSSAP", names(data), value = TRUE)]
data_BSSAP
list_data<-list(data_BSMAL, data_BSMNU, data_BSMGE, data_BSSCH, data_BSSEA, data_BSMKN, 
                data_BSMAP, data_BSMRE, data_BSSKN, data_BSSAP)
list_data
library(corrplot)
corrplot::corrplot(cor(data_BSMAL),  
                   method="number", 
                   type="full", 
                   tl.col='black', 
                   tl.srt=45)
corrplot::corrplot(cor(data_BSMNU),  
                   method="circle", 
                   type="full", 
                   tl.col='black', 
                   tl.srt=45)
corrplot::corrplot(cor(data_BSMGE),  
                   method="square", 
                   type="full", 
                   tl.col='black', 
                   tl.srt=45)
corrplot::corrplot(cor(data_BSSCH),  
                   method="ellipse", 
                   type="full", 
                   tl.col='black', 
                   tl.srt=45)
corrplot::corrplot(cor(data_BSSEA),  
                   method="pie", 
                   type="full", 
                   tl.col='black', 
                   tl.srt=45)
corrplot::corrplot(cor(data_BSMKN),  
                   method="number", 
                   type="full", 
                   tl.col='black', 
                   tl.srt=45)
corrplot::corrplot(cor(data_BSMAP),  
                   method="circle", 
                   type="full", 
                   tl.col='black', 
                   tl.srt=45)
corrplot::corrplot(cor(data_BSMRE),  
                   method="square", 
                   type="full", 
                   tl.col='black', 
                   tl.srt=45)
corrplot::corrplot(cor(data_BSSKN),  
                   method="ellipse", 
                   type="full", 
                   tl.col='black', 
                   tl.srt=45)
corrplot::corrplot(cor(data_BSSAP),  
                   method="pie", 
                   type="full", 
                   tl.col='black', 
                   tl.srt=45)
for(m in 1:ncol(data_BSMAL)){
  colname<-colnames(data_BSMAL)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(data_BSMAL[[m]],
       main = paste("Histogram", colname),
       xlab=colname,
       col="deepskyblue",
       border="white")
  dev.off()
}
for(m in 1:ncol(data_BSMNU)){
  colname<-colnames(data_BSMNU)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(data_BSMNU[[m]],
       main = paste("Histogram", colname),
       xlab=colname,
       col="red",
       border="white")
  dev.off()
}
for(m in 1:ncol(data_BSMGE)){
  colname<-colnames(data_BSMGE)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(data_BSMGE[[m]],
       main = paste("Histogram", colname),
       xlab=colname,
       col="orange",
       border="white")
  dev.off()
}
for(m in 1:ncol(data_BSSCH)){
  colname<-colnames(data_BSSCH)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(data_BSSCH[[m]],
       main = paste("Histogram", colname),
       xlab=colname,
       col="green",
       border="white")
  dev.off()
}
for(m in 1:ncol(data_BSSEA)){
  colname<-colnames(data_BSSEA)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(data_BSSEA[[m]],
       main = paste("Histogram", colname),
       xlab=colname,
       col="brown",
       border="white")
  dev.off()
}
for(m in 1:ncol(data_BSMKN)){
  colname<-colnames(data_BSMKN)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(data_BSMKN[[m]],
       main = paste("Histogram", colname),
       xlab=colname,
       col="deepskyblue",
       border="gray1")
  dev.off()
}
for(m in 1:ncol(data_BSMAP)){
  colname<-colnames(data_BSMAP)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(data_BSMAP[[m]],
       main = paste("Histogram", colname),
       xlab=colname,
       col="red",
       border="gray1")
  dev.off()
}
for(m in 1:ncol(data_BSMRE)){
  colname<-colnames(data_BSMRE)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(data_BSMRE[[m]],
       main = paste("Histogram", colname),
       xlab=colname,
       col="orange",
       border="gray1")
  dev.off()
}
for(m in 1:ncol(data_BSSKN)){
  colname<-colnames(data_BSSKN)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(data_BSSKN[[m]],
       main = paste("Histogram", colname),
       xlab=colname,
       col="green",
       border="gray1")
  dev.off()
}
for(m in 1:ncol(data_BSSAP)){
  colname<-colnames(data_BSSAP)[m]
  outfile<-paste0(colname,".png")
  png(outfile,width = 2000, height=2000,res=300)
  hist(data_BSSAP[[m]],
       main = paste("Histogram", colname),
       xlab=colname,
       col="brown",
       border="gray1")
  dev.off()
}

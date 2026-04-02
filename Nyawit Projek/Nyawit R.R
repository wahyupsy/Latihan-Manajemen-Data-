install.packages("dplyr")
install.packages()

library(psych)
library(haven)
library(dplyr)
library(corrplot)
        
data <-read_sav("dataset/TIMSS Indonesia 2015.sav")
dim(data)

#Mengambil Bagian data#
data [5,8]
data [1:100,1:100]
data [c(1:100),c(1:100)]
data [c(1:100,200:600),]

data1 <- data [1:100,1:100]
data2 <- data [1:100,1:100]

head(data1)
head(data2)

datacomb <-bind_rows(data1, data2)
colnames(datacomb)

#UNTUK MENGAMBIL SPESIFIK DATA/VARIABEL"
databsmk <- datacomb %>% select(contains("BSMK"))
View(databsmk)
colnames(databsmk) 

colnames(databsmk)

#UNTUK MENGAMBIL SPESIFIK DATA BERDASARKAN JUMLAH HURUH/ANGKA"
colnames(data)
dtuniq <- data[,17:276]
colnames(dtuniq)
namates <- unique(substr(colnames(dtuniq), 1, 4))
namates

#UNTUK MENGAMBIL SPESIFIK DATA SET BERDSARKAN NAMA DATA
dtuniq_BSMKN <- dtuniq[, grep("BSMKN", colnames(dtuniq))]
colnames(dtuniq_BSMKN)

dtuniq_BTBG <- dtuniq[, grep ("BTBG", colnames(dtuniq))]
colnames(dtuniq_BTBG)

# MEMBUAT LIST
ListData <- list(dtuniq_BSMKN, dtuniq_BTBG, nomor)
nomor <- c(1:20)

data (,1)
data$BSMALG01

databaru <- ListData[(1)]
ListData [(3)]

# MENAMAI DATA
names(ListData)[1]<-"BSMKN"
names(ListData)[2]<-"BTBG"

# LOOPING
angka <- c(1:10)
angka <- c(21:40) 
angka [3]
for (i in 1:10){
  temp <- angka[i]
  print (temp+3)
}

for (i in 1:2){
  tmp <- ListData[[z]]
  write
  writexl::write_xlsx(tmp, paste0("dataku-",z,"xlsx")
}

#DIGIMAP
psych::describe(temp)

# PLOT PALENG
BSMKN
corr_bsm <- cor(dtuniq_BSMKN)


#FUNDAMEN LOOPING
Kalimat <- paste0("Aku dan Kamu")
kalimat2 <- paste0(Kalimat, "satu")

katax <- c("berbeda", 
           "teman mesra",
           "kosong" , 
           "bukan apa-apa" ,
           "tau ah gelap",
           "luar biasa" , 
           "Horeee")
katax [2]
for (i in 1:7){
  print(paste0(Kalimat, "-", katax[i]))
               

               }
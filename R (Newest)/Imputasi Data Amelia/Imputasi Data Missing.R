#Install Package Amelia II dan Package yang lainnya
library(Amelia)
library(readxl)
library(dplyr)

dt <- read_excel("Data set miss.xlsx")
summary(dt)
dtemp <- dt

namadimensi <- c("citra_tubuh", "Harga_diri", "Diri_Ideal", "Peran_Diri")
ButirDimensi <- list(c(1:18), c(19:54), c(67:83), c(84:95))

i=1
dtemp <- dt[ ,ButirDimensi[[i]]]

imp <- amelia(dt, m = 5)

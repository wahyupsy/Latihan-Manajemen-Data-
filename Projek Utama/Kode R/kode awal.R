

library(psych)
library(haven)
library(dplyr)

data <- read_sav("dataset/TIMSS Indonesia 2015.sav")
saveRDS (data, file="dataset/TIMSS Indonesia 2015.RDS")


dataku <- readRDS(file="dataset/TIMSS Indonesia 2015.RDS")


data1 <- data[1:100,1:100]
data2 <- data[1:100,1:100]

head(data1)
head(data2)


datagab <- bind_rows(data1, data2)
colnames(datagab)

# -- memilih kolom BSMK saja -----------
databsmk <- datagab %>% select(contains("BSMK"))


colnames(databsmk)




dir.create("histogram_output", showWarnings = FALSE)
numeric_cols <- sapply(datagab, is.numeric)

for (col in names(datagab)[numeric_cols]) {
  
  png(filename = paste0("histogram_output/", col, ".png"),
      width = 800, height = 600)
  
  hist(datagab[[col]],
       main = paste("Histogram of", col),
       xlab = col,
       col = "skyblue",
       border = "white")
  
  dev.off()
}


















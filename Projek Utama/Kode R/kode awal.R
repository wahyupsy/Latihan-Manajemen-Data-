library(psych)
library(haven)
library(dplyr)

data <- read_sav("dataset/TIMSS Indonesia 2015.sav")
dim(data)
data[1:100, 1:100]
data[c(1:100),c(1:100)]

data1 <- data[1:100, 1:100]
data2 <- data[1:100, 1:100]

head(data1)

datagab <- bind_rows(data1, data2)
colnames(datagab)

databsmk <- datagab %>% 
  select(contains("BSMK"))
View(databsmk)

# Membuat folder bernama 'output_histogram'
if(!dir.exists("output_histogram")) {
  dir.create("output_histogram")
}

# Memilih data BSMK saja
databsmk <- datagab %>% select(starts_with("BSMK"))

# Loop untuk setiap kolom
for (col_name in colnames(databsmk)) {
  
  # Menentukan nama file berdasarkan nama kolom
  file_path <- paste0("output_histogram/hist_", col_name, ".png")
  
  # Membuka perangkat grafis (PNG)
  png(file_path, width = 800, height = 600)
  
  # Membuat histogram
  # Anda bisa menyesuaikan warna dan label
  hist(databsmk[[col_name]], 
       main = paste("Distribusi", col_name),
       xlab = "Score",
       ylab = "Frekuensi",
       col = "skyblue",
       border = "white")
  
  # Menutup perangkat grafis
  dev.off()
}

install.packages("officer")
install.packages("magrittr")

library(officer)
library(magrittr)
library(dplyr)
library(ggplot2)

# 1. Setup Folder
if(!dir.exists("output_word")) dir.create("output_word")
if(!dir.exists("temp_plots")) dir.create("temp_plots")

# 2. Ambil Data BSMK
data_bsmk <- datagab %>% select(starts_with("BSMK"))

# 3. Inisialisasi Dokumen Word
doc <- read_docx() %>%
  body_add_heading("Laporan Analisis Deskriptif TIMSS 2015", level = 1) %>%
  body_add_par("Variabel BSMK (Mathematics - Knowing Domain)", style = "Normal") %>%
  body_add_par("Berikut adalah distribusi frekuensi dari Plausible Values (PV) untuk domain kognitif Knowing pada pelajaran Matematika.", style = "Normal")

# 4. Loop untuk Membuat Grafik dan Memasukkannya ke Word
for (col_name in colnames(data_bsmk)) {
  
  # A. Buat Plot menggunakan ggplot2
  p <- ggplot(data_bsmk, aes(x = .data[[col_name]])) +
    geom_histogram(fill = "steelblue", color = "white", bins = 25) +
    labs(title = paste("Histogram", col_name),
         subtitle = "Domain: Mathematics Knowing",
         x = "Plausible Value Score",
         y = "Frekuensi") +
    theme_minimal()
  
  # B. Simpan Plot Sementara
  plot_path <- paste0("temp_plots/", col_name, ".png")
  ggsave(plot_path, plot = p, width = 5, height = 4)
  
  # C. Tambahkan ke Word
  doc <- doc %>%
    body_add_heading(paste("Analisis Variabel:", col_name), level = 2) %>%
    body_add_par(paste("Grafik di bawah menunjukkan sebaran data untuk", col_name, 
                       ". Dalam konteks TIMSS, data ini merupakan estimasi kemampuan siswa."), style = "Normal") %>%
    body_add_img(src = plot_path, width = 5, height = 4) %>%
    body_add_break()
}

# 5. Simpan File Word
print(doc, target = "output_word/Laporan_TIMSS_BSMK.docx")

# Opsional: Hapus folder plot sementara
# unlink("temp_plots", recursive = TRUE)

# Pertemuan 5 Maret 2026
dt1 <-data[,17:276]
colnames(dt1)
namates <- unique(substr(colnames(dt1), 1, 4))
namates

dt1_bsmkn <-dt1[,grep("BSMKN", colnames(dt1))]
dt1_btbg <-dt1[,grep("BTBG", colnames(dt1))]

datagab_1 <-cbind(dt1_btbg, dt1_bsmkn)
colnames(datagab_1)

nomor<- c(1:20)
lisdata <-list(dt1_bsmkn, dt1_btbg, nomor)

names(lisdata)[1]<- "bsmkn"
lisdata[[1]]
angka<-c(21:40)
angka[1]
for (i in 1:10){
  tmp<-angka[i]
  print(tmp+3)
}

for (z in 1:2){
  tmp<-lisdata[[z]]
  writexl::write_xlsx(tmp, paste0("dataqyu-", z, ".xlsx"))
  
}

dt1_bsmkn


# Pastikan folder output tersedia
if(!dir.exists("output_histogramm")) {
  dir.create("output_histogramm")
}

# Looping untuk setiap kolom dalam dt1_bsmkn
for (col_name in colnames(dt1_bsmkn)) {
  
  file_path <- paste0("output_histogramm/hist_norm_", col_name, ".png")
  png(file_path, width = 800, height = 600)
  
  data_col <- dt1_bsmkn[[col_name]]
  
  # 1. Plot Histogram
  # freq = FALSE digunakan agar sumbu Y menunjukkan density, bukan frekuensi
  # Ini wajib agar skala histogram sesuai dengan kurva normal
  hist(data_col, 
       freq = FALSE, 
       main = paste("Distribusi", col_name),
       xlab = "Skor",
       ylab = "Density",
       col = "purple", 
       border = "white")
  
  # 2. Tambahkan Garis Kurva Normal
  # dnorm menghitung probability density function (PDF) dari distribusi normal
  # x ~ N(\mu, \sigma^2)
  curve(dnorm(x, mean = mean(data_col, na.rm = TRUE), 
              sd = sd(data_col, na.rm = TRUE)), 
        add = TRUE, 
        col = "black", 
        lwd = 2)
  
  dev.off()
}


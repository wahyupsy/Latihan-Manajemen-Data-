# ========================================
# Analisis Perbandingan PIP: SMA vs SMK
# ========================================

# 1. Load packages
library(dplyr)
library(Amelia)
library(car)

# 2. Baca data
data_umum <- readRDS("Rapor asesmen Nasional SMA.RDS")

# 3. Konversi dan clean data
data_clean <- data_umum |>
  mutate(
    # Konversi jumlah_siswa_penerima_PIP ke numeric
    jumlah_siswa_penerima_PIP = case_when(
      jumlah_siswa_penerima_PIP == "NULL" ~ NA_character_,
      TRUE ~ jumlah_siswa_penerima_PIP
    ),
    jumlah_siswa_penerima_PIP = as.numeric(jumlah_siswa_penerima_PIP),
    
    # Konversi jenis_sek ke factor
    jenis_sek = as.factor(jenis_sek)
  )

# 4. Cek missing data
cat("=== MISSING DATA SUMMARY ===\n")
cat("Total observasi:", nrow(data_clean), "\n")
cat("Missing di jumlah_siswa_penerima_PIP:", sum(is.na(data_clean$jumlah_siswa_penerima_PIP)), "\n")
cat("Proporsi missing:", 
    round(sum(is.na(data_clean$jumlah_siswa_penerima_PIP)) / nrow(data_clean) * 100, 2), "%\n\n")

data_analisis <- data_clean |>
  filter(jenis_sek %in% c("SMA", "SMK")) |>
  # PENTING: Menghapus level factor yang tidak ada datanya
  mutate(jenis_sek = droplevels(jenis_sek)) |> 
  select(jenis_sek, jumlah_siswa_penerima_PIP, 
         jumlah_peserta_didik, SES_sekolah, kd_kokab) |>
  mutate(
    jumlah_peserta_didik = as.numeric(jumlah_peserta_didik),
    SES_sekolah = as.numeric(SES_sekolah)
  )

missing_pip <- sum(is.na(data_analisis$jumlah_siswa_penerima_PIP))
cat("=== SETELAH SUBSET ===\n")
cat("Total observasi (SMA + SMK):", nrow(data_analisis), "\n")
cat("Missing di jumlah_siswa_penerima_PIP:", missing_pip, "\n")
cat("Proporsi missing:", round(missing_pip / nrow(data_analisis) * 100, 2), "%\n\n")

if(missing_pip > 0) {
  cat("=== MELAKUKAN IMPUTASI AMELIA ===\n")
  
  # Prepare data untuk Amelia (hanya variabel numerik)
  data_for_amelia <- data_analisis |>
    select(jumlah_siswa_penerima_PIP, jumlah_peserta_didik, SES_sekolah) |>
    as.data.frame()
  
  # Jalankan Amelia dengan 5 imputasi
  set.seed(12345)
  amelia_out <- amelia(data_for_amelia, 
                       m = 5,  # 5 imputed datasets
                       idvars = NULL,
                       noms = NULL,
                       ords = NULL,
                       p2s = 0)  # suppress progress bar
  
  # Ambil dataset pertama hasil imputasi
  data_imputed <- amelia_out$imputations[[1]]
  
  # Gabungkan kembali dengan jenis_sek
  data_final <- data_analisis |>
    select(jenis_sek) |>
    bind_cols(data_imputed)
  
  cat("Imputasi selesai.\n\n")
} else {
  cat("=== TIDAK ADA MISSING DATA - IMPUTASI TIDAK DIPERLUKAN ===\n\n")
  data_final <- data_analisis
}

# 5. Deskriptif statistik per kelompok
cat("=== STATISTIK DESKRIPTIF ===\n")
desc_stats <- data_final |>
  summarise(
    n = n(),
    mean = mean(jumlah_siswa_penerima_PIP, na.rm = TRUE),
    sd = sd(jumlah_siswa_penerima_PIP, na.rm = TRUE),
    median = median(jumlah_siswa_penerima_PIP, na.rm = TRUE),
    min = min(jumlah_siswa_penerima_PIP, na.rm = TRUE),
    max = max(jumlah_siswa_penerima_PIP, na.rm = TRUE),
    .by = jenis_sek
  )
print(desc_stats)
cat("\n")

# 6. Uji normalitas per kelompok
cat("=== UJI NORMALITAS (SHAPIRO-WILK) ===\n")
sma_data <- data_final |> filter(jenis_sek == "SMA") |> pull(jumlah_siswa_penerima_PIP)
smk_data <- data_final |> filter(jenis_sek == "SMK") |> pull(jumlah_siswa_penerima_PIP)

if(length(sma_data) > 5000) {
  set.seed(12345)
  sma_sample <- sample(sma_data, 5000)
} else {
  sma_sample <- sma_data
}

if(length(smk_data) > 5000) {
  set.seed(12345)
  smk_sample <- sample(smk_data, 5000)
} else {
  smk_sample <- smk_data
}

shapiro_sma <- shapiro.test(sma_sample)
shapiro_smk <- shapiro.test(smk_sample)

cat("SMA: W =", round(shapiro_sma$statistic, 4), ", p =", 
    format.pval(shapiro_sma$p.value, digits = 3), "\n")
cat("SMK: W =", round(shapiro_smk$statistic, 4), ", p =", 
    format.pval(shapiro_smk$p.value, digits = 3), "\n\n")

# 7. Uji homogenitas varians (Levene's test)
cat("=== UJI HOMOGENITAS VARIANS (LEVENE) ===\n")
levene_test <- leveneTest(jumlah_siswa_penerima_PIP ~ jenis_sek, data = data_final)
print(levene_test)
cat("\n")

# 8. Pilih uji beda yang sesuai
normal_assumption <- (shapiro_sma$p.value > 0.05) && (shapiro_smk$p.value > 0.05)
equal_var <- levene_test$`Pr(>F)`[1] > 0.05

if(normal_assumption && equal_var) {
  cat("=== UJI INDEPENDENT T-TEST (asumsi normal + varians homogen) ===\n")
  test_result <- t.test(jumlah_siswa_penerima_PIP ~ jenis_sek, 
                        data = data_final, 
                        var.equal = TRUE)
  print(test_result)
} else if(normal_assumption && !equal_var) {
  cat("=== UJI WELCH T-TEST (asumsi normal + varians heterogen) ===\n")
  test_result <- t.test(jumlah_siswa_penerima_PIP ~ jenis_sek, 
                        data = data_final, 
                        var.equal = FALSE)
  print(test_result)
} else {
  cat("=== UJI MANN-WHITNEY U (asumsi normalitas dilanggar) ===\n")
  test_result <- wilcox.test(jumlah_siswa_penerima_PIP ~ jenis_sek, 
                             data = data_final)
  print(test_result)
}

# 9. Effect size
cat("\n=== EFFECT SIZE ===\n")
if(normal_assumption) {
  # Cohen's d
  pooled_sd <- sqrt(((length(sma_data) - 1) * sd(sma_data)^2 + 
                       (length(smk_data) - 1) * sd(smk_data)^2) / 
                      (length(sma_data) + length(smk_data) - 2))
  cohens_d <- (mean(sma_data) - mean(smk_data)) / pooled_sd
  cat("Cohen's d =", round(cohens_d, 3), "\n")
  cat("Interpretasi: ")
  if(abs(cohens_d) < 0.2) cat("trivial\n")
  else if(abs(cohens_d) < 0.5) cat("small\n")
  else if(abs(cohens_d) < 0.8) cat("medium\n")
  else cat("large\n")
} else {
  # Rank-biserial correlation
  r <- 1 - (2 * test_result$statistic) / (length(sma_data) * length(smk_data))
  cat("Rank-biserial correlation =", round(r, 3), "\n")S
}

# ========================================
# 9.1 Visualisasi (Tambahan)
# ========================================
library(ggplot2)

# A. Boxplot untuk melihat sebaran dan Median
plot_boxplot <- ggplot(data_final, aes(x = jenis_sek, y = jumlah_siswa_penerima_PIP, fill = jenis_sek)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) + # Sembunyikan outlier bawaan agar tidak double dengan jitter
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) + 
  labs(
    title = "Distribusi Penerima PIP: SMA vs SMK",
    subtitle = "Visualisasi boxplot dengan sebaran titik data",
    x = "Jenis Sekolah",
    y = "Jumlah Siswa Penerima PIP",
    fill = "Sekolah"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Tampilkan plot
print(plot_boxplot)

# B. Density Plot untuk melihat Normalitas/Skewness
plot_density <- ggplot(data_final, aes(x = jumlah_siswa_penerima_PIP, fill = jenis_sek)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Kurva Densitas Penerima PIP",
    x = "Jumlah Siswa Penerima PIP",
    y = "Density",
    fill = "Jenis Sekolah"
  ) +
  theme_classic()

# Tampilkan plot
print(plot_density)

# Simpan visualisasi ke file
ggsave("boxplot_pip.png", plot = plot_boxplot, width = 8, height = 6)
ggsave("density_pip.png", plot = plot_density, width = 8, height = 6)

# ========================================
# 10. Membuat TABEL APA
# ========================================

library(gtsummary)
library(flextable)

cat("\n=== MEMBUAT TABEL FORMAT APA (VERSI TERBARU) ===\n")

tabel_apa <- data_final |>
  select(jenis_sek, jumlah_siswa_penerima_PIP) |>
  tbl_summary(
    by = jenis_sek,
    missing = "no", 
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    label = list(jumlah_siswa_penerima_PIP ~ "Jumlah Siswa Penerima PIP")
  ) |>
  add_overall() |>
  add_difference(test = list(all_continuous() ~ jenis_uji_tabel)) |>
  
  modify_header(
    label = "**Variabel**",
    stat_1 = "**SMA**\nN = {n}",
    stat_2 = "**SMK**\nN = {n}",
    estimate = "**Mean Difference**",
    conf.low = "**95% CI**",  # gtsummary 2.0 menggabungkan low & high di kolom ini
    p.value = "**p-value**"
  ) |>
  
  modify_footnote(
    everything() ~ NA, 
    p.value ~ "Diuji menggunakan penyesuaian asumsi (T-Test/Welch/Mann-Whitney)"
  ) |>
  
  modify_caption("**Tabel 1**\n*Perbandingan Jumlah Siswa Penerima PIP antara SMA dan SMK*") |>
  as_flex_table() |>
  theme_apa() 

# Tampilkan dan simpan
tabel_apa
save_as_docx(tabel_apa, path = "Tabel_Hasil_PIP_APA_Final.docx")
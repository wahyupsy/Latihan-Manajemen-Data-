# ==============================================================================
# BAGIAN 0: INSTALASI DAN PEMUATAN PAKET
# ==============================================================================

library(tidyverse)
library(labelled)
library(skimr)
library(corrplot)
library(psych)
library(Amelia)
library(readxl)


# ==============================================================================
# BAGIAN 1: MEMUAT DATA
# ==============================================================================

# --- 1.1 Data utama (RDS) ---
df_raw <- readRDS("Rapor asesmen Nasional SMA.RDS")

cat("Dimensi data mentah:", nrow(df_raw), "baris x", ncol(df_raw), "kolom\n")

# --- 1.2 Codebook (Excel) ---
codebook <- read_excel(
  "1 Codebook rapor-publik-asesmen-nasional-2025-guru-2025-sma-smk-ma-mak-sederajat.XLSX"
)

# Standarisasi nama kolom codebook
codebook <- codebook |>
  rename(
    variable    = Codebook,
    nama_label  = `Nama Indikator`,
    definisi    = `Definisi Konseptual Indikator`,
    operasional = `Definisi Operasional Indikator`,
    rentang     = `Rentang Nilai/Keterangan`,
    sumber      = `sumber data`
  )

cat("\nJumlah variabel dalam codebook:", nrow(codebook), "\n")


# ==============================================================================
# BAGIAN 2: PRA-PEMROSESAN DAN KONVERSI TIPE DATA
# ==============================================================================

# --- 2.1 Identifikasi kolom dengan "NULL" string (bukan NA sejati) ---
# Dalam data mentah, nilai hilang dikodekan sebagai string "NULL"
# Ini harus dikonversi ke NA sebelum dianalisis

df <- df_raw

# Tampilkan kolom bermasalah (character yang seharusnya numerik)
cols_char <- df |> select(where(is.character)) |> names()

# Kolom identifikasi/kategorikal yang memang karakter
cols_id <- c(
  "kd_guru_an", "kd_sekolah", "pendidikan_sederajat", "jenis_sek",
  "sts_sek", "kurikulum", "daerah_khusus", "kd_kokab",
  "wilayah_bagian", "jenis_wilayah", "status_wilayah",
  "ketersediaan_internet", "ketersediaan_listrik"
)

# Kolom skor (0–100) yang tersimpan sebagai character karena "NULL"
cols_skor_char <- setdiff(cols_char, cols_id)

cat("\nKolom skor yang tersimpan sebagai character (perlu dikonversi):\n")
cat(paste(cols_skor_char, collapse = ", "), "\n")

# --- 2.2 Konversi kolom skor ke numerik ---
# "NULL" otomatis menjadi NA, angka dikonversi ke numeric
df <- df |>
  mutate(
    across(
      all_of(cols_skor_char),
      ~ suppressWarnings(as.numeric(na_if(.x, "NULL")))
    )
  )

# --- 2.3 Konversi kolom numerik (proporsi) dari character ke numeric ---
cols_proporsi <- c(
  "proporsi_pendidik_min_s1", "proporsi_pendidik_sertifikasi",
  "jumlah_peserta_didik", "jumlah_pendidik", "rasio_pendidik_peserta_didik",
  "jumlah_r_kelas", "jumlah_komp_milik", "jumlah_perpus",
  "jumlah_rombel", "jumlah_siswa_rombel", "jumlah_siswa_penerima_PIP",
  "rasio_siswa_penerima_PIP"
)

df <- df |>
  mutate(
    across(
      all_of(cols_proporsi),
      ~ suppressWarnings(as.numeric(.x))
    )
  )

# --- 2.4 Konversi kolom kategorikal ke factor ---
df <- df |>
  mutate(
    jenis_sek         = factor(jenis_sek),
    sts_sek           = factor(sts_sek,
                               levels = c("N", "S"),
                               labels = c("Negeri", "Swasta")),
    kurikulum         = factor(kurikulum),
    daerah_khusus     = factor(daerah_khusus,
                               levels = c("tidak", "ya"),
                               labels = c("Bukan Daerah Khusus", "Daerah Khusus")),
    wilayah_bagian    = factor(wilayah_bagian,
                               levels = c("Indonesia Barat",
                                          "Indonesia Tengah",
                                          "Indonesia Timur")),
    jenis_wilayah     = factor(jenis_wilayah),
    status_wilayah    = factor(status_wilayah,
                               levels = c("URBAN", "RURAL", "PERI-URBAN")),
    ketersediaan_internet = factor(ketersediaan_internet,
                                   levels = c("Ada", "Tidak Ada")),
    ketersediaan_listrik  = factor(ketersediaan_listrik,
                                   levels = c("Ada", "Tidak Ada"))
  )

cat("\nTipe data setelah konversi:\n")
df |>
  summarise(across(everything(), ~ class(.x)[1])) |>
  pivot_longer(everything(), names_to = "variabel", values_to = "tipe") |>
  count(tipe) |>
  print()


# ==============================================================================
# BAGIAN 3: PEMBERIAN LABEL VARIABEL
# ==============================================================================

# Fungsi: buat named vector label dari codebook
label_vector <- setNames(
  codebook$nama_label,
  codebook$variable
) |>
  # Hanya ambil yang ada di data
  (\(x) x[names(x) %in% names(df)])()

# Terapkan label ke data frame
var_label(df) <- as.list(label_vector)

# Verifikasi label pada beberapa variabel
cat("\nContoh label variabel:\n")
cat("ACH  :", var_label(df$ACH), "\n")
cat("ITB  :", var_label(df$ITB), "\n")
cat("SES  :", var_label(df$SES_sekolah), "\n")
cat("JOT  :", var_label(df$JOT), "\n")

# Simpan data berlabel
saveRDS(df, "df_rapor_AN_berlabel.rds")
cat("\nData berlabel disimpan: df_rapor_AN_berlabel.rds\n")

# ==============================================================================
# BAGIAN 4: EKSPLORASI AWAL (PRE-IMPUTATION)
# ==============================================================================

# --- 4.1 Gambaran umum data ---
cat("\n========== GAMBARAN UMUM DATA ==========\n")
glimpse(df)

# --- 4.2 Statistik deskriptif ringkas ---
# Semua kolom sekaligus
cat("\n========== STATISTIK DESKRIPTIF (skimr) ==========\n")
skim_tanpa_histogram <- skim_with(numeric = sfl(hist = NULL))
print(skim_tanpa_histogram(df))

# --- 4.3 Deskriptif mendalam untuk variabel skor (psych) ---
# Identifikasi kolom skor numerik (0-100, dari AN)
cols_skor_num <- df |>
  select(ACH:SPAB_6) |>
  select(where(is.numeric)) |>
  names()

cat("\n========== DESKRIPTIF SKOR AN (psych::describe) ==========\n")
psych::describe(df[, cols_skor_num]) |>
  round(2) |>
  print()

# --- 4.4 Distribusi variabel kategorikal ---
cat("\n========== DISTRIBUSI VARIABEL KATEGORIKAL ==========\n")

cat("\n-- Jenis Sekolah --\n")
print(count(df, jenis_sek, sort = TRUE))

cat("\n-- Status Sekolah (Negeri/Swasta) --\n")
print(count(df, sts_sek))

cat("\n-- Kurikulum --\n")
print(count(df, kurikulum))

cat("\n-- Status Wilayah --\n")
print(count(df, status_wilayah))

cat("\n-- Wilayah Bagian --\n")
print(count(df, wilayah_bagian))

cat("\n-- Ketersediaan Internet --\n")
print(count(df, ketersediaan_internet))

# --- 4.5 Pola missing data ---
cat("\n========== POLA MISSING DATA ==========\n")
missing_summary <- df |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variabel", values_to = "n_missing") |>
  mutate(pct_missing = round(n_missing / nrow(df) * 100, 2)) |>
  filter(n_missing > 0) |>
  arrange(desc(pct_missing))

cat("Variabel dengan missing data:\n")
print(missing_summary, n = Inf)

# Cek apakah SMK_* memang seluruhnya kosong untuk SMA (masuk akal)
cat("\n-- Cek kolom SMK pada baris jenis_sek == SMA --\n")
df |>
  filter(jenis_sek == "SMA") |>
  select(starts_with("SMK_")) |>
  summarise(across(everything(), ~ sum(!is.na(.)))) |>
  print()

# Catatan: kolom SMK_* diisi HANYA untuk sekolah SMK
# Ini bukan missing acak (MNAR by design), tidak perlu diimputasi


# ==============================================================================
# BAGIAN 5: IMPUTASI DATA MISSING — AMELIA II (EMB Algorithm)
# ==============================================================================

# Amelia menggunakan algoritma Expectation-Maximization with Bootstrapping (EMB):
#   - Asumsi utama: data multivariat normal (atau mendekati normal)
#   - Cocok untuk skor kontinu 0–100 (indeks komposit skala interval)
#   - Lebih cepat dari mice pada dataset besar karena EMB beroperasi
#     secara simultan (bukan variabel-per-variabel)
#   - Mendukung transformasi log/sqrt internal, spesifikasi batas nilai,
#     identifikasi variabel nominal/ordinal, dan variabel ID (dikecualikan)

# Strategi imputasi:
# 1. Kolom SMK_* → DIKECUALIKAN; hanya berlaku untuk sekolah SMK (MNAR by design)
# 2. Kolom identifikasi → dideklarasikan sebagai 'idvars' (tidak diimputasi)
# 3. Variabel kategorikal → dideklarasikan sebagai 'noms' (nominal)
# 4. Skor 0–100 → diberi batas bawah 0 dan batas atas 100 via argumen 'bounds'
# 5. m = 5 dataset imputasi (standar konvensi MI)

# --- 5.1 Pisahkan subset untuk imputasi ---
cols_smk    <- names(df)[startsWith(names(df), "SMK_")]
cols_id_all <- c("kd_guru_an", "kd_sekolah", "pendidikan_sederajat", "kd_kokab")

# Kolom kandidat imputasi (tanpa ID dan tanpa SMK)
cols_untuk_imputasi <- setdiff(names(df), c(cols_id_all, cols_smk))
df_imp_input <- df |> select(all_of(cols_untuk_imputasi))

# --- 5.2 Visualisasi pola missing sebelum imputasi ---
cat("\n========== IMPUTASI MISSING DATA (Amelia) ==========\n")
cat("Algoritma : Expectation-Maximization with Bootstrapping (EMB)\n")
cat("Asumsi    : Multivariat normal pada variabel kontinu\n")
cat("m         : 5 dataset imputasi\n\n")

# Peta missing data (jalankan interaktif untuk melihat plot)
# missmap(df_imp_input, main = "Peta Missing Data Sebelum Imputasi",
#         col = c("red", "grey80"), legend = TRUE)

# --- 5.3 Siapkan argumen Amelia ---

# Variabel nominal (kategorikal) — Amelia perlu tahu ini agar
# tidak memperlakukannya sebagai kontinu
vars_nominal <- c(
  "jenis_sek", "sts_sek", "kurikulum", "daerah_khusus",
  "wilayah_bagian", "jenis_wilayah", "status_wilayah",
  "ketersediaan_internet", "ketersediaan_listrik"
)
# Filter ke yang ada di df_imp_input
vars_nominal <- intersect(vars_nominal, names(df_imp_input))

# Ubah factor ke integer terlebih dahulu — Amelia menerima integer untuk noms
df_imp_amelia <- df_imp_input |>
  mutate(across(all_of(vars_nominal), as.integer))

# Identifikasi kolom skor 0–100 untuk diberi batas nilai
cols_skor_0100 <- df_imp_amelia |>
  select(where(is.numeric)) |>
  select(-all_of(intersect(
    c("proporsi_pendidik_min_s1","proporsi_pendidik_sertifikasi",
      "jumlah_peserta_didik","jumlah_pendidik","rasio_pendidik_peserta_didik",
      "jumlah_r_kelas","jumlah_komp_milik","jumlah_perpus",
      "jumlah_rombel","jumlah_siswa_rombel","jumlah_siswa_penerima_PIP",
      "rasio_siswa_penerima_PIP","SES_sekolah"),
    names(df_imp_amelia)
  ))) |>
  names()

# Bangun matriks bounds: kolom 1 = indeks kolom, 2 = batas bawah, 3 = batas atas
idx_skor <- which(names(df_imp_amelia) %in% cols_skor_0100)
bounds_matrix <- cbind(idx_skor, 0, 100)

# --- 5.4 Jalankan Amelia ---
set.seed(2025)

imp_amelia <- amelia(
  x        = df_imp_amelia,
  m        = 5,              # jumlah dataset imputasi
  noms     = vars_nominal,   # variabel nominal
  bounds   = bounds_matrix,  # batas nilai skor 0–100
  emburn   = c(10, 50),      # iterasi EM: minimum 10, maksimum 50
  # p2s    = 2,              # aktifkan untuk melihat log detail (0=silent, 1=ringkas, 2=detail)
  p2s      = 1
)

# --- 5.5 Diagnostik Amelia ---
cat("\n-- Ringkasan Hasil Amelia --\n")
summary(imp_amelia)

NEXT !@#

# Uji densitas: bandingkan distribusi observasi vs imputasi
# Jalankan secara interaktif untuk melihat plot:
# compare.density(imp_amelia, var = "ACH")   # skor ekspektasi akademik
# compare.density(imp_amelia, var = "ITB")   # skor non-diskriminasi
# compare.density(imp_amelia, var = "BCP")   # pemahaman hukuman fisik

# Overimputation diagnostic (cross-validation internal Amelia)
# Hati-hati: lambat pada data besar; nonaktifkan jika perlu
# overimpute(imp_amelia, var = "ACH")

# --- 5.6 Ambil satu dataset lengkap (dataset ke-1 dari 5) ---
df_imputed_subset <- imp_amelia$imputations[[1]]

# Kembalikan variabel nominal ke factor sesuai level semula
for (v in vars_nominal) {
  original_levels <- levels(df_imp_input[[v]])
  df_imputed_subset[[v]] <- factor(
    df_imputed_subset[[v]],
    levels = seq_along(original_levels),
    labels = original_levels
  )
}

# Gabungkan kembali kolom identifikasi dan SMK (tidak diimputasi)
df_lengkap <- df |>
  select(all_of(c(cols_id_all, cols_smk))) |>
  bind_cols(df_imputed_subset)

cat("\nVerifikasi missing setelah imputasi (kolom non-SMK):\n")
df_lengkap |>
  select(-all_of(c(cols_id_all, cols_smk))) |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything()) |>
  filter(value > 0) |>
  print()

# Simpan data hasil imputasi
saveRDS(df_lengkap, "df_rapor_AN_imputed.rds")

# Opsional: simpan seluruh objek imputasi Amelia (5 dataset)
# saveRDS(imp_amelia, "imp_amelia_object.rds")
# Gunakan ini jika ingin analisis pooled (Rubin's rules) dengan semua 5 dataset

cat("\nData imputasi disimpan: df_rapor_AN_imputed.rds\n")

# Shorthand: gunakan df_lengkap untuk analisis selanjutnya
df_an <- df_lengkap

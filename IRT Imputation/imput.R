library(mirt)
library(dplyr)

#Load Data
df <- read.csv("DataMerlin.csv")

# 3. Pemetaan Domain secara Presisi (Menangani butir_X dan butir_AX)
# Fungsi ini akan mencari kolom yang sesuai dengan angka butir yang dimaksud
get_actual_cols <- function(numbers, dataset_cols) {
  matches <- unlist(lapply(numbers, function(n) {
    # Mencari kolom yang berakhiran angka butir tersebut (misal: butir_1 atau butir_A1)
    dataset_cols[grep(paste0("butir_(A)?", n, "$"), dataset_cols)]
  }))
  return(unique(matches))
}

# Definisi Domain berdasarkan struktur data Anda
list_dimensi <- list(
  Citra_Tubuh    = get_actual_cols(1:18, colnames(df)),
  Harga_Diri     = get_actual_cols(19:54, colnames(df)),
  Ideal_Diri     = get_actual_cols(55:66, colnames(df)),
  Identitas_Diri = get_actual_cols(67:83, colnames(df)),
  Peran_Diri     = get_actual_cols(84:95, colnames(df))
)

# 4. Loop Imputasi Per Dimensi (Versi Perbaikan)
imputed_list <- list()

for (domain in names(list_dimensi)) {
  message("--- Memproses Domain: ", domain, " ---")
  
  # Ambil data dan pastikan dalam format numeric matrix
  sub_cols <- list_dimensi[[domain]]
  sub_data <- as.matrix(df[, sub_cols]) 
  
  # A. Fit Model (Gunakan "graded" bukan "GRM", hapus removeEmptyRows)
  # Graded Response Model cocok untuk skala Likert 1-4 Anda
  model_fit <- mirt(sub_data, 1, itemtype = 'graded')
  
  # B. Estimasi Skor Laten (Theta)
  # fscores menghitung kemampuan responden di setiap domain
  theta_scores <- fscores(model_fit, full.scores = TRUE, full.scores.SE = FALSE)
  
  # C. Imputasi (Mengisi nilai kosong berdasarkan pola IRT)
  # mirt akan memprediksi respon yang paling mungkin berdasarkan nilai Theta
  data_filled <- imputeMissing(model_fit, theta_scores)
  
  # Simpan hasil sementara
  imputed_list[[domain]] <- as.data.frame(data_filled)
}

# 5. Gabungkan kembali seluruh hasil menjadi satu dataset utuh
df_merlin_final <- do.call(cbind, imputed_list)

# Simpan ke CSV untuk dicek
write.csv(df_merlin_final, "Data_Merlin_Imputed_IRT_Final.csv", row.names = FALSE)

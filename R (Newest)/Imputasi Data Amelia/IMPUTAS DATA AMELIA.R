library(readxl)
library(dplyr)
library(Amelia)
library(writexl)

df <- read_excel("Data set miss.xlsx")

df1 <- df %>% dplyr::select(contains("butir"))
df1 <- df1 %>%
  mutate(across(c(butir_2, butir_11, butir_12, butir_14, butir_16, 
                  butir_A17, butir_24, butir_39, butir_40, butir_43, 
                  butir_59, butir_60, butir_71, butir_86, butir_95),
                ~ ifelse(is.na(.), NA, 5 - .)))


writexl::write_xlsx(df1, "Data Merlin.xlsx")


dt <- readxl::read_excel("Data Merlin.xlsx")

namadimensi <- c("Citra_Tubuh", "Harga_Diri", "Diri_Ideal", "Identitas", "Peran_Diri")
butirdimensi <- list(c(1:18), c(19:54), c(55:66), c(67:83), c(84:95))

i=1
for(i in 1:5)
  
  dtemp <- dt[,butirdimensi[[i]]]  

  # --- AMELIA 
item_cols <- grep("^butir", names(dtemp))
  ord_vars <- item_cols
  
  hasil_amelia <- amelia(
    dtemp,
    m = 3,                # jumlah imputasi
    ords = ord_vars,      # variabel ordinal
    idvars = NULL         # jika tidak ada ID
  )
  
  hasil_2 <- hasil_amelia$imputations[[1]]
  
  writexl::write_xlsx(hasil_2, paste0("Hasil Imputasi AMELIA",namadimensi[i],".xlsx"))
  
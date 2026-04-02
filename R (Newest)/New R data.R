dt<-readRDS(file="Data Long.RDS")
colnames(dt)
table(dt$subtest_name)

library(dplyr)
library(tidyr)

# 1. Filter data hanya untuk subtest "SisKer"
dt_sisker_long <- dt %>% 
  filter(subtest_name == "SisKer")

# 2. Transformasi ke Wide
# Kita ingin satu baris satu 'testee_id'
# Kolom baru diambil dari 'question_code'
# Nilainya diambil dari 'score'
dt_sisker_wide <- dt_sisker_long %>%
  select(testee_id, question_code, score) %>% # Ambil kolom yang relevan saja
  pivot_wider(
    names_from = question_code, 
    values_from = score
  )

# 3. Cek hasil
dim(dt_sisker_wide)
head(dt_sisker_wide)


item_cols <- colnames(dt_sisker_wide)
subtes_identifikasi <- unique(substr(item_cols, 1, 11))
subtes_identifikasi
table(substr(item_cols, 1, 11))

# Mengekstrak kolom yang memiliki kata "AVR"
dt_sisker_avr <- dt_sisker_wide %>% 
  select(testee_id, contains("AVR"))

# Cek hasil
head(dt_sisker_avr)

library(mice)

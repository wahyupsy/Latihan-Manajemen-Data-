library(ggplot2)
library(officer)
library(magrittr)


g1 <- ggplot(datagab, aes(x = .data[[20]])) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  labs(title = paste("Histogram Kolom", colnames(datagab)[20]),
       x = colnames(datagab)[20])

g2 <- ggplot(datagab, aes(x = .data[[21]])) +
  geom_histogram(fill = "darkorange", color = "white", bins = 30) +
  labs(title = paste("Histogram Kolom", colnames(datagab)[21]),
       x = colnames(datagab)[21])


library(officer)
library(magrittr)

doc <- read_docx()

doc <- doc %>%
  body_add_par(paste("Grafik:", col20), style = "heading 1") %>%
  body_add_gg(g1, width = 6, height = 4) %>%
  body_add_par(paste("Grafik:", col21), style = "heading 1") %>%
  body_add_gg(g2, width = 6, height = 4)

print(doc, target = "grafik_20_21.docx")

col20 <- names(datagab)[20]
col21 <- names(datagab)[21]

col20
col21


library(ggplot2)

g1 <- ggplot(datagab, aes(x = .data[[col20]])) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = paste("Histogram", col20), x = col20)

g2 <- ggplot(datagab, aes(x = .data[[col21]])) +
  geom_histogram(bins = 30, fill = "darkorange", color = "white") +
  labs(title = paste("Histogram", col21), x = col21)


library(officer)
library(magrittr)

doc <- read_docx()

doc <- doc %>%
  body_add_par(paste("Grafik:", col20), style = "heading 1") %>%
  body_add_gg(g1, width = 6, height = 4) %>%
  body_add_par(paste("Grafik:", col21), style = "heading 1") %>%
  body_add_gg(g2, width = 6, height = 4)

print(doc, target = "grafik_20_21.docx")

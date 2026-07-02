library(tidyverse)
library(shiny)
library(tidyr)
answers_dir<-"RATA/answers/"
files <- dir(answers_dir, full.names = TRUE)
# Only keep directories (remove `diseno.csv`).
files <- files[dir.exists(files)]

files <- dir(files, full.names = TRUE, recursive = TRUE)
answers <- data.frame(
  Producto = sub("\\.csv$", "", basename(files)),
  Valuador = basename(dirname(files))
) %>%
  bind_cols(read_csv(files, col_types = cols()))
req(nrow(answers) > 0)
answers<-arrange(answers, Producto, Valuador) %>%
  separate(Producto, c("Producto", "Rep"))




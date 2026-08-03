.libPaths(c("/home/anibal/R/library", .libPaths()))

library(SensoMineR)
library(ggplot2)

base <- "/home/anibal/Mis documentos/SENSORIAL/PANEL/2026/Tesista_Laura_Mercado_2026 (copy 1)/grupo_f1"
setwd(base)
archivo <- file.path("answers", "respuestas_unificadas.csv")
salida <- file.path(base, "resultados_elipses")
dir.create(salida, showWarnings = FALSE, recursive = TRUE)

datos_originales <- read.csv(
  archivo,
  check.names = FALSE,
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

atributos <- names(datos_originales)[-(1:2)]
datos <- datos_originales[, c("tratamiento", "panelista", atributos)]
datos$tratamiento <- factor(datos$tratamiento)
datos$panelista <- factor(datos$panelista)
datos[atributos] <- lapply(
  datos[atributos],
  function(x) suppressWarnings(as.numeric(x))
)

datos_completos <- datos[complete.cases(datos), ]
if (nrow(datos_completos) == 0) {
  stop("No hay filas completas para ejecutar panellipse.")
}

# SensoMineR imprime gráficos auxiliares durante el cálculo.
setwd(salida)

# Estructura requerida: producto, juez y variables sensoriales.
# Es la adaptación del análisis de vale_2025 a un ensayo sin sesiones.
resultado <- panellipse(
  datos_completos,
  col.p = 1,
  col.j = 2,
  firstvar = 3,
  level.search.desc = 1,
  graph.type = "ggplot"
)

grafico_elipses <- resultado$graph$plotIndEll +
  theme(text = element_text(family = "sans"))
ggsave(
  file.path(salida, "panellipse_tratamientos.png"),
  grafico_elipses,
  width = 11,
  height = 8,
  dpi = 300
)
ggsave(
  file.path(salida, "panellipse_tratamientos.pdf"),
  grafico_elipses,
  width = 11,
  height = 8
)

saveRDS(resultado, file.path(salida, "panellipse_resultado.rds"))

if (!is.null(resultado$coordinates)) {
  try(
    write.csv(
      as.data.frame(resultado$coordinates),
      file.path(salida, "panellipse_coordenadas.csv"),
      row.names = TRUE,
      fileEncoding = "UTF-8"
    ),
    silent = TRUE
  )
}

if (!is.null(resultado$hotelling)) {
  try(
    write.csv(
      as.data.frame(resultado$hotelling),
      file.path(salida, "panellipse_hotelling.csv"),
      row.names = TRUE,
      fileEncoding = "UTF-8"
    ),
    silent = TRUE
  )
}

resumen <- c(
  paste("Filas analizadas:", nrow(datos_completos)),
  paste("Tratamientos:", nlevels(datos_completos$tratamiento)),
  paste("Panelistas:", nlevels(datos_completos$panelista)),
  paste("Atributos:", length(atributos)),
  "Metodo: SensoMineR::panellipse",
  "Tipo de grafico: ggplot"
)
writeLines(resumen, file.path(salida, "resumen_elipses.txt"))
cat(paste(resumen, collapse = "\n"), "\n")

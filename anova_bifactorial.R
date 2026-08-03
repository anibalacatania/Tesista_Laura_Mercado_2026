.libPaths(c("/home/anibal/R/library", .libPaths()))

library(agricolae)

base <- "/home/anibal/Mis documentos/SENSORIAL/PANEL/2026/Tesista_Laura_Mercado_2026 (copy 1)/grupo_f1"
archivo <- file.path(base, "answers", "respuestas_unificadas.csv")
salida <- file.path(base, "resultados_anova_bifactorial")
dir.create(salida, showWarnings = FALSE, recursive = TRUE)

datos <- read.csv(
  archivo,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

datos$tratamiento <- factor(datos$tratamiento)
datos$panelista <- factor(datos$panelista)
atributos <- names(datos)[-(1:2)]
datos[atributos] <- lapply(
  datos[atributos],
  function(x) suppressWarnings(as.numeric(x))
)

resultados_anova <- vector("list", length(atributos))
names(resultados_anova) <- atributos
resultados_presentacion <- vector("list", length(atributos))
names(resultados_presentacion) <- atributos

for (atributo in atributos) {
  d <- datos[, c("tratamiento", "panelista", atributo)]
  names(d)[3] <- "respuesta"
  d <- d[complete.cases(d), ]

  # ANOVA bifactorial sin replicación. La interacción se usa como error residual.
  modelo <- aov(respuesta ~ tratamiento + panelista, data = d)
  tabla <- as.data.frame(summary(modelo)[[1]])
  tabla$efecto <- trimws(rownames(tabla))
  rownames(tabla) <- NULL
  names(tabla)[1:5] <- c(
    "grados_libertad", "suma_cuadrados", "media_cuadratica",
    "F", "p_valor"
  )
  tabla$atributo <- atributo
  tabla$significativo_0.05 <- ifelse(
    is.na(tabla$p_valor), "",
    ifelse(tabla$p_valor < 0.05, "Sí", "No")
  )
  resultados_anova[[atributo]] <- tabla[, c(
    "atributo", "efecto", "grados_libertad", "suma_cuadrados",
    "media_cuadratica", "F", "p_valor", "significativo_0.05"
  )]

  lsd <- LSD.test(modelo, trt = "tratamiento", alpha = 0.05, group = TRUE)
  grupos <- lsd$groups
  grupos$tratamiento <- rownames(grupos)
  rownames(grupos) <- NULL

  medias <- aggregate(
    respuesta ~ tratamiento,
    data = d,
    FUN = function(x) mean(x, na.rm = TRUE)
  )
  errores <- aggregate(
    respuesta ~ tratamiento,
    data = d,
    FUN = function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  )
  names(medias)[2] <- "media"
  names(errores)[2] <- "error_estandar"
  presentacion <- merge(medias, errores, by = "tratamiento", sort = FALSE)
  presentacion <- merge(
    presentacion,
    grupos[, c("tratamiento", "groups")],
    by = "tratamiento",
    all.x = TRUE,
    sort = FALSE
  )
  presentacion <- presentacion[match(levels(d$tratamiento), presentacion$tratamiento), ]
  presentacion$atributo <- atributo
  presentacion$media_error_grupo <- sprintf(
    "%.2f ± %.2f %s",
    presentacion$media,
    presentacion$error_estandar,
    presentacion$groups
  )
  resultados_presentacion[[atributo]] <- presentacion
}

anova_completo <- do.call(rbind, resultados_anova)
rownames(anova_completo) <- NULL
write.csv(
  anova_completo,
  file.path(salida, "anova_bifactorial_completo.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

anova_resumen <- anova_completo[
  anova_completo$efecto %in% c("tratamiento", "panelista"),
  c("atributo", "efecto", "F", "p_valor", "significativo_0.05")
]
write.csv(
  anova_resumen,
  file.path(salida, "anova_bifactorial_resumen.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

medias_lsd <- do.call(rbind, resultados_presentacion)
rownames(medias_lsd) <- NULL
write.csv(
  medias_lsd[, c(
    "atributo", "tratamiento", "media", "error_estandar",
    "groups", "media_error_grupo"
  )],
  file.path(salida, "medias_lsd_por_tratamiento.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

tabla_publicacion <- data.frame(tratamiento = levels(datos$tratamiento))
for (atributo in atributos) {
  x <- resultados_presentacion[[atributo]]
  tabla_publicacion[[atributo]] <- x$media_error_grupo[
    match(tabla_publicacion$tratamiento, x$tratamiento)
  ]
}
p_tratamiento <- anova_resumen[
  anova_resumen$efecto == "tratamiento",
  c("atributo", "p_valor")
]
fila_p <- data.frame(tratamiento = "P-valor tratamiento")
for (atributo in atributos) {
  fila_p[[atributo]] <- sprintf(
    "%.4f",
    p_tratamiento$p_valor[p_tratamiento$atributo == atributo]
  )
}
tabla_publicacion <- rbind(tabla_publicacion, fila_p)
write.csv(
  tabla_publicacion,
  file.path(salida, "resultados_bifactorial.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

resumen <- c(
  paste("Filas analizadas:", nrow(datos)),
  paste("Tratamientos:", nlevels(datos$tratamiento)),
  paste("Panelistas:", nlevels(datos$panelista)),
  paste("Atributos:", length(atributos)),
  "Modelo: aov(respuesta ~ tratamiento + panelista)",
  "Diseño: ANOVA bifactorial sin replicación",
  "Comparación de medias: LSD de Fisher, alfa = 0.05",
  "La interacción tratamiento:panelista no puede estimarse por haber una observación por celda."
)
writeLines(resumen, file.path(salida, "resumen_anova_bifactorial.txt"))
cat(paste(resumen, collapse = "\n"), "\n")

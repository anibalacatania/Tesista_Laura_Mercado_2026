# Cargar librerías necesarias
library(dplyr)
library(tidyr)



# Función para transformar el dataframe original
crear_stats <- function(datos) {
  # Verificar si la columna "Rep" está presente
  tiene_rep <- "Rep" %in% colnames(datos)

  # Transformar a formato largo
  df_long <- datos %>%
    pivot_longer(cols = 4:ncol(datos), names_to = "descriptor", values_to = "valor")

  if (tiene_rep) {
    # Calcular valoracion_valuador si hay repeticiones
    valoracion_valuador <- df_long %>%
      group_by(Producto, Valuador, descriptor) %>%
      summarise(valoracion_valuador = mean(valor, na.rm = TRUE), .groups = 'drop')

    # Calcular media, mediana y moda globales por producto
    estadisticas_globales <- df_long %>%
      group_by(Producto, descriptor) %>%
      summarise(
        media = mean(valor, na.rm = TRUE),
        mediana = median(valor, na.rm = TRUE),
        moda = as.numeric(names(sort(table(valor), decreasing = TRUE)[1])),
        .groups = 'drop'
      )

    # Unir las estadísticas globales con las valoraciones de los valuadores
    df_final <- valoracion_valuador %>%
      left_join(estadisticas_globales, by = c("Producto", "descriptor"))
  } else {
    # Calcular solo media, mediana y moda globales por producto si no hay repeticiones
    df_final <- df_long %>%
      group_by(Producto, descriptor) %>%
      summarise(
        media = mean(valor, na.rm = TRUE),
        mediana = median(valor, na.rm = TRUE),
        moda = as.numeric(names(sort(table(valor), decreasing = TRUE)[1])),
        .groups = 'drop'
      )
  }

  return(df_final)
}



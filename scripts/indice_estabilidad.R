library(dplyr)

calcular_indice <- function(df) {
  # Obtener los nombres de las columnas de variables (asumiendo que las primeras tres son Producto, sesion y Panelista)
  variable_columns <- names(df)[4:ncol(df)]

  # Inicializar un vector para almacenar los índices de cada variable
  indices <- numeric(length(variable_columns))
  names(indices) <- variable_columns

  # Número de productos y panelistas
  n <- length(unique(df$Producto))
  N <- length(unique(df$Valuador))

  # Iterar sobre cada columna de variable
  for (var in variable_columns) {
    # Agrupar por Producto y Panelista
    consistencia <- df %>%
      group_by(Producto, Valuador) %>%
      summarise(
        consistent = all(abs(diff(get(var)) )== 1),
        .groups = 'drop'
      )

    # Contar cuántas veces las variables fueron usadas de manera similar
    suma_consistencia <- sum(consistencia$consistent)

    # Calcular el índice para esta variable
    indice <- (suma_consistencia * 100) / (n * N)

    # Almacenar el índice
    indices[var] <- indice
  }

  # Convertir los índices en un data frame
  indices_df <- data.frame( variable_columns,Indice = indices)
  return(indices_df)
}





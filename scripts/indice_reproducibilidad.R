calcular_indice_reproducibilidad<- function(datos) {
  # Identificar los panelistas y productos distintos
  panelistas <- unique(datos$Valuador)
  productos <- unique(datos$Producto)

  # Inicializar un vector para almacenar los índices de reproducibilidad
  indices <- numeric(length(panelistas))

  # Número total de descriptores
  des <- ncol(datos) - 3 # Suponiendo que las primeras tres columnas son: Panelista, Producto, y Rep

  # Calcular el índice para cada panelista
  for (i in seq_along(panelistas)) {
    panelista <- panelistas[i]

    # Filtrar datos para el panelista actual
    datos_panelista <- subset(datos, Valuador == panelista)

    sumatoria <- 0

    for (producto in productos) {
      # Filtrar datos para el producto actual
      datos_producto <- subset(datos_panelista, Producto == producto)

      # Comparar las dos repeticiones
      rep1 <- datos_producto[datos_producto$Rep == 1, -c(1:3)]
      rep2 <- datos_producto[datos_producto$Rep == 2, -c(1:3)]

      # Calcular descomj incluyendo coincidencias y diferencias de ±1
      descomj <- sum(rep1 == rep2 | abs(rep1 - rep2) == 1, na.rm = TRUE)

      # Sumar descomj/des
      sumatoria <- sumatoria + (descomj / des)
    }

    # Calcular el índice para el panelista
    n <- length(productos)
    indices[i] <- sumatoria / n
  }

  # Crear un data frame con los resultados
  resultados <- data.frame(Valuador = panelistas, IndiceReproducibilidad = indices)

  return(resultados)
}

# Llamada a la función con los datos


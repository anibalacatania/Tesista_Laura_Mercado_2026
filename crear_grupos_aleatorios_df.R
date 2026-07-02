library(tidyr)
library(writexl)

# Función para crear grupos aleatorios de productos desde un data frame
crear_grupos_aleatorios_df <- function(data, num_productos) {
  # Convertir la primera columna del data frame en un vector de caracteres
  vinos <- data[[1]]
  
  # Barajar los productos aleatoriamente
  set.seed(005)  # Fijar semilla para reproducibilidad
  vinos <- sample(vinos)
  
  # Calcular el número total de productos y cuántos grupos se necesitan
  num_total <- length(vinos)
  num_grupos <- ceiling(num_total / num_productos)
  
  # Crear los grupos añadiendo NA en caso de que falten productos para completar el grupo
  grupos <- split(vinos, ceiling(seq_along(vinos) / num_productos))
  
  # Verificar si el último grupo tiene menos productos que los especificados
  # Si es así, rellenar con NA
  for (i in seq_along(grupos)) {
    if (length(grupos[[i]]) < num_productos) {
      grupos[[i]] <- c(grupos[[i]], rep(NA, num_productos - length(grupos[[i]])))
    }
  }
  
  # Combinar los grupos en un data frame
  grupos_df <- as.data.frame(do.call(cbind, grupos))
  
  # Renombrar las columnas como f1, f2, f3, etc.
  colnames(grupos_df) <- paste0("f", seq_len(ncol(grupos_df)))
  
  # Escribir los grupos a un archivo CSV
  write.csv(grupos_df, "grupos_aleatorios.csv", row.names = FALSE)
  
  # También escribir en formato Excel
  write_xlsx(grupos_df, "grupos_aleatorios.xlsx")
  
  return(grupos_df)
}

# Ejemplo de uso


# Crear grupos aleatorios desde el data frame con 4 productos por grupo
grupos_creados <- crear_grupos_aleatorios_df(vinos1, num_productos = 4)
write_xlsx(grupos_creados, "grupos_aleatorios.xlsx")

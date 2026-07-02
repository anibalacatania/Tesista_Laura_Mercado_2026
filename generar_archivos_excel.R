library(tidyverse)
library(plyr)     # Para mapvalues
library(crossdes) # Para la función williams

# Función para generar archivos CSV a partir de un data frame con columnas f1, f2, f3...
generar_archivos_csv <- function(grupos_df, n_copas, ID_random = TRUE) {
  
  # Iterar sobre las columnas del data frame (f1, f2, f3, etc.)
  for (col in colnames(grupos_df)) {
    g2 <- as.vector(grupos_df[[col]])
    
    # Seleccionar copas aleatorias para cada panelista
    set.seed(654)
    s1 <- sample(g2, n_copas, replace = FALSE)
    
    set.seed(981)
    s2 <- sample(g2, n_copas, replace = FALSE)
    
    set.seed(711)
    s3 <- sample(g2, n_copas, replace = FALSE)
    
    set.seed(290)
    s4 <- sample(g2, n_copas, replace = FALSE)
    
    # Generar el diseño de Williams
    dam <- williams(n_copas)
    
    # Asignar los productos a las posiciones del diseño de Williams
    dam2 <- mapvalues(dam, c(1:n_copas), s1)
    dam3 <- mapvalues(dam, c(1:n_copas), s2)
    dam4 <- mapvalues(dam, c(1:n_copas), s3)
    dam5 <- mapvalues(dam, c(1:n_copas), s4)
    
    # Combinar los resultados en un solo data frame
    rep1 <- rbind(dam2, dam3, dam4, dam5)
    rep1 <- t(rep1)
    rep1 <- as.data.frame(rep1)
    copas <- rownames(rep1)
    rep1 <- cbind(copas, rep1)
    
    # Reorganizar el data frame en formato largo
    rep1 <- gather(rep1, panelista, copas)
    
    # Generar códigos únicos de tres dígitos para la columna ID
    set.seed(123 + which(colnames(grupos_df) == col))  # Cambiar la semilla según el grupo
    unique_ids <- sample(100:999, length(unique(rep1$copas)), replace = FALSE)
    
    # Asignar los códigos e identificadores a cada copa
    rep2 <- rep1 %>%
      group_by(copas) %>%
      mutate(ID = as.character(if (ID_random) {
        unique_ids[match(copas, unique(rep1$copas))]
      } else {
        unique_ids[1]  # Usar el mismo ID si ID_random es FALSE
      }),
      Muestra = as.character(match(copas, g2)))  # Asignar el número de muestra según el orden original
    
    # Filtrar filas donde "NombreProducto" es NA
    rep2 <- rep2 %>%
      filter(!is.na(copas))
    
    # Renombrar y reordenar columnas
    rep2 <- rep2 %>%
      dplyr::rename(Valuador = panelista,
             CodigoProducto = ID,
             NombreProducto = copas,
             Muestra = Muestra) %>%
      dplyr::select(Valuador, Muestra, NombreProducto, CodigoProducto)  # Reordenar columnas
    
    # Crear una carpeta con el mismo nombre que el archivo CSV
    folder_name <- paste0("grupo_", col)
    if (!dir.exists(folder_name)) {
      dir.create(folder_name)
    }
    
    # Guardar el archivo CSV dentro de la carpeta
    output_file <- file.path(folder_name, paste0("grupo_", col, ".csv"))
    write_csv(rep2, output_file)
    
    # Filtrar datos para el archivo vinos_fX.csv, ordenar por Muestra y renombrar columnas
    vinos_data <- rep2 %>%
      dplyr::filter(Valuador == "V1") %>%
      dplyr::select(NombreProducto, Muestra) %>%
      arrange(Muestra) %>%  # Ordenar por la columna Muestra
      dplyr::rename(Copa = NombreProducto, CodigoProducto = Muestra)  # Renombrar columnas
    
    # Guardar el archivo vinos_fX.csv dentro de la carpeta
    vinos_file <- file.path(folder_name, paste0("vinos_", col, ".csv"))
    write_csv(vinos_data, vinos_file)
    
    # Filtrar datos para el archivo orden_servicio_fX.csv, incluir CodigoProducto
    orden_servicio_data <- rep2 %>%
      filter(Valuador == "V1") %>%
      dplyr::select(NombreProducto, Muestra, CodigoProducto) %>%
      arrange(Muestra)  # Ordenar por la columna Muestra
    
    # Guardar el archivo orden_servicio_fX.csv dentro de la carpeta
    orden_servicio_file <- file.path(folder_name, paste0("orden_servicio_", col, ".csv"))
    write_csv(orden_servicio_data, orden_servicio_file)
  }
}

generar_archivos_csv(grupos_creados, n_copas=4, ID_random = TRUE) 

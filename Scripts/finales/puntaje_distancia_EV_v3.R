setwd("C:/Users/pcusu/Desktop")

# =======================================
# 1) Librerías
# =======================================
library(sf)
library(sfnetworks)
library(tidygraph)
library(tidyverse)
library(units)
library(igraph)
library(RColorBrewer) 
library(tmap)

# =======================================
# 2) Cargar red vial y calcular pesos
# =======================================
if (!exists("net")) {
  calles_ba <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/jefatura-de-gabinete-de-ministros/calles/callejero.geojson")
  net <- as_sfnetwork(calles_ba, directed = FALSE)
}
net <- net %>%
  activate("edges") %>%
  mutate(
    length_m = st_length(geometry),
    time_PcD = as.numeric(length_m) / (2.5 * 1000 / 3600),
    time_PsD = as.numeric(length_m) / (5 * 1000 / 3600)
  )
# =======================================
# 3) Orígenes y destinos
# =======================================
origenes <- st_read("C:/Users/pcusu/Desktop/Accesibilidad_CABA/Datos/Datos_indice/centroides_MZ.geojson")
destinos <- st_read("C:/Users/pcusu/Desktop/Accesibilidad_CABA/Datos/Datos_indice/esp_verdes.geojson")
# =======================================
# 4) Snap de puntos a los nodos de la red
# =======================================
snap_points <- function(points, nodes) st_nearest_feature(points, nodes)
nodos <- net %>% activate("nodes") %>% st_as_sf()
origenes$nearest_node <- snap_points(origenes, nodos)
destinos$nearest_node <- snap_points(destinos, nodos)
destinos <- destinos %>%
  distinct(nearest_node, .keep_all = TRUE)
# =======================================
# 5) Crear grafo de igraph con los pesos de tiempo
# =======================================
grafo <- net %>% as.igraph()
E(grafo)$weight_PcD <- net %>% activate("edges") %>% pull(time_PcD)
E(grafo)$weight_PsD <- net %>% activate("edges") %>% pull(time_PsD)
# =======================================
# 6) Calcular tiempo promedio a los 5 destinos más cercanos
# =======================================
calc_min_time <- function(from, destino_nodes, weights, N = 5) {
  dist <- igraph::distances(grafo, v = from, to = destino_nodes, weights = weights)
  dist_sorted <- sort(dist)
  mean5 <- mean(dist_sorted[1:min(N, length(dist_sorted))])
  return(mean5)
}

origenes <- origenes %>%
  mutate(
    time_PcD_s = map_dbl(nearest_node, ~ calc_min_time(.x, destinos$nearest_node, E(grafo)$weight_PcD)),
    time_PsD_s = map_dbl(nearest_node, ~ calc_min_time(.x, destinos$nearest_node, E(grafo)$weight_PsD))
  )

origenes <- origenes %>%
  mutate(
    time_PcD_min = time_PcD_s / 60,
    time_PsD_min = time_PsD_s / 60
  )
# =======================================
# 7) Lógica de puntaje y agregación por moda para evitar valores sueltos
# =======================================
# FUNCIÓN ACTUALIZADA CON UMBRALES DE 10 Y 20 MINUTOS 
calcular_puntaje_gradual <- function(tiempo_min) {
  case_when(
    tiempo_min <= 10 ~ 1.00,
    tiempo_min <= 15 ~ 0.50, 
    tiempo_min <= 20 ~ 0.25,
    TRUE ~ 0.00 
  )
}
origenes <- origenes %>%
  mutate(
    puntaje_PcD = calcular_puntaje_gradual(time_PcD_min),
    puntaje_PsD = calcular_puntaje_gradual(time_PsD_min)
  )
calcular_moda <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 0) return(NA_real_)
  ux[which.max(tabulate(match(x, ux)))]
}
origenes_unique <- origenes %>%
  st_drop_geometry() %>%
  select(CFRM, puntaje_PcD, puntaje_PsD) %>%
  group_by(CFRM) %>%
  summarise(
    puntaje_PcD = calcular_moda(puntaje_PcD),
    puntaje_PsD = calcular_moda(puntaje_PsD)
  )
# =======================================
# 8) Unir resultados
# =======================================
manzanas <- st_read("C:/Users/pcusu/Desktop/Accesibilidad_CABA/Datos/Datos_indice/manzanas_geom.geojson") %>% st_make_valid()
manzanas_final <- manzanas %>%
  left_join(origenes_unique, by = "CFRM")

if("ID" %in% names(manzanas_final)) manzanas_final <- select(manzanas_final, -ID)
if("perimetro" %in% names(manzanas_final)) manzanas_final <- select(manzanas_final, -perimetro)
if("area" %in% names(manzanas_final)) manzanas_final <- select(manzanas_final, -area)

# =======================================
# 9) Tabla Resumen Completa
# =======================================
crear_tabla_resumen <- function(datos_sf, perfil) {
  
  col_puntaje <- paste0("puntaje_", perfil)
  velocidad_mpm <- if (perfil == "PcD") (2.5 * 1000) / 60 else (5 * 1000) / 60
  
  categorias_tiempo <- c("1 - Menos de 10 min", "0,5 - Entre 10 y 15 min", "0,25 - Entre 15 y 20 min", "0 - Mas de 20 min")
  umbrales_min <- c(10, 15, 20, NA)
  
  conteo_manzanas <- datos_sf %>%
    st_drop_geometry() %>%
    count(.data[[col_puntaje]], name = "N_Manzanas") %>%
    rename(Puntaje = .data[[col_puntaje]])
  
  resumen <- tibble(
    Categoria = categorias_tiempo,
    Puntaje = c(1.00, 0.50, 0.25, 0.00),
    `Tiempo (min)` = umbrales_min, 
    `Distancia Aprox (m)` = c(round(10 * velocidad_mpm), round(15 * velocidad_mpm), round(20 * velocidad_mpm), NA)
  ) %>%
    left_join(conteo_manzanas, by = "Puntaje") %>%
    mutate(N_Manzanas = replace_na(N_Manzanas, 0)) # Rellenar con 0 si una categoría no tiene manzanas
  
  return(resumen)
}

tabla_pcd <- crear_tabla_resumen(manzanas_final, "PcD")
tabla_psd <- crear_tabla_resumen(manzanas_final, "PsD")

# =======================================
# 10.3) Combinar y exportar tabla resumen
# =======================================

# Combinar las tablas de PcD y PsD en una sola para una comparación fácil
tabla_combinada <- tabla_pcd %>%
  # Seleccionar y renombrar columnas para claridad antes de unir
  select(Categoria, Puntaje, `Distancia Aprox (m)`, N_Manzanas) %>%
  rename(
    `Distancia (m) PcD` = `Distancia Aprox (m)`,
    `N Manzanas PcD` = N_Manzanas
  ) %>%
  # Unir con la tabla de PsD usando la Categoría como llave
  left_join(
    select(tabla_psd, Categoria, `Distancia Aprox (m)`, N_Manzanas),
    by = "Categoria"
  ) %>%
  # Renombrar las columnas de la tabla PsD
  rename(
    `Distancia (m) PsD` = `Distancia Aprox (m)`,
    `N Manzanas PsD` = N_Manzanas
  )

# Imprimir la tabla combinada para verificar
print(tabla_combinada)

write.csv(tabla_combinada, "C:/Users/pcusu/Desktop/puntaje_tabla_EV.csv", row.names = FALSE, fileEncoding = "UTF-8")

# =======================================
# 10) Función de Mapa Corregida
# =======================================
crear_mapa_accesibilidad <- function(datos_sf, perfil, tabla_resumen) {
  
  col_puntaje <- paste0("puntaje_", perfil)
  titulo_mapa <- paste("Acceso a Espacios Verdes - Perfil", perfil)
  
  # Preparar etiquetas para la leyenda (Categoría y Conteo)
  etiquetas_leyenda <- paste0(tabla_resumen$Categoria, " [", tabla_resumen$N_Manzanas, "]")
  
  # Definir 4 colores específicos para las 4 categorías
  colores <- c("#2b83ba", "#abdda4", "#fdae61", "#d7191c") # Azul, Verde claro, Naranja, Rojo
  
  # Convertir la columna de puntaje a un factor ordenado para controlar la leyenda
  datos_sf <- datos_sf %>%
    mutate(
      categoria_puntaje = factor(.data[[col_puntaje]],
                                 levels = c(1.0, 0.5, 0.25, 0.0),
                                 ordered = TRUE)
    )
  
  mapa <- tm_shape(datos_sf) +
    tm_polygons(
      col = "categoria_puntaje",
      palette = colores,
      title = paste("Puntaje y Total de Manzanas [", nrow(datos_sf), "]"),
      labels = etiquetas_leyenda,
      colorNA = "grey" # Color para manzanas sin datos
    ) +
    tm_layout(
      title = titulo_mapa,
      frame = FALSE,
      legend.title.size = 0.9,
      legend.text.size = 0.7
    )
  
  return(mapa)
}

# =======================================
# 11) Visualización y Exportación
# =======================================
# Crear y mostrar los mapas usando la nueva función
mapa_pcd <- crear_mapa_accesibilidad(manzanas_final, "PcD", tabla_pcd)
print(mapa_pcd)

mapa_psd <- crear_mapa_accesibilidad(manzanas_final, "PsD", tabla_psd)
print(mapa_psd)

# Exportar el resultado final a GeoJSON
st_write(manzanas_final, "C:/Users/pcusu/Desktop/puntaje_distancia_EV_10_20min.geojson", delete_layer = TRUE)
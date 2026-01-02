# =======================================
# 1) Librerías
# =======================================
library(sf)
library(sfnetworks)
library(tidygraph)
library(tidyverse)
library(units)
library(igraph)

# =======================================
# 2) Cargar red vial
# =======================================
calles_ba <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/jefatura-de-gabinete-de-ministros/calles/callejero.geojson")
net <- as_sfnetwork(calles_ba, directed = FALSE)

# =======================================
# 3) Calcular longitudes y pesos para cada perfil (network)
# =======================================
net <- net %>%
  activate("edges") %>%
  mutate(
    length_m = st_length(geometry),
    time_PcD = as.numeric(length_m) / (2.5 * 1000 / 3600),  # 2.5 km/h
    time_PsD = as.numeric(length_m) / (5 * 1000 / 3600)     # 5 km/h
  )

# =======================================
# 4) Orígenes y destinos (EQUIPAMIENTOS EDUCATIVOS)
# =======================================
origenes <- st_read("C:/Users/pcusu/Desktop/Accesibilidad_CABA/Datos/Datos_indice/centroides_MZ.geojson")
destinos <- st_read("C:/Users/pcusu/Desktop/Accesibilidad_CABA/Datos/Datos_indice/equip_educ.geojson")

# =======================================
# 5) Snap a nodos de la red
# =======================================
snap_points <- function(points, nodes) st_nearest_feature(points, nodes)
nodos <- net %>% activate("nodes") %>% st_as_sf()

origenes$nearest_node <- snap_points(origenes, nodos)
destinos$nearest_node <- snap_points(destinos, nodos)

# Evitar duplicados en nodos destino
destinos <- destinos %>%
  distinct(nearest_node, .keep_all = TRUE)

# =======================================
# 6) Grafo igraph con pesos de tiempo
# =======================================
grafo <- net %>% as.igraph()
E(grafo)$weight_PcD <- net %>% activate("edges") %>% pull(time_PcD)
E(grafo)$weight_PsD <- net %>% activate("edges") %>% pull(time_PsD)

# =======================================
# 7) Distancias reales a destinos más cercanos
# =======================================

# Función: tiempo real del nodo origen a todos los destinos
calc_min_time <- function(from, destino_nodes, weights, N = 5) {
  dist <- igraph::distances(grafo, v = from, to = destino_nodes, weights = weights)
  dist_sorted <- sort(dist)
  mean5 <- mean(dist_sorted[1:min(N, length(dist_sorted))])
  return(mean5)
}

# Calcular tiempo promedio a los 5 equipamientos educativos más cercanos
origenes <- origenes %>%
  mutate(
    time_PcD_s = map_dbl(nearest_node, ~ calc_min_time(.x, destinos$nearest_node, E(grafo)$weight_PcD)),
    time_PsD_s = map_dbl(nearest_node, ~ calc_min_time(.x, destinos$nearest_node, E(grafo)$weight_PsD))
  )

# Convertir a minutos
origenes <- origenes %>%
  mutate(
    time_PcD_min = time_PcD_s / 60,
    time_PsD_min = time_PsD_s / 60
  )

# =======================================
# 8) Puntaje 0/1 según tiempo umbral de 8 min
# =======================================
origenes <- origenes %>%
  mutate(
    acceso_PcD = if_else(time_PcD_min <= 8, 1L, 0L),
    acceso_PsD = if_else(time_PsD_min <= 8, 1L, 0L)
  )

# =======================================
# 9) Agregar por manzana
# =======================================
origenes_unique <- origenes %>%
  st_drop_geometry() %>%
  select(CFRM, time_PcD_min, time_PsD_min, acceso_PcD, acceso_PsD) %>%
  group_by(CFRM) %>%
  summarise(
    time_PcD_min = mean(time_PcD_min, na.rm = TRUE),
    time_PsD_min = mean(time_PsD_min, na.rm = TRUE),
    acceso_PcD = mean(acceso_PcD, na.rm = TRUE),
    acceso_PsD = mean(acceso_PsD, na.rm = TRUE)
  )

# =======================================
# 10) Guardar resultados
# =======================================
manzanas <- st_read("C:/Users/pcusu/Desktop/Accesibilidad_CABA/Datos/Datos_indice/manzanas_geom.geojson") %>% st_make_valid()

manzanas_final <- manzanas %>%
  left_join(origenes_unique, by = "CFRM") %>%
  select(-ID, -perimetro)

# =======================================
# 11) Resumen rápido
# =======================================
manzanas_final %>%
  st_drop_geometry() %>%
  summarise(
    Total = n(),
    PcD_1 = sum(acceso_PcD, na.rm = TRUE),
    PcD_0 = sum(1 - acceso_PcD, na.rm = TRUE),
    PsD_1 = sum(acceso_PsD, na.rm = TRUE),
    PsD_0 = sum(1 - acceso_PsD, na.rm = TRUE)
  )

st_write(manzanas_final, "puntaje_tiempo_equipamientos_educativos.geojson", delete_layer = TRUE)

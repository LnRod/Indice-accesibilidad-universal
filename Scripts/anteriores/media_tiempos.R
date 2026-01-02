library(sf)
library(dplyr)
library(dodgr)
library(tidyr)
library(readr)

# Manzanas y centroides
manzanas <- st_read("C:/Users/pcusu/Desktop/Accesibilidad_CABA/Datos/Datos_indice/manzanas.geojson")
manzanas_centroides <- st_read("C:/Users/pcusu/Desktop/Accesibilidad_CABA/Datos/Datos_indice/centroides_MZ.geojson") %>% st_make_valid()

# Origen 
transporte <- st_read("C:/Users/pcusu/Desktop/transporte/transporte_acceso.geojson")
#equip_asis_centroides <- st_centroid(equip_asis)

# Red vial
streetnet_CABA <- readRDS('C:/Users/pcusu/Desktop/Accesibilidad_CABA/scripts/graph_streetnet_CABA.rds')

# Añadir id para unir luego
manzanas_centroides <- manzanas_centroides %>% mutate(id = row_number())
destino_centroides <- transporte %>% mutate(id = row_number())

# Extraer coordenadas x,y y crear data.frames solo con coords
manzanas_coords <- manzanas_centroides %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(id, x, y)

destino_coords <- destino_centroides %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(id, x, y)

# ===============================
# FILTRAR PUNTOS FUERA DEL ÁREA DEL GRAFO (POR SI ACASO)
# ===============================

# Calcular bbox real del grafo (usar columnas correctas)
bbox_grafo <- c(
  xmin = min(c(streetnet_CABA$from_lon, streetnet_CABA$to_lon), na.rm = TRUE),
  xmax = max(c(streetnet_CABA$from_lon, streetnet_CABA$to_lon), na.rm = TRUE),
  ymin = min(c(streetnet_CABA$from_lat, streetnet_CABA$to_lat), na.rm = TRUE),
  ymax = max(c(streetnet_CABA$from_lat, streetnet_CABA$to_lat), na.rm = TRUE)
)

# Filtrar manzanas y espacios culturales dentro del bbox
manzanas_coords <- manzanas_coords %>%
  filter(x >= bbox_grafo["xmin"], x <= bbox_grafo["xmax"],
         y >= bbox_grafo["ymin"], y <= bbox_grafo["ymax"])

destino_coords <- destino_coords %>%
  filter(x >= bbox_grafo["xmin"], x <= bbox_grafo["xmax"],
         y >= bbox_grafo["ymin"], y <= bbox_grafo["ymax"])

# ===============================
# CALCULAR MATRIZ DE TIEMPOS EN MINUTOS
# ===============================

# Solo extraer x,y para dodgr
from_coords <- manzanas_coords %>% select(x, y)
to_coords <- destino_coords %>% select(x, y)

time_matrix <- dodgr_times(graph = streetnet_CABA, from = from_coords, to = to_coords)
time_matrix_min <- time_matrix / 60

# ===============================
# ARMAR FORMATO LARGO Y TOP 5 MÁS CERCANOS
# ===============================

# Renombrar columnas para pivot_longer
colnames(time_matrix_min) <- paste0("to_", seq_len(ncol(time_matrix_min)))

# Crear tabla formato largo: desde manzana (from_id) a (to_id)
time_long <- as.data.frame(time_matrix_min) %>%
  mutate(from_id = manzanas_coords$id) %>%
  pivot_longer(cols = -from_id, names_to = "to_id", values_to = "time_min") %>%
  filter(!is.na(time_min))

# Extraer número del "to_id" (que corresponde a índices en EC_coords)
time_long <- time_long %>%
  mutate(to_id = parse_number(to_id))

# Obtener top 5 espacios culturales más cercanos por manzana
nearest_places <- time_long %>%
  group_by(from_id) %>%
  slice_min(order_by = time_min, n = 5) %>%
  ungroup()

# ===============================
# CALCULAR PUNTAJE DE TIEMPO PROMEDIO A LOS 5 MÁS CERCANOS
# ===============================

mean_nearest_places <- nearest_places %>%
  group_by(from_id) %>%
  summarise(mean_time_min_top5 = mean(time_min, na.rm = TRUE))

media_espacio <- mean(mean_nearest_places$mean_time_min_top5)

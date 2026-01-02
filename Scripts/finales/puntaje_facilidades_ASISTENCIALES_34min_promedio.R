# ==========================================
# 1) Librerías
# ==========================================
library(sf)
library(dplyr)
library(nngeo)
library(units)
library(tibble)
library(tidyr)
library(stringr)
library(tmap) # Para visualización de mapas
library(RColorBrewer) # Para paletas de colores

# ==========================================
# 2) Cargar datos y calcular puntaje de destinos
# ==========================================
# *** DATOS DE DESTINO: Equipamiento Asistencial (equip_asis.geojson) ***
# NOTA: La ruta se ha actualizado a 'equip_asis_5.geojson' según su output.
destinos <- st_read("C:/Users/pcusu/Desktop/Accesibilidad_CABA/Datos/Datos_indice/equip_asis_5.geojson")
manzanas <- st_read("C:/Users/pcusu/Desktop/Accesibilidad_CABA/Datos/Datos_indice/manzanas_geom.geojson") %>% st_make_valid()

# *** CÁLCULO DE PUNTAJE DE DESTINOS (Lógica de Facilidades ASISTENCIALES) ***
destinos_puntaje <- destinos %>%
  mutate(
    # Lógica de puntaje para Equipamiento Asistencial/Social (SUMA DE PESOS)
    puntaje_base = case_when(
      entidad == "centros_dia" ~ 0.25,
      entidad == "Centros_dia" ~ 0.25,
      entidad == "hogares_paradores" ~ 0.50,
      entidad == "polideportivos" ~ 0.50,
      entidad == "comisarias" ~ 0.50,
      TRUE ~ 0.00 # Otros no puntúan
    ),
    dest_idx = row_number() # ID único para st_nn
  ) %>%
  filter(!st_is_empty(.)) %>% # Limpiar geometrías nulas/vacías
  filter(puntaje_base > 0)

# ==========================================
# 3) Estandarizar ID de manzanas y Proyección CRS
# ==========================================
if ("CFRM" %in% names(manzanas)) {
  manzanas <- manzanas %>% rename(ID_mza = CFRM)
} else if ("ID" %in% names(manzanas)) {
  manzanas <- manzanas %>% rename(ID_mza = ID)
} else {
  stop("No se encuentra clave única de manzanas (ID o CFRM)")
}

# Transformar a CRS métrico para cálculos de distancia (WGS 84 / UTM zone 20S)
manzanas_proj <- st_transform(manzanas, 32720)
destinos_proj <- st_transform(destinos_puntaje, 32720)

# El origen para st_nn es el centroide de la geometría de la manzana.
origenes_proj <- st_centroid(manzanas_proj, of_largest_polygon = TRUE)

# ==========================================
# 4) Definir velocidades y umbrales de distancia (Umbral máximo: 34 minutos)
# ==========================================
# Velocidades de los perfiles (metros por minuto)
velocidad_mpm_pcd <- (2.5 * 1000) / 60 
velocidad_mpm_psd <- (5 * 1000) / 60

# Umbral de tiempo MÁXIMO (FILTRO BINARIO): 34 minutos 
tiempo_max_min <- 34 
distancia_max_pcd <- round(tiempo_max_min * velocidad_mpm_pcd) 
distancia_max_psd <- round(tiempo_max_min * velocidad_mpm_psd) 

# Encontrar los 5 vecinos más cercanos dentro del umbral de 34 minutos
# Se usa 'origenes_proj' (centroides proyectados) para el cálculo de distancia
vecinos_PcD <- st_nn(origenes_proj, destinos_proj, k = 5, maxdist = distancia_max_pcd, returnDist = FALSE)
vecinos_PsD <- st_nn(origenes_proj, destinos_proj, k = 5, maxdist = distancia_max_psd, returnDist = FALSE)

# ==========================================
# 5) Función para acumular puntajes (PROMEDIO de los 5 destinos)
# ==========================================
# Función que calcula el promedio (MEAN)
acumular_puntajes <- function(origenes_sf, vecinos_list) {
  # Usamos el dataframe de manzanas proyectadas ya que tienen el ID_mza en el mismo orden que los centroides
  origenes_df <- st_drop_geometry(manzanas_proj) %>% select(ID_mza)
  
  valid_indices <- lengths(vecinos_list) > 0
  
  # Repetir ID_mza por cada vecino encontrado
  puntajes_temp <- tibble(
    ID_mza = rep(origenes_df$ID_mza[valid_indices], lengths(vecinos_list)[valid_indices]),
    dest_idx = unlist(vecinos_list[valid_indices])
  ) %>%
    left_join(st_drop_geometry(destinos_puntaje) %>% select(dest_idx, puntaje_base), by = "dest_idx") %>%
    # Usa MEAN (promedio) del puntaje base de los K=5 (o menos) vecinos encontrados
    group_by(ID_mza) %>%
    summarise(puntaje_acumulado = mean(puntaje_base, na.rm = TRUE)) %>%
    ungroup()
  
  return(puntajes_temp)
}

# Aplicar la función para cada perfil
puntajes_PcD <- acumular_puntajes(origenes_proj, vecinos_PcD)
puntajes_PsD <- acumular_puntajes(origenes_proj, vecinos_PsD)

# ==========================================
# 6) Unir puntajes y redondear
# ==========================================
manzanas_final <- manzanas %>%
  left_join(puntajes_PcD, by = "ID_mza") %>%
  left_join(puntajes_PsD, by = "ID_mza") %>%
  rename(
    # Nombres actualizados para ASISTENCIAL
    puntaje_fac_asistencial_PcD = puntaje_acumulado.x,
    puntaje_fac_asistencial_PsD = puntaje_acumulado.y
  ) %>%
  mutate(
    # Reemplazar NA (fuera de rango accesible/34 min) con 0.00
    puntaje_fac_asistencial_PcD = replace_na(puntaje_fac_asistencial_PcD, 0.00),
    puntaje_fac_asistencial_PsD = replace_na(puntaje_fac_asistencial_PsD, 0.00)
  ) %>%
  # Forzar dos decimales en los puntajes finales
  mutate(
    puntaje_fac_asistencial_PcD = round(puntaje_fac_asistencial_PcD, 2),
    puntaje_fac_asistencial_PsD = round(puntaje_fac_asistencial_PsD, 2)
  ) %>%
  select(ID_mza, geometry, puntaje_fac_asistencial_PcD, puntaje_fac_asistencial_PsD)

# ==========================================
# 7) Generación de Tabla de Rangos
# ==========================================
# Se mantiene la misma lógica de rangos (0.00, < 0.25, < 0.50, < 0.75, <= 1.00)
crear_tabla_rangos_promedio <- function(datos_sf, col_puntaje, perfil) {
  
  # CORRECCIÓN: Mantener la geometría aquí (tabla_resumen_sf)
  tabla_resumen_sf <- datos_sf %>%
    mutate(
      Rango = case_when(
        .data[[col_puntaje]] == 0.00 ~ "0.00",
        .data[[col_puntaje]] < 0.25 ~ "< 0.25",
        .data[[col_puntaje]] < 0.50 ~ "< 0.50",
        .data[[col_puntaje]] < 0.75 ~ "< 0.75",
        .data[[col_puntaje]] <= 1.00 ~ "<= 1.00",
        TRUE ~ "> 1.00" # Se deja la categoría de puntaje teórico > 1.00 aunque no debería ocurrir con un promedio de pesos < 1.00
      ),
      # Calcular el puntaje máximo real
      max_score = max(.data[[col_puntaje]], na.rm = TRUE)
    )
  
  # Usar la versión sin geometría para el cálculo de conteo
  tabla_resumen_df <- st_drop_geometry(tabla_resumen_sf)
  
  etiquetas_orden <- c(
    "0.00",
    "< 0.25",
    "< 0.50",
    "< 0.75",
    "<= 1.00",
    "> 1.00"
  )
  
  # Definir el nombre de la columna de conteo para usarlo de forma dinámica
  nombre_col_conteo <- paste0("N_Manzanas_", perfil)
  
  tabla_final <- tabla_resumen_df %>%
    count(Rango, name = nombre_col_conteo) %>%
    right_join(tibble(Rango = etiquetas_orden), by = "Rango") %>%
    mutate(
      Rango = factor(Rango, levels = etiquetas_orden), 
      across(starts_with("N_Manzanas"), replace_na, 0)
    ) %>%
    # Uso dinámico corregido del conteo
    filter(.data[[nombre_col_conteo]] > 0 | Rango != "> 1.00") %>%
    arrange(Rango)
  
  return(list(
    tabla = tabla_final, 
    max_score = round(max(tabla_resumen_df$max_score, na.rm = TRUE), 2),
    datos_categorizados = tabla_resumen_sf # Ahora devuelve el objeto SF con Rango
  ))
}

# Generar resultados
resultados_pcd <- crear_tabla_rangos_promedio(manzanas_final, "puntaje_fac_asistencial_PcD", "PcD")
resultados_psd <- crear_tabla_rangos_promedio(manzanas_final, "puntaje_fac_asistencial_PsD", "PsD")

# Combinar tablas
tabla_combinada <- resultados_pcd$tabla %>%
  left_join(resultados_psd$tabla, by = "Rango")

# Imprimir el resumen de umbrales y puntajes máximos
print(tibble(
  Perfil = c("PcD (2.5 km/h)", "PsD (5 km/h)"),
  `Tiempo Máx (min)` = tiempo_max_min,
  `Distancia Máx (m)` = c(distancia_max_pcd, distancia_max_psd),
  `Puntaje Máximo (PcD)` = resultados_pcd$max_score,
  `Puntaje Máximo (PsD)` = resultados_psd$max_score
))

# Imprimir la tabla de rangos combinada
print(tabla_combinada)

# ==========================================
# 8) Mapa y Exportación
# ==========================================

# 8.1 Función para crear el mapa
crear_mapa_facilidades_perfil <- function(datos_sf, col_puntaje, perfil, tabla_resumen) {
  
  # Preparar etiquetas y datos categorizados
  # 'datos_sf' ya es el objeto SF con la columna Rango
  df_mapa <- datos_sf %>%
    mutate(
      Rango = factor(Rango, levels = tabla_resumen$Rango), # Asegurar el orden del factor
      # Usar el nombre de columna dinámico para el conteo de manzanas
      N_Manzanas = tabla_resumen[[paste0("N_Manzanas_", perfil)]][match(Rango, tabla_resumen$Rango)]
    ) 
  # CORRECCIÓN: Se elimina la llamada errónea a st_as_sf() ya que df_mapa ya es un objeto SF
  
  # Preparar etiquetas de leyenda (Rango + Conteo)
  etiquetas_leyenda <- paste0(df_mapa$Rango, " [", df_mapa$N_Manzanas, "]")
  
  # Definir colores para los rangos (Rojo a Azul para bajo a alto, más gris para 0)
  colores <- c("#d73027", "#fc8d59", "#fee090", "#e0f3f8", "#91bfdb") # RdYlBu (5 clases)
  
  mapa <- tm_shape(df_mapa) +
    tm_polygons(
      col = "Rango",
      palette = colores[1:length(unique(df_mapa$Rango))],
      title = paste("Puntaje Facilidades -", perfil),
      labels = unique(etiquetas_leyenda),
      colorNA = "grey" 
    ) +
    tm_layout(
      title = paste("Facilidades Asistenciales (34 min, Promedio K=5) - Perfil", perfil),
      frame = FALSE,
      legend.title.size = 0.9,
      legend.text.size = 0.7
    )
  
  return(mapa)
}

# Crear y mostrar mapas
manzanas_categorizadas_pcd <- resultados_pcd$datos_categorizados
mapa_pcd <- crear_mapa_facilidades_perfil(manzanas_categorizadas_pcd, "puntaje_fac_asistencial_PcD", "PcD", tabla_combinada)
print(mapa_pcd)

manzanas_categorizadas_psd <- resultados_psd$datos_categorizados
mapa_psd <- crear_mapa_facilidades_perfil(manzanas_categorizadas_psd, "puntaje_fac_asistencial_PsD", "PsD", tabla_combinada)
print(mapa_psd)


# 8.2 Exportar capa final GeoJSON
st_write(manzanas_final, "C:/Users/pcusu/Desktop/puntaje_facilidades_ASISTENCIAL_34min_promedio.geojson", delete_dsn = TRUE)

# 8.3 Exportar tabla de rangos CSV
write.csv(tabla_combinada, "C:/Users/pcusu/Desktop/resumen_rangos_facilidades_ASISTENCIAL_34min_promedio.csv", row.names = FALSE, fileEncoding = "UTF-8")

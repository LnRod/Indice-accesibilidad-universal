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

# ==========================================
# 2) Cargar datos y calcular puntaje de destinos
# ==========================================
destinos <- st_read("C:/Users/pcusu/Desktop/Accesibilidad_CABA/Datos/Datos_indice/esp_verdes.geojson")
manzanas <- st_read("C:/Users/pcusu/Desktop/Accesibilidad_CABA/Datos/Datos_indice/manzanas_geom.geojson")

# *** CÁLCULO DE PUNTAJE DE DESTINOS ***
destinos_puntaje <- destinos %>%
  mutate(
    # 1. Puntuación basada en la Categoria 
    puntaje_base = case_when(
      Categoria == "A" ~ 0.60, 
      Categoria == "B" ~ 0.40, 
      Categoria == "C" ~ 0.25, 
      Categoria == "D" ~ 0.15, 
      TRUE ~ 0.10
    ),
    # 2. Puntuación por EdC 
    EDC_num = if_else(!is.na(EDC) & EDC == "x", 0.20, 0),
    
    # 3. Puntuación por WIFI público
    WIFI_num = if_else(!is.na(WIFI) & WIFI == "x", 0.20, 0),
    
    # 4. Puntaje Total del Destino
    puntaje_total = puntaje_base + EDC_num + WIFI_num
  ) %>%
  mutate(dest_idx = row_number()) 

# ==========================================
# 3) Estandarizar ID de manzanas
# ==========================================
if ("CFRM" %in% names(manzanas)) {
  manzanas <- manzanas %>% rename(ID_mza = CFRM)
} else if ("ID" %in% names(manzanas)) {
  manzanas <- manzanas %>% rename(ID_mza = ID)
} else {
  stop("No se encuentra clave única de manzanas (ID o CFRM)")
}

# ==========================================
# 4) Definir velocidades y umbrales de distancia (20 minutos)
# ==========================================
# Velocidades de los perfiles (metros por minuto)
velocidad_mpm_pcd <- (2.5 * 1000) / 60 
velocidad_mpm_psd <- (5 * 1000) / 60

# Umbral de tiempo máximo: 20 minutos 
tiempo_max_min <- 20 
distancia_max_pcd <- round(tiempo_max_min * velocidad_mpm_pcd) 
distancia_max_psd <- round(tiempo_max_min * velocidad_mpm_psd) 

# Encontrar los 5 vecinos más cercanos dentro del umbral de 20 minutos
vecinos_PcD <- st_nn(manzanas, destinos_puntaje, k = 5, maxdist = distancia_max_pcd)
vecinos_PsD <- st_nn(manzanas, destinos_puntaje, k = 5, maxdist = distancia_max_psd)

# ==========================================
# 5) Función para acumular puntajes (PROMEDIO de los 5 destinos)
# ==========================================
acumular_puntajes <- function(manzanas_sf, vecinos_list) {
  valid_indices <- lengths(vecinos_list) > 0
  
  puntajes_temp <- tibble(
    ID_mza = rep(manzanas_sf$ID_mza[valid_indices], lengths(vecinos_list)[valid_indices]),
    dest_idx = unlist(vecinos_list[valid_indices])
  ) %>%
    left_join(st_drop_geometry(destinos_puntaje) %>% select(dest_idx, puntaje_total), by = "dest_idx") %>%
    # Usa MEAN (promedio)
    group_by(ID_mza) %>%
    summarise(puntaje_acumulado = mean(puntaje_total, na.rm = TRUE)) %>%
    ungroup()
  
  return(puntajes_temp)
}

# Aplicar la función para cada perfil
puntajes_PcD <- acumular_puntajes(manzanas, vecinos_PcD)
puntajes_PsD <- acumular_puntajes(manzanas, vecinos_PsD)

# ==========================================
# 6) Unir puntajes y redondear
# ==========================================
manzanas_final <- manzanas %>%
  left_join(puntajes_PcD, by = "ID_mza") %>%
  left_join(puntajes_PsD, by = "ID_mza") %>%
  rename(
    puntaje_facilidades_PcD_promedio = puntaje_acumulado.x,
    puntaje_facilidades_PsD_promedio = puntaje_acumulado.y
  ) %>%
  mutate(
    # Reemplazar NA (fuera de rango accesible/20 min) con 0.00
    puntaje_facilidades_PcD_promedio = replace_na(puntaje_facilidades_PcD_promedio, 0.00),
    puntaje_facilidades_PsD_promedio = replace_na(puntaje_facilidades_PsD_promedio, 0.00)
  ) %>%
  # Forzar dos decimales en los puntajes finales (para exportación y tabla)
  mutate(
    puntaje_facilidades_PcD_promedio = round(puntaje_facilidades_PcD_promedio, 2),
    puntaje_facilidades_PsD_promedio = round(puntaje_facilidades_PsD_promedio, 2)
  ) %>%
  select(ID_mza, geometry, puntaje_facilidades_PcD_promedio, puntaje_facilidades_PsD_promedio)

# ==========================================
# 7) Generación de Tabla de Rangos
# ==========================================

# Rangos solicitados: 0.00, < 0.25, < 0.50, < 0.75, 1 (asumimos <= 1.00)
crear_tabla_rangos_promedio <- function(datos_sf, col_puntaje, perfil) {
  
  # Definir las categorías de corte
  # Usamos case_when para definir límites claros, ya que cut puede ser confuso con el 0.00
  tabla_resumen <- datos_sf %>%
    st_drop_geometry() %>%
    mutate(
      Rango = case_when(
        .data[[col_puntaje]] == 0.00 ~ "0.00",
        .data[[col_puntaje]] < 0.25 ~ "< 0.25",
        .data[[col_puntaje]] < 0.50 ~ "< 0.50",
        .data[[col_puntaje]] < 0.75 ~ "< 0.75",
        .data[[col_puntaje]] <= 1.00 ~ "<= 1.00",
        TRUE ~ "Otro" # Para cualquier valor superior o anómalo
      )
    )
  
  # Orden de las etiquetas para la tabla final
  etiquetas_orden <- c(
    "0.00",
    "< 0.25",
    "< 0.50",
    "< 0.75",
    "<= 1.00"
  )
  
  # Contar las manzanas por rango y asegurar el orden
  tabla_final <- tabla_resumen %>%
    count(Rango, name = paste0("N_Manzanas_", perfil)) %>%
    # Usar right_join con todas las etiquetas para asegurar que todas estén presentes (con conteo 0 si no hay)
    right_join(tibble(Rango = etiquetas_orden), by = "Rango") %>%
    mutate(
      Rango = factor(Rango, levels = etiquetas_orden), # Ordenar por factor
      across(starts_with("N_Manzanas"), replace_na, 0)
    ) %>%
    arrange(Rango)
  
  return(tabla_final)
}

# Generar tablas de resumen
tabla_pcd <- crear_tabla_rangos_promedio(manzanas_final, "puntaje_facilidades_PcD_promedio", "PcD")
tabla_psd <- crear_tabla_rangos_promedio(manzanas_final, "puntaje_facilidades_PsD_promedio", "PsD")

# Combinar tablas
tabla_combinada <- tabla_pcd %>%
  left_join(tabla_psd, by = "Rango")

# Imprimir la tabla para revisión
print(tabla_combinada)

# ==========================================
# 8) Exportar resultados
# ==========================================

# 8.1 Exportar capa final GeoJSON
st_write(manzanas_final, "C:/Users/pcusu/Desktop/puntaje_facilidades_EV_final_promedio_bruto.geojson", delete_dsn = TRUE)

# 8.2 Exportar tabla de rangos CSV
write.csv(tabla_combinada, "C:/Users/pcusu/Desktop/resumen_rangos_facilidades_EV_promedio.csv", row.names = FALSE, fileEncoding = "UTF-8")

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
# *** NOTA IMPORTANTE: Se asume que este archivo contiene las ESTACIONES/PARADAS de Transporte. ***
# Usamos la ruta del script anterior de transporte para consistencia.
destinos <- st_read("C:/Users/pcusu/Desktop/Accesibilidad_CABA/Datos/Datos_indice/acceso_transporte.geojson")
manzanas <- st_read("C:/Users/pcusu/Desktop/Accesibilidad_CABA/Datos/Datos_indice/manzanas_geom.geojson")

# *** CÁLCULO DE PUNTAJE DE DESTINOS (Lógica de Facilidades de TRANSPORTE) ***
destinos_puntaje <- destinos %>%
  mutate(
    # Implementación de la lógica de puntaje basada en el TIPO de equipamiento de TRANSPORTE.
    # Se asume que la columna de clasificación se llama 'tipo' o similar, usando 'tipo'
    # como columna placeholder para hacer la clasificación por las cadenas provistas.
    puntaje_base = case_when(
      str_detect(tipo, regex("Paradas_Metrobus", ignore_case = TRUE)) ~ 0.75,
      str_detect(tipo, regex("refugios_colectivos", ignore_case = TRUE)) ~ 0.60,
      str_detect(tipo, regex("estaciones-de-subte", ignore_case = TRUE)) ~ 0.45,
      str_detect(tipo, regex("estacion_FFCC", ignore_case = TRUE)) ~ 0.30,
      str_detect(tipo, regex("Paradas_Colectivos", ignore_case = TRUE)) ~ 0.20,
      str_detect(tipo, regex("paradas_taxis", ignore_case = TRUE)) ~ 0.05,
      TRUE ~ 0.00 # Otros no puntúan
    ),
    dest_idx = row_number() # ID único para st_nn
  )


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
# 4) Definir velocidades y umbrales de distancia (Umbral máximo: 8 minutos)
# ==========================================
# Velocidades de los perfiles (metros por minuto)
velocidad_mpm_pcd <- (2.5 * 1000) / 60 
velocidad_mpm_psd <- (5 * 1000) / 60

# Umbral de tiempo MÁXIMO (FILTRO BINARIO): 8 minutos 
tiempo_max_min <- 8 
distancia_max_pcd <- round(tiempo_max_min * velocidad_mpm_pcd) 
distancia_max_psd <- round(tiempo_max_min * velocidad_mpm_psd) 

# Encontrar los 5 vecinos más cercanos dentro del umbral de 8 minutos
vecinos_PcD <- st_nn(manzanas, destinos_puntaje, k = 5, maxdist = distancia_max_pcd)
vecinos_PsD <- st_nn(manzanas, destinos_puntaje, k = 5, maxdist = distancia_max_psd)

# ==========================================
# 5) Función para acumular puntajes (PROMEDIO de los 5 destinos)
# ==========================================
# Se mantiene la función que calcula el promedio (MEAN)
acumular_puntajes <- function(manzanas_sf, vecinos_list) {
  valid_indices <- lengths(vecinos_list) > 0
  
  puntajes_temp <- tibble(
    ID_mza = rep(manzanas_sf$ID_mza[valid_indices], lengths(vecinos_list)[valid_indices]),
    dest_idx = unlist(vecinos_list[valid_indices])
  ) %>%
    left_join(st_drop_geometry(destinos_puntaje) %>% select(dest_idx, puntaje_base), by = "dest_idx") %>%
    # Usa MEAN (promedio)
    group_by(ID_mza) %>%
    summarise(puntaje_acumulado = mean(puntaje_base, na.rm = TRUE)) %>%
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
    # Nombres actualizados para TRANSPORTE
    puntaje_fac_transporte_PcD = puntaje_acumulado.x,
    puntaje_fac_transporte_PsD = puntaje_acumulado.y
  ) %>%
  mutate(
    # Reemplazar NA (fuera de rango accesible/8 min) con 0.00
    puntaje_fac_transporte_PcD = replace_na(puntaje_fac_transporte_PcD, 0.00),
    puntaje_fac_transporte_PsD = replace_na(puntaje_fac_transporte_PsD, 0.00)
  ) %>%
  # Forzar dos decimales en los puntajes finales
  mutate(
    puntaje_fac_transporte_PcD = round(puntaje_fac_transporte_PcD, 2),
    puntaje_fac_transporte_PsD = round(puntaje_fac_transporte_PsD, 2)
  ) %>%
  select(ID_mza, geometry, puntaje_fac_transporte_PcD, puntaje_fac_transporte_PsD)

# ==========================================
# 7) Generación de Tabla de Rangos
# ==========================================
# Se mantiene la misma lógica de rangos (0.00, < 0.25, < 0.50, < 0.75, <= 1.00)
crear_tabla_rangos_promedio <- function(datos_sf, col_puntaje, perfil) {
  
  tabla_resumen <- datos_sf %>%
    st_drop_geometry() %>%
    mutate(
      Rango = case_when(
        .data[[col_puntaje]] == 0.00 ~ "0.00",
        .data[[col_puntaje]] < 0.25 ~ "< 0.25",
        .data[[col_puntaje]] < 0.50 ~ "< 0.50",
        .data[[col_puntaje]] < 0.75 ~ "< 0.75",
        .data[[col_puntaje]] <= 1.00 ~ "<= 1.00",
        TRUE ~ "Otro" 
      )
    )
  
  etiquetas_orden <- c(
    "0.00",
    "< 0.25",
    "< 0.50",
    "< 0.75",
    "<= 1.00"
  )
  
  tabla_final <- tabla_resumen %>%
    count(Rango, name = paste0("N_Manzanas_", perfil)) %>%
    right_join(tibble(Rango = etiquetas_orden), by = "Rango") %>%
    mutate(
      Rango = factor(Rango, levels = etiquetas_orden), 
      across(starts_with("N_Manzanas"), replace_na, 0)
    ) %>%
    arrange(Rango)
  
  return(tabla_final)
}

# Generar tablas de resumen
tabla_pcd <- crear_tabla_rangos_promedio(manzanas_final, "puntaje_fac_transporte_PcD", "PcD")
tabla_psd <- crear_tabla_rangos_promedio(manzanas_final, "puntaje_fac_transporte_PsD", "PsD")

# Combinar tablas
tabla_combinada <- tabla_pcd %>%
  left_join(tabla_psd, by = "Rango")

# Imprimir la tabla para revisión
print(tabla_combinada)

# ==========================================
# 8) Exportar resultados
# ==========================================

# 8.1 Exportar capa final GeoJSON
st_write(manzanas_final, "C:/Users/pcusu/Desktop/puntaje_facilidades_TRANSPORTE_8min_promedio.geojson", delete_dsn = TRUE)

# 8.2 Exportar tabla de rangos CSV
write.csv(tabla_combinada, "C:/Users/pcusu/Desktop/resumen_rangos_facilidades_TRANSPORTE_8min_promedio.csv", row.names = FALSE, fileEncoding = "UTF-8")

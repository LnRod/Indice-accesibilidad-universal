# ==========================================
# 1) Librerías
# ==========================================
library(sf)
library(dplyr)
library(nngeo)
library(units)
library(tibble)

# ==========================================
# 2) Calcular puntaje correcto de destinos culturales
# ==========================================
# Cargar destinos
destinos <- st_read("C:/Users/pcusu/Desktop/Accesibilidad_CABA/Datos/Datos_indice/Espacios_culturales.geojson")

# Clasificación: ANFITEATRO, MUSEO => 0.80  
# GALERIA DE ARTE, MONUMENTOS Y LUGARES HISTORICOS => 0.60  
# SALA DE TEATRO, CENTRO CULTURAL => 0.40  
# BAR, BIBLIOTECA, SALA DE CINE => 0.20

destinos_puntaje <- destinos %>%
  mutate(
    puntaje_base = case_when(
      FUNCION_PR %in% c("ANFITEATRO", "MUSEO") ~ 0.80,
      FUNCION_PR %in% c("GALERIA DE ARTE", "MONUMENTOS Y LUGARES HISTORICOS") ~ 0.60,
      FUNCION_PR %in% c("SALA DE TEATRO", "CENTRO CULTURAL") ~ 0.40,
      FUNCION_PR %in% c("BAR", "BIBLIOTECA", "SALA DE CINE") ~ 0.20,
      TRUE ~ 0  # Otros no puntúan
    ),
    dest_idx = row_number()  # ID único para st_nn
  )

# ==========================================
# 3) Revisar ID manzanas y clave segura
# ==========================================
manzanas <- st_read("puntaje_distancia_espacios_culturales.geojson")

if ("ID" %in% names(manzanas)) {
  manzanas <- manzanas %>% mutate(ID_mza = ID)
} else if ("CFRM" %in% names(manzanas)) {
  manzanas <- manzanas %>% mutate(ID_mza = CFRM)
} else {
  stop("No se encuentra clave única de manzanas (ID o CFRM)")
}

# ==========================================
# 4) Filtrar manzanas con acceso válido (Puntaje 0/1)
# ==========================================
manzanas_PcD <- manzanas %>% filter(acceso_PcD == 1)
manzanas_PsD <- manzanas %>% filter(acceso_PsD == 1)

# ==========================================
# 5) Vecinos dentro de radio (según velocidad)
# ==========================================
# 2.5 km/h ≈ 625 m en 9 min | 5 km/h ≈ 1250 m en 9 min

vecinos_PcD <- st_nn(manzanas_PcD, destinos_puntaje, k = 5, maxdist = 625)
vecinos_PsD <- st_nn(manzanas_PsD, destinos_puntaje, k = 5, maxdist = 1250)

# ==========================================
# 6) Relacionar y acumular puntajes
# ==========================================
# PcD
D_PcD <- tibble(
  ID_mza = rep(manzanas_PcD$ID_mza, lengths(vecinos_PcD)),
  dest_idx = unlist(vecinos_PcD)
) %>%
  left_join(destinos_puntaje %>% select(dest_idx, puntaje_base), by = "dest_idx") %>%
  group_by(ID_mza) %>%
  summarise(puntaje_PcD = sum(puntaje_base, na.rm = TRUE))

# PsD
D_PsD <- tibble(
  ID_mza = rep(manzanas_PsD$ID_mza, lengths(vecinos_PsD)),
  dest_idx = unlist(vecinos_PsD)
) %>%
  left_join(destinos_puntaje %>% select(dest_idx, puntaje_base), by = "dest_idx") %>%
  group_by(ID_mza) %>%
  summarise(puntaje_PsD = sum(puntaje_base, na.rm = TRUE))

# ==========================================
# 7) Unir a manzanas
# ==========================================
manzanas_final <- manzanas %>%
  left_join(D_PcD, by = "ID_mza") %>%
  left_join(D_PsD, by = "ID_mza") %>%
  mutate(
    puntaje_PcD = replace_na(puntaje_PcD, 0),
    puntaje_PsD = replace_na(puntaje_PsD, 0)
  )

# ==========================================
# 8) Exportar capa final
# ==========================================
st_write(manzanas_final, "manzanas_final_puntaje_espacios_culturales.geojson", delete_dsn = TRUE)

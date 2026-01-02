# ==========================================
# 1) Librerías
# ==========================================
library(sf)
library(dplyr)
library(nngeo)
library(units)
library(tibble)

# ==========================================
# 2) Calcular puntaje correcto de destinos
# ==========================================
destinos_puntaje <- destinos %>%
  mutate(
    puntaje_base = case_when(
      Categoria == "A" ~ 0.60,
      Categoria == "B" ~ 0.40,
      Categoria == "C" ~ 0.25,
      Categoria == "D" ~ 0.15,
      TRUE ~ 0.10
    ),
    EDC_num = if_else(is.na(EDC), 0, if_else(EDC == "x", 0.20, 0)),
    WIFI_num = if_else(is.na(WIFI), 0, if_else(WIFI == "x", 0.20, 0)),
    puntaje_extra = EDC_num + WIFI_num,
    puntaje_total = puntaje_base + puntaje_extra
  ) %>%
  mutate(dest_idx = row_number())  # ID único para st_nn

# ==========================================
# 3) Revisar ID manzanas y usar clave segura
# ==========================================
# Revisar columna clave: ID o CFRM
if ("ID" %in% names(manzanas)) {
  manzanas <- manzanas %>% mutate(ID_mza = ID)
} else if ("CFRM" %in% names(manzanas)) {
  manzanas <- manzanas %>% mutate(ID_mza = CFRM)
} else {
  stop("No se encuentra clave única de manzanas (ID o CFRM)")
}

# ==========================================
# 4) Filtrar manzanas con acceso
# ==========================================
manzanas_PcD <- manzanas %>% filter(acceso_PcD == 1)
manzanas_PsD <- manzanas %>% filter(acceso_PsD == 1)

# ==========================================
# 5) Vecinos dentro de umbral
# ==========================================
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
  left_join(destinos_puntaje %>% select(dest_idx, puntaje_total), by = "dest_idx") %>%
  group_by(ID_mza) %>%
  summarise(puntaje_PcD = sum(puntaje_total, na.rm = TRUE))

# PsD
D_PsD <- tibble(
  ID_mza = rep(manzanas_PsD$ID_mza, lengths(vecinos_PsD)),
  dest_idx = unlist(vecinos_PsD)
) %>%
  left_join(destinos_puntaje %>% select(dest_idx, puntaje_total), by = "dest_idx") %>%
  group_by(ID_mza) %>%
  summarise(puntaje_PsD = sum(puntaje_total, na.rm = TRUE))

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
st_write(manzanas_final, "manzanas_final_puntaje_acumulado.geojson", delete_dsn = TRUE)

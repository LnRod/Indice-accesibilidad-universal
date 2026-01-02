library(sf)
library(dplyr)

puntajeacc<-st_read("C:/Users/pcusu/Desktop/puntaje_acceso_categorias_MZ.geojson")

puntajeacc_2 <- puntajeacc %>% 
  mutate(
    puntaje_transitabilidad = round(
      (Puntaje_Ancho_Vereda + Puntaje_Vados + Puntaje_Cruces + Puntaje_Semaforos) / 2.00,
      2 # Rounds the result to 2 decimal places
    ),
    puntaje_confort = round(
      (Puntaje_Ruido + Puntaje_AscensoDescenso + Puntaje_Totems + Puntaje_ReservasEst) / 2.00,
      2 # Rounds the result to 2 decimal places
    ),
    puntaje_usos_su = round(
      (Puntaje_Comercial + Puntaje_Equipamientos),
      2 # Rounds the result to 2 decimal places
    ))

table(puntajeacc_2$puntaje_usos_su)

puntajeacc_3 <- puntajeacc_2 %>% 
  mutate(
    # Calcula el promedio sumando los 3 puntajes normalizados y dividiendo por 3.
    # El resultado estará automáticamente en la escala 0-1.
    puntaje_acceso = (puntaje_transitabilidad + puntaje_usos_su + puntaje_confort) / 3.00,
    
    # Opcional: Redondear el promedio final a 2 decimales para consistencia
    puntaje_acceso = round(puntaje_acceso, 2)
  )


puntaje_acceso_x_equipamientos <- puntajeacc_2 %>% select(c(-'Puntaje_Usos_Su', -'Puntaje_Confort', -'Puntaje_Acceso', -'Puntaje_Infra'))

colnames(puntajeacc_3)

st_write(puntaje_acceso_totales, "puntaje_acceso_total.geojson")


puntaje_acceso_totales <- puntajeacc_3 %>% select(c('CFRM', 'puntaje_transitabilidad', 'puntaje_confort', 'puntaje_usos_su', 'puntaje_acceso'))


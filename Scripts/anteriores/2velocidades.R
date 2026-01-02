# Velocidad en km/h
vel_normal <- 5 
vel_discap <- 2.5

# Tiempos umbral (minutos)
tiempo <- 10

# Distancias: 
# 1-velocidad (km/h) en el umbral: 5 minutos
vel_10mins_normal <- 5 * 10 / 60 #velocidad * tiempo * km a la hora
vel_10mins_disc <- 2.5 * 10 / 60 #velocidad * tiempo * km a la hora

#velocidad por minuto (pasaje de km/h a metros por minuto)
vel_1min_normal <- (1 * 0.83 / 10) * 100/2 #tiempo * velocidad en 5 mins / 10 mins * pasaje a m/s
vel_1min_disc <- (1 * 0.41 / 10) * 100/2 #tiempo * velocidad en 5 mins / 10 mins * pasaje a m/s
#normal = 82 metros/minuto
#discapacidad = 40 metros/minuto

# Distancias equivalentes (m) en 10 minutos
distancia_normal <- 82 * 10
distancia_discap <- 40 * 10

#la distancia maxima de una persona sin discapacidades que camina 10 minutos a (5 km/h) es 820 metros
#la distancia maxima de una persona con discapacidades que camina 10 minutos a (2,5 km/h) es 410 metros


#la distancia maxima de una persona sin discapacidades que camina 5 minutos a (5 km/h) es 410 metros
#la distancia maxima de una persona con discapacidades que camina 5 minutos a (2,5 km/h) es 200 metros


library(data.table)

#### Definiendo arbol de directorios ####
dir_root <- "/home/yo/dataton_2018/"
dir_info_pagadores <- paste0(dir_root, "dt_info_pagadores_muestra/")

#### Columnas de la informacion de los usuarios ####
info_pagadores_columns <- c("id_cliente", "seg_str", "ocupacion",
                            "tipo_vivienda", "nivel_academico", 
                            "estado_civil", "genero", "edad", 
                            "ingreso_rango")

#### Leyendo la data ####
dt_info_pagadores <- fread(paste0(dir_info_pagadores, "dt_info_pagadores_muestra.csv"),
                           sep = ",", encoding = "UTF-8", header = F)

#### Definiendo los nombres de las columnas ####
colnames(dt_info_pagadores) <- info_pagadores_columns

####
#### Pasando de encodings a descripciones de las variables ####
####

#### ocupacion ####
dt_ocupacion <- data.table(ocupacion = c("E","I","O","P","S","1","2",
                                         "3","4","5","6","7","8","9"),
                           ocupacion_str= c("SOCIO O EMPLEADO - SOCIO",
                                            "DESEMPLEADO CON INGRESOS",
                                            "OTRA","INDEPENDIENTE",
                                            "DESEMPLEADO SIN INGRESOS",
                                            "EMPLEADO","ESTUDIANTE",
                                            "INDEPENDIENTE","HOGAR",
                                            "JUBILADO","AGRICULTOR",
                                            "GANADERO","COMERCIANTE",
                                            "RENTISTA DE CAPITAL"))
dt_info_pagadores <- merge(dt_info_pagadores, dt_ocupacion, by = "ocupacion")
dt_info_pagadores[, ocupacion := NULL]
setnames(dt_info_pagadores, "ocupacion_str", "ocupacion")
rm(dt_ocupacion)

#### tipo_vivienda ####
dt_tipo_vivienda <- data.table(tipo_vivienda = c("A","R","F","I","P","O"),
                               tipo_vivienda_str = c("ALQUILADA","ALQUILADA",
                                                     "FAMILIAR","NO INFORMA",
                                                     "PROPIA","PROPIA"))
dt_info_pagadores <- merge(dt_info_pagadores, dt_tipo_vivienda, by = "tipo_vivienda")
dt_info_pagadores[, tipo_vivienda := NULL]
setnames(dt_info_pagadores, "tipo_vivienda_str", "tipo_vivienda")
rm(dt_tipo_vivienda)

#### nivel_academico ####
dt_nivel_academico <- data.table(nivel_academico = c("H","B","U","E","N","P","S","T","I"),
                                 nivel_academico_str = c("BACHILLERATO","BACHILLERATO",
                                                         "UNIVERSITARIO","ESPECIALIZACION",
                                                         "NINGUNO","PRIMARIA",
                                                         "POSTGRADO","TECNICO",
                                                         "NO INFORMA"))
dt_info_pagadores <- merge(dt_info_pagadores, dt_nivel_academico, by = "nivel_academico")
dt_info_pagadores[, nivel_academico := NULL]
setnames(dt_info_pagadores, "nivel_academico_str", "nivel_academico")
rm(dt_nivel_academico)

#### estado_civil ####
dt_estado_civil <- data.table(estado_civil = c("S","M","F","I","D","W","O"),
                              estado_civil_str = c("SOLTERO","CASADO",
                                                   "DESCONOCIDO","NO INFORMA",
                                                   "DIVORCIADO","VIUDO","OTRO"))
dt_info_pagadores <- merge(dt_info_pagadores, dt_estado_civil, by = "estado_civil")
dt_info_pagadores[, estado_civil := NULL]
setnames(dt_info_pagadores, "estado_civil_str", "estado_civil")
rm(dt_estado_civil)

#### Reorganizando las columnas ####
dt_info_pagadores <- dt_info_pagadores[, c("id_cliente", "seg_str", "ocupacion",
                                           "tipo_vivienda", "nivel_academico", "estado_civil",
                                           "genero", "edad", "ingreso_rango")]

#### Guardando el resultado en disco ####
fwrite(dt_info_pagadores, 
       paste0(dir_info_pagadores, "dt_info_pagadores_descriptivo.csv"), 
       sep = ",")

# dt_info_pagadores[, table(ingreso_rango)]
dt_info_pagadores[ingreso_rango == "0", ingreso := 0]
dt_info_pagadores[ingreso_rango == "a. (0  1.1MM]", ingreso := 550000]
dt_info_pagadores[ingreso_rango == "b. (1.1  2.2MM]", ingreso := 1650000]
dt_info_pagadores[ingreso_rango == "c. (2.2  3.3MM]", ingreso := 2750000]
dt_info_pagadores[ingreso_rango == "d. (3.3  4.4MM]", ingreso := 3850000]
dt_info_pagadores[ingreso_rango == "e. (4.4  5.5MM]", ingreso := 4950000]
dt_info_pagadores[ingreso_rango == "f. (5.5  6.6MM]", ingreso := 6050000]
dt_info_pagadores[ingreso_rango == "g. (6.6  7.6MM]", ingreso := 7100000]
dt_info_pagadores[ingreso_rango == "h. (7.6  8.7MM]", ingreso := 8150000]
dt_info_pagadores[ingreso_rango == "i. (8.7  Inf)",   ingreso := 11000000]
dt_info_pagadores[is.na(ingreso), ingreso := 0]

#### Guardando el resultado en disco ####
fwrite(dt_info_pagadores[, .(id_cliente, ingreso)], 
       paste0(dir_info_pagadores, "dt_info_pagadores_ingresos.csv"), 
       sep = ",")

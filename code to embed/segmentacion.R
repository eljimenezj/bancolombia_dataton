library(data.table)
library(ggplot2)

#### Definiendo arbol de directorios ####
dir_root <- "/home/yo/dataton_2018/"
dir_trxpse <- paste0(dir_root, "dt_trxpse_personas_2016_2018_muestra_adjt/")
dir_info_pagadores <- paste0(dir_root, "dt_info_pagadores_muestra/")

#### Columnas de la informacion de los usuarios ####
trxpse_columns <- c("id_trans", "id_cliente", "fecha", "hora", 
                    "valor_trx", "ref1", "ref2", "ref3",
                    "sector", "subsector", "descripcion")

#### Leyendo la data ####
dt_trxpse <- fread(paste0(dir_trxpse, "dt_trxpse.csv"), 
                   sep = ",", encoding = "UTF-8")

#### Definiendo los nombres de las columnas ####
colnames(dt_trxpse) <- trxpse_columns

dt_trxpse[, id_trans := as.integer(id_trans)]
dt_trxpse[, id_cliente := as.integer(id_cliente)]
dt_trxpse[, fecha := as.integer(fecha)]
dt_trxpse[, hora := as.integer(hora)]

dt_trxpse[, ano := substr(as.character(fecha), 1, 4)]
dt_trxpse[, mes := substr(as.character(fecha), 5, 6)]
dt_trxpse[, dia := substr(as.character(fecha), 7, 8)]
dt_trxpse[, fecha := as.Date(as.character(fecha), "%Y%m%d")]

perc_99 <- dt_trxpse[, quantile(valor_trx, probs = 0.995, names = F)]
dt_trxpse <- dt_trxpse[valor_trx < perc_99]
dt_trxpse <- dt_trxpse[valor_trx > 1000]

#### Cargando información de clientes ####
dt_info_pagadores <- fread(paste0(dir_info_pagadores, "dt_info_pagadores_descriptivo.csv"),
                           sep = ",", colClasses = "character")
dt_info_pagadores[, id_cliente := as.integer(id_cliente)]

#### parametros
probs_puntaje <- c(0.4, 0.7, 0.9)
fecha_limite <- as.Date("20180701", "%Y%m%d")
# Agregando la columna recencia
dt_RFM <- dt_trxpse[fecha >= fecha_limite, 
                    .(recency = min(as.numeric(difftime(Sys.Date(), fecha, units="days"))),
                      freq = .N, monetary = sum(valor_trx)), by = "id_cliente"]
# Puntaje recencia
recency_perc <- dt_RFM[, quantile(-recency, probs = probs_puntaje, names = F),]
dt_RFM[, recency_point := 4]
dt_RFM[-recency <= recency_perc[3], recency_point := 3]
dt_RFM[-recency <= recency_perc[2], recency_point := 2]
dt_RFM[-recency <= recency_perc[1], recency_point := 1]
# Puntaje frecuancia
freq_perc <- dt_RFM[, quantile(freq, probs = probs_puntaje, names = F),]
dt_RFM[, freq_point := 4]
dt_RFM[freq <= freq_perc[3], freq_point := 3]
dt_RFM[freq <= freq_perc[2], freq_point := 2]
dt_RFM[freq <= freq_perc[1], freq_point := 1]
# Puntaje monto
monetary_perc <- dt_RFM[, quantile(monetary, probs = probs_puntaje, names = F),]
dt_RFM[, monetary_point := 4]
dt_RFM[monetary <= monetary_perc[3], monetary_point := 3]
dt_RFM[monetary <= monetary_perc[2], monetary_point := 2]
dt_RFM[monetary <= monetary_perc[1], monetary_point := 1]
# Totalizando puntaje
dt_RFM[, RFM_point := recency_point + freq_point + monetary_point]
# Haciendo join con la informacion de los pagadores
dt_info_pagadores <- merge(dt_info_pagadores, dt_RFM, 
                           by = "id_cliente", all.x = T)
dt_info_pagadores[is.na(recency_point), recency_point := 0]
dt_info_pagadores[is.na(freq_point), freq_point := 0]
dt_info_pagadores[is.na(monetary_point), monetary_point := 0]
dt_info_pagadores[is.na(RFM_point), RFM_point := 0]


# dt_consumo_historico <- dt_trxpse[, .(consumo_hist = ifelse(.N == 0, 0, sum(valor_trx)/.N)), 
#                                   by = "id_cliente"]
dt_consumo_historico <- dt_trxpse[, .(consumo_hist = median(valor_trx)), by = "id_cliente"]
consumo_hist_perc <- dt_consumo_historico[, quantile(consumo_hist, probs = probs_puntaje, names = F),]
dt_consumo_historico[, consumo_hist_point := 4]
dt_consumo_historico[consumo_hist <= consumo_hist_perc[3], consumo_hist_point := 3]
dt_consumo_historico[consumo_hist <= consumo_hist_perc[2], consumo_hist_point := 2]
dt_consumo_historico[consumo_hist <= consumo_hist_perc[1], consumo_hist_point := 1]

dt_info_pagadores <- merge(dt_info_pagadores, dt_consumo_historico, 
                           by = "id_cliente", all.x = T)
dt_info_pagadores[is.na(consumo_hist_point), consumo_hist_point := 0]

dt_info_pagadores[, total_point := RFM_point + consumo_hist_point]

point_perc <- dt_info_pagadores[, quantile(total_point, probs = probs_puntaje, names = F),]
dt_info_pagadores[, segmento := 'Experto Digital']
dt_info_pagadores[total_point <= point_perc[3], segmento := 'Asociado Digital']
dt_info_pagadores[total_point <= point_perc[2], segmento := 'Aprendiz Digital']
dt_info_pagadores[total_point <= point_perc[1], segmento := 'Newbie Digital']

#### Guardando el resultado en disco ####
fwrite(dt_info_pagadores[, .(id_cliente, 
                             recency, freq, monetary, consumo_hist,
                             recency_point, freq_point, monetary_point, RFM_point,
                             consumo_hist_point, total_point, segmento)], 
       paste0(dir_info_pagadores, "dt_info_pagadores_segmento.csv"), sep = ",")

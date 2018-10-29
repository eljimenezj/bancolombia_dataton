library(data.table)

#### Definiendo arbol de directorios ####
dir_root <- "/home/yo/dataton_2018/"
dir_info_pagadores <- paste0(dir_root, "dt_info_pagadores_muestra/")

lend_predict <- function(id){

model <- readRDS(paste0(dir_root, "tree_model.rds"))

dt_info_pagadores <- fread(paste0(dir_info_pagadores, "dt_info_pagadores_descriptivo.csv"),
                           sep = ",", colClasses = "character")

dt_info_pagadores[, id_cliente := as.integer(id_cliente)]
suppressWarnings(dt_info_pagadores[, edad := as.integer(edad)])
edad_mean <- dt_info_pagadores[, round(mean(edad, na.rm = T))]
dt_info_pagadores[is.na(edad), edad := edad_mean]
dt_info_pagadores[, seg_str := as.factor(seg_str)]
dt_info_pagadores[, ocupacion := as.factor(ocupacion)]
dt_info_pagadores[, tipo_vivienda := as.factor(tipo_vivienda)]
dt_info_pagadores[, nivel_academico := as.factor(nivel_academico)]
dt_info_pagadores[, estado_civil := as.factor(estado_civil)]
dt_info_pagadores[, ingreso_rango := as.factor(ingreso_rango)]

cliente <- dt_info_pagadores[id_cliente == id]
if(nrow(cliente) == 0){
  0
}else{
  predict.val <- predict(model, cliente)
  ifelse((predict.val[, 2] < 0.7), 0, 1)
}

# predict.val <- predict(model, dt_info_pagadores)
# median(predict.val[,2])
# prop.table(table(ifelse(predict.val[,2] >=0.8, 1, 0)))
}
lend_predict(id = 10)


library(data.table)
library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ROSE)

#### Definiendo arbol de directorios ####
dir_root <- "/home/yo/dataton_2018/"
dir_info_pagadores <- paste0(dir_root, "dt_info_pagadores_muestra/")

sample_pagadores <- fread(paste0(dir_info_pagadores, "sample_pagadores.csv"), sep = ",")

# sample_pagadores <- sample_pagadores[, .(seg_str, ocupacion, tipo_vivienda,
#                                          nivel_academico, estado_civil,
#                                          edad, ingreso_rango, RFM_point,
#                                          consumo_hist, consumo_hist_point, 
#                                          total_point, target)]
sample_pagadores <- sample_pagadores[, .(seg_str, ocupacion, tipo_vivienda,
                                         nivel_academico, estado_civil,
                                         edad, ingreso_rango, target)]
sample_pagadores[, edad := as.integer(edad)]
edad_mean <- sample_pagadores[, round(mean(edad, na.rm = T))]
sample_pagadores[is.na(edad), edad := edad_mean]
sample_pagadores[, seg_str := as.factor(seg_str)]
sample_pagadores[, ocupacion := as.factor(ocupacion)]
sample_pagadores[, tipo_vivienda := as.factor(tipo_vivienda)]
sample_pagadores[, nivel_academico := as.factor(nivel_academico)]
sample_pagadores[, estado_civil := as.factor(estado_civil)]
sample_pagadores[, ingreso_rango := as.factor(ingreso_rango)]

#### Partiendo la data en entrenamiento y test
set.seed(9)
index_train <- sample(1:nrow(sample_pagadores), 
                      size = nrow(sample_pagadores) * 0.7, 
                      replace = F)
sample_train <- sample_pagadores[index_train]
sample_test  <- sample_pagadores[-index_train,]

# grow tree 
tree <- rpart(target ~ seg_str + ocupacion + tipo_vivienda + nivel_academico + estado_civil, 
              method = "class", 
              data = sample_train#, minsplit=2, minbucket=1
              )

fancyRpartPlot(tree)

# plot tree 
plot(tree, uniform=TRUE, 
     main="Prestamo de clientes")
text(tree, use.n=TRUE, all=TRUE, cex=.8)

predict_test <- predict(tree, newdata = sample_test[, -c("target")])

accuracy.meas(sample_test$target, predict_test[,2])
roc.curve(sample_test$target, predict_test[,2])

saveRDS(tree, paste0(dir_root, "tree_model.rds"))

### Monto maximo a prestar

path3 <- "D:\\Users\\EdgarJJ\\Desktop\\Plumbers\\chunks\\dt_info_pagadores_descriptivo.csv"
df3 <-  fread(path3,sep=",", header = T, stringsAsFactors = F,
                    encoding="UTF-8")

saving_capacity <- function(id,p_ahorro){
  df_id     <- data.frame(valor_trx = df[id_cliente==id,]$valor_trx)
  dfc       <- sma(df_id$valor_trx , h=3, silent=FALSE)
  ingresos  <- data.frame(ingreso = df2[id_cliente==id,]$ingreso)
  egreso    <- data.frame(valor_trx=as.numeric(dfc$forecast + (sd(df_id$valor_trx)*runif(1,0,0.1))*
                                                 round(runif(1,-1,1),digits = 0)))
  egreso    <- mean(egreso$valor_trx)
  capacidad <- data.frame((ingresos*p_ahorro)-egreso)
  colnames(capacidad) <- c("Monto_maximo")
  return(capacidad) 
}

saving_capacity(120,0.3)


prestamo_a_otorgar <- function(id, p_ahorro){
  if(df3[id_cliente==id,]$seg_str == "OTRO"){
    tasa <- 20
    factor_mult <- 10.886508
  }else if(df3[id_cliente==id,]$seg_str == "PERSONAL"){
    tasa <- 18
    factor_mult <- 10.983414
  }else if(df3[id_cliente==id,]$seg_str == "PERSONAL PLUS"){
    tasa <- 16
    factor_mult <- 11.083126
  }else if(df3[id_cliente==id,]$seg_str == "EMPRENDEDOR"){
    tasa <- 14
    factor_mult <- 11.185780
  }else{ #(seg_str == "PREFERENCIAL")
    tasa <- 12
    factor_mult <- 11.291516
  }
  credito <- saving_capacity(id,p_ahorro)$Monto_maximo * factor_mult
  credito <- ifelse(credito > 20000000, 20000000, credito)
  
  data.frame(Tasa = paste0(tasa,"%"," ","E.A"), Credito = credito)
  
}

prestamo_a_otorgar(115871,0.5)


sc <- function(id,y){
df_id     <- data.frame(valor_trx = df[id_cliente==id,]$valor_trx)
dfc       <- sma(df_id$valor_trx , h=3, silent=FALSE)
ingresos  <- data.frame(ingreso = rep(df2[id_cliente==id,]$ingreso,3))
egreso    <- data.frame(valor_trx=as.numeric(dfc$forecast + (sd(df_id$valor_trx)*runif(1,0,0.1))*
                                               round(runif(1,-1,1),digits = 0)))

p_ahorro         <- y
ahorro           <- data.frame(ahorro=(ingresos*p_ahorro)-egreso)
colnames(ahorro) <- c("Ahorro")
return(ahorro)
}

sc(115871,0.2)
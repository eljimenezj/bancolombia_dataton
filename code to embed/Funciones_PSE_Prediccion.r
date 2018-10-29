## Lectura base de datos PSE

# Librerias necesarias 
library(scales)
library(plotly)
library(ggplot2)
library(plotrix)
library(data.table)

path <- "D:\\Users\\EdgarJJ\\Desktop\\Plumbers\\chunks\\consolidado.csv"
path2 <- "D:\\Users\\EdgarJJ\\Desktop\\Plumbers\\chunks\\dt_info_pagadores_ingresos.csv"

df <- fread(path,sep=",", header = T, stringsAsFactors = F,
                        encoding="UTF-8")

colnames(df) <- c("id_trans","id_cliente","fecha","hora",
                  "valor_trx","ref1","ref2","ref3",
                  "sector","subsector","descripcion")

### Lectura ingresos estimados

df2 <- fread(path2,sep=",", header = T, stringsAsFactors = F,
            encoding="UTF-8")



## Transformacioin columna fechas
#df$fecha <- as.Date(as.character(df$fecha), "%Y%m%d")


## Funcion que devuelve el promedio del gasto pronosticado 
## tres periodos adelente. Solo se le ingresa el id del cliente 

# install.packages("smooth")
require(smooth)

fc <- function(id){
  df_id <- data.frame(valor_trx = df[id_cliente==id,]$valor_trx)
  dfc   <- sma(df_id$valor_trx , h=3, silent=FALSE)
  f     <- data.frame(valor_trx=as.numeric(dfc$forecast + (sd(df_id$valor_trx)*runif(1,0,0.1))*
                                          round(runif(1,-1,1),digits = 0)))
  #return(rbind(df_id,f))
  return(mean(f$valor_trx))
  }

fc(10)

# length(unique(df$id_cliente))-length(df2$id_cliente)
### Funcion que devuelve pronosticos y graficos



pfc <- function(id){
  df_id     <- data.frame(valor_trx = df[id_cliente==id,]$valor_trx)
  dfc       <- sma(df_id$valor_trx , h=3, silent=FALSE)
  egreso    <- data.frame(valor_trx=as.numeric(dfc$forecast + (sd(df_id$valor_trx)*runif(1,0,0.1))*
                                             round(runif(1,-1,1),digits = 0)))
  ingresos  <- data.frame(ingreso = df2[id_cliente==id,]$ingreso)
  periodo   <- c(1:length(egreso$valor_trx))
  df_plot   <- data.frame(periodo,ingresos,egreso)
  
  # Grafica
  p10 <- ggplot(df_plot, aes(x= periodo)) + 
    geom_line(aes(y = ingreso, colour = "Ingresos")) + 
    geom_line(aes(y = valor_trx, colour = "Gastos Pron. PSE ")) 
  
  p10 <- p10 + ggtitle(paste("Ingresos estimados vs Pronóstico Gastos PSE \n Usuario ID: ",id))
  p10 <- p10 + 
    scale_y_continuous(labels = scales::comma, name = "Cifras en pesos COP")
  
  ggplotly(p10)
  }
  
  pfc(115871)

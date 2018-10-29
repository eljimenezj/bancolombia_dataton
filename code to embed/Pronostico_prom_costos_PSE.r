## Lectura base de datos PSE

# Librerias necesarias 
library(scales)
library(plotly)
library(ggplot2)
library(plotrix)
library(data.table)

path <- "D:\\Users\\EdgarJJ\\Desktop\\Plumbers\\chunks\\consolidado.csv"

df <- fread(path,sep=",", header = T, stringsAsFactors = F,
                        encoding="UTF-8")

colnames(df) <- c("id_trans","id_cliente","fecha","hora",
                  "valor_trx","ref1","ref2","ref3",
                  "sector","subsector","descripcion")



## Funcion que devuelve el promedio del gasto pronosticado 
## tres periodos adelente. Solo se le ingresa el id del cliente 

### Lectura ingresos estimados


## Transformacioin columna fechas
df$fecha <-  as.Date(as.character(df$fecha), "%Y%m%d")
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

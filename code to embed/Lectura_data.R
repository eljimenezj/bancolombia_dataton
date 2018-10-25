## Lectura base de datos PSE

# Librerias necesarias 
library(easyGgplot2)
library(scales)
library(plotly)
library(ggplot2)
library(plotrix)
library(ezplot)


path <- "D:\\Users\\EdgarJJ\\Desktop\\Plumbers\\chunks\\consolidado.csv"

df <- fread(path,sep=",", header = T, stringsAsFactors = F,
                        encoding="UTF-8")

colnames(df) <- c("id_trans","id_cliente","fecha","hora",
                  "valor_trx","ref1","ref2","ref3",
                  "sector","subsector","descripcion")

## Transformacioin columna fechas
df$fecha <- as.Date(as.character(df$fecha), "%Y%m%d")

## Pendiente transformación horas - FORRO


## Funciones para Shinny

## Funcion de busqueda de id

id_selection <- function(id){
  df_id <- df[id_cliente==id,]
  
  label <- factor(quarter(df_id$fecha))  ## Este month debe poder cambiar por quarter or year para el shinny
  p10   <- ggplot(df_id, aes(x = label, 
                           y = valor_trx)) +  geom_boxplot(fill = "#220095", colour = "black",
                                                           alpha = 0.7)
  p10 <- p10 + ggtitle(paste("Transacciones del usuario con id",id))
  p10 <- p10 + scale_x_discrete(name = "Tiempo") +
    scale_y_continuous(labels = scales::comma, name = "Cifras en pesos COP")
  
  ggplotly(p10)
}


## Ejemplo de ejecucion con id cliente
id_selection(12) # probar diferentes id
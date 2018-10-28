library(data.table)
library(dplyr)
options( stringsAsFactors = F)

path <- "D:\\Users\\EdgarJJ\\Desktop\\Plumbers\\df.csv"       # Archivo orginal de BC 
df   <- fread(path,sep="¬", header = F, stringsAsFactors = F) # Lectura como un read.lines


ch <- round(nrow(df)/10) # Tamaño del chunk (Aprox. 1 mll de registros)

# Generación de Chunks del data frame orginal
for (i in 1:11){
  assign(paste0("chunk",i), data[((ch*(i-1))+1):(i*ch),]) # Obtenemos 11 chunks
}

# Depuracion
lines_split <- strsplit(chunk1$V1, split=",", fixed=TRUE)       # Dividir por comas el dataset (chunk1...chunk2 -> Manual) 
ind 		<- 1:20 	                                        # Añadir 20 columnas adicionales al final 
df2 		<- as.data.table(do.call("rbind",
								lapply(lines_split, "[", ind))) # convertir en un data.table el lines_split (que solo es una cadena de caracteres)

# Retirando las 10 primeras columnas
df10 <- df2[,c(1:10)]  # Las 10 primeras columnas estan correctamente separadas
df11 <- df2[,-c(1:10)] # Parte que se va a arreglar posterior

# Concatenar
df12 <- mutate(df11, x = paste(V11,V12,V13,V14,V15,V16,V17,V18,V19,V20)) # Crea una columna nueva con todo concatenado
df12 <- as.data.table(x=df12$x)                                          # Extraer solo la columna unica que nos interesa 

## Limpiar
df13 <- as.data.table(gsub("  "," ",df12$V1))   # Retirar espacios
df13 <- as.data.table(gsub("NA","",df13$V1))    # Retirar los NA que se generaron anteriormente 
df13 <- as.data.table(gsub(",","",df13$V1))     # Retirar comas de este campo descripción 
df13 <- as.data.table(gsub(":","",df13$V1))     # Retirar string :
df13 <- as.data.table(gsub("@","",df13$V1))     # Retirar string @
colnames(df13) <- "V11"

## Pegar al dataset original
dfc <- as.data.table(cbind(df10,df13))          #Unificar las 10 primeras columnas con la columna 11 que estabamos limpiando

##Escribir
setwd("D:\\Users\\EdgarJJ\\Desktop\\Plumbers\\chunks")  # Establecer carpeta donde almacenar los chunks de manera local

## Definición de tipos de variables del dataset chunk
dfc$V1  <- as.numeric(dfc$V1)  
dfc$V2  <- as.numeric(dfc$V2)
dfc$V3  <- as.numeric(dfc$V3)
dfc$V4  <- as.numeric(dfc$V4)
dfc$V5  <- as.numeric(dfc$V5)


write.csv(dfc,"chunk1.csv",row.names=FALSE)     # Escribir como un csv sin index en el wd (chunk1...chunkn.. manual)
rm(df2,df10,df11,df12,df13,dfc,lines_split,chunk11) # Eliminar variables del ambiente para liberar ram

# Nota este proceso no esta automatizado, por lo cual se debe correr tantas veces como chunks tengamos
# para este caso 11 veces, hay que cambiar la variable chunk1 por chunk2 y asi sucesivamente cada que corramos
# las lineas en las que hay que hacer los cambios son, line 17, line 52 y 53, y se tiene que correr manual y repetitivamente
# desde la linea 16 (#Depuracion)


## Funcion para consolidar todos los chunks

path <- "D:\\Users\\EdgarJJ\\Desktop\\Plumbers\\chunks" # Direccion donde estan todos los chunks almacenados
# nota: en esta ruta guarde unicamente los chunks, nada mas porque es automatico la consolidacion

# Funcion
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=T)
  datalist = lapply(filenames, function(x){fread(x,,sep=",", header = T, stringsAsFactors = F)})
  Reduce(function(x,y) {rbind(x,y)}, datalist)}

 #Ejecutar funcion
data <-  multmerge(path) #recibe como argumento la ruta donde estan los chunks
write.csv(data,"consolidado.csv",row.names=FALSE) # Escribir el archivo consolidado
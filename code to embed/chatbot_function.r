### Funcion para chatbot
path4 <- "D:\\Users\\EdgarJJ\\Desktop\\Plumbers\\chunks\\dt_info_pagadores_segmento.csv"

df3 <-  fread(path4,sep=",", header = T, stringsAsFactors = F,
              encoding="UTF-8")

chatbot <- function(id,question){
  if (question=="En que puedo invertir?"){
  if(df3[id_cliente==id,]$segmento == "Experto Digital"){
    "Señor(a) usuari(a), según su perfil transaccional, puede revisar esta opción de inversión:\n Renta Alta Convicción: Alternativa concentrada de Inversión\n Este fondo te permitirá buscar un crecimiento de capital en un horizonte de largo plazo asumiendo un riesgo alto."
  }else if(df3[id_cliente==id,]$segmento == "Asociado Digital"){
    "Señor(a) usuari(a), según su perfil transaccional, puede revisar esta opción de inversión:\n Renta Balanceado Global:\n Esta es una alternativa de inversión a mediano y largo plazo, que te permite tener en un solo producto un portafolio altamente diversificado."
  }else if(df3[id_cliente==id,]$segmento == "Aprendiz Digital"){
    "Señor(a) usuari(a), según su perfil transaccional, puede revisar esta opción de inversión:\n Renta Fija Plus:\n Con Renta Fija Plus encuentras una alternativa para invertir a mediano plazo en emisores no tradicionales."
  }else{
    "Señor(a) usuari(a), según su perfil transaccional, puede revisar esta opción de inversión:\n Fiducuenta:\n Encuentra en la Fiducuenta una opción de inversión a corto plazo, manteniendo disponible tu dinero para el manejo tu liquidez."
    }
  }else if(question=="Cual es la tasa de rendimiento de mi inversion?"){
    if(df3[id_cliente==id,]$segmento == "Experto Digital"){
      "Señor(a) usuari(a), según su perfil transaccional y su fondo de inversion referenciado, podrá tener una tasa de rendimiento aproximada de 12% E.A"
    }else if(df3[id_cliente==id,]$segmento == "Asociado Digital"){
      "Señor(a) usuari(a), según su perfil transaccional y su fondo de inversion referenciado, podrá tener una tasa de rendimiento aproximada de 10,2% E.A"
    }else if(df3[id_cliente==id,]$segmento == "Aprendiz Digital"){
      "Señor(a) usuari(a), según su perfil transaccional y su fondo de inversion referenciado, podrá tener una tasa de rendimiento aproximada de 8,6% E.A"
    }else{
      "Señor(a) usuari(a), según su perfil transaccional y su fondo de inversion referenciado, podrá tener una tasa de rendimiento aproximada de 8% E.A"
    } 
  }else if(question=="Cuanto es el monto minimo de inversion para mi fondo?"){
    if(df3[id_cliente==id,]$segmento == "Experto Digital"){
      "Señor(a) usuari(a), según su perfil transaccional y su fondo de inversion referenciado,el monto minimo para su apertura es de $3.124.968 COP"
    }else if(df3[id_cliente==id,]$segmento == "Asociado Digital"){
      "Señor(a) usuari(a), según su perfil transaccional y su fondo de inversion referenciado,el monto minimo para su apertura es de $3.124.968 COP"
    }else if(df3[id_cliente==id,]$segmento == "Aprendiz Digital"){
      "Señor(a) usuari(a), según su perfil transaccional y su fondo de inversion referenciado,el monto minimo para su apertura es de $ 1.562.484 COP"
    }else{
      "Señor(a) usuari(a), según su perfil transaccional y su fondo de inversion referenciado,el monto minimo para su apertura es de $ 200.000 COP "
    }     
  }

  else{"No entendi tu pregunta, por favor intenta nuevamente"}
  
  }

cat(chatbot(120,"Cuanto es el monto minimo de inversion para mi fondo?"))

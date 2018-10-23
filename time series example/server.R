library(shiny)

shinyServer(function(input, output) {
 
  myData <- reactive({
    inFile <- input$datos
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                     quote=input$quote, dec = input$decimal, stringsAsFactors = F)
    
   })
  
  output$contenido <- renderTable({
    
    data1<-myData()
    if (is.null(data1)){return(NULL)}
    t(head(myData()))
    
  })
  
  output$descripcion<-renderTable({
    
    if (is.null(myData())){return(NULL)}
    
    xtable(t(summary(myData()[,2])))
    
  })
  
  output$n<-renderText({
    
    data1<-myData()
    if (is.null(data1)){return(NULL)}
    length(data1[,2])
    
  })
  
  output$plot<- renderDygraph({
    
    data1<-myData()
    if (is.null(data1)){return(NULL)}
    
    data1[,1]=as.Date(data1[,1], '%d/%m/%Y')
    
    dygraph(as.xts(data1[,2], order.by = data1[,1]), main = "") %>%
      dySeries(label = "Valores") %>%
      dyRangeSelector() %>%
      dyRoller(rollPeriod = 1)
    
    
    
  })
  
  output$hist<- renderPlot({
    
    data1<-myData()
    if (is.null(data1)){return(NULL)}
    
    data1[,1]=as.Date(data1[,1], '%d/%m/%Y')
    hist(data1[,2], main = "", ylab="Frecuencia", xlab = "Valores",
         borde="white",col="darkblue")
    
  })
  
  ########## Polinomico
  
  mod<- reactive({
    
    data1<-myData()
    if (is.null(data1)){return(NULL)}
    
    n<-length(data1[,2])
    corte<-n-ceiling((as.numeric(input$val)/100)*n)
    ajuste<- ts(data1[1:corte,2],frequency = as.numeric(input$frec))
    val<- ts(data1[(corte+1):n,2],frequency = as.numeric(input$frec))
    
    t<-seq(1:length(ajuste))
    t2<-t*t
    t3<-t2*t
    
    if (input$grado=="No Aplica"){mod <- ajuste}
    if (input$grado=="Lineal"){mod<-lm(ajuste~t)}
    if (input$grado=="Cuadratico"){mod<-lm(ajuste~t+t2)}
    if (input$grado=="Cubico"){mod<-lm(ajuste~t+t2+t3)}
    
    return(mod)
      
    
    
  })
  
  st<- reactive({
    
    mod <- mod()
    data1<-myData()
    if (is.null(data1)){return(NULL)}
    if (input$grado=="No Aplica"){return(NULL)}
    if (input$estacional=="No Aplica"){return(NULL)}
    if (input$estacional=="Trigonometricas" & input$frec==2){return("Cambie la frecuencia")}
    
    n<-length(data1[,2])
    corte<-n-ceiling((as.numeric(input$val)/100)*n)
    ajuste<-ts(data1[1:corte,2],frequency = as.numeric(input$frec))
    val<-ts(data1[(corte+1):n,2],frequency = as.numeric(input$frec))
    It=seasonaldummy(ajuste)
    
    
    t<-seq(1:length(ajuste))
    t2<-t*t
    t3<-t2*t
    
    if (input$estacional=="Variables Indicadoras")
      if (input$grado=="Lineal"){mod1<-lm(ajuste~t+It)}
    if (input$grado=="Cuadratico"){mod1<-lm(ajuste~t+t2+It)}
    if (input$grado=="Cubico"){mod1<-lm(ajuste~t+t2+t3+It)}
    
    
    if (input$estacional=="Trigonometricas")
      if(input$frec!=2){
        
        It.trig=fourier(ajuste,floor(as.numeric(input$frec)/2)-1)
        
        if (input$grado=="Lineal"){mod1<-lm(ajuste~t+It.trig)}
        if (input$grado=="Cuadratico"){mod1<-lm(ajuste~t+t2+It.trig)}
        if (input$grado=="Cubico"){mod1<-lm(ajuste~t+t2+t3+It.trig)}
      }
    
    
    return(mod1)
    
    
  })    
  
  output$coef<-renderPrint({
    
    mod<-mod()
    st<- st()
    data1<-myData()
    if (is.null(data1)){return(NULL)}
    if (input$estacional=="Trigonometricas" & input$frec==2){return("Cambie la frecuencia")}
    if(input$grado=="No Aplica"){return("Ajuste un Modelo")}
    if (input$grado!= "No Aplica" & input$estacional=="No Aplica"){mod} else {st}
    
    
  })
  
  output$medfit<-renderTable({
    
    data1<-myData()
    if (is.null(data1)){return(NULL)}
    if (input$estacional=="Trigonometricas" & input$frec==2){return("Cambie la frecuencia")}
    if (input$grado=="No Aplica"){return(NULL)}
    
    n<-length(data1[,2])
    corte<-n-ceiling((as.numeric(input$val)/100)*n)
    ajuste<-ts(data1[1:corte,2],frequency = as.numeric(input$frec))
    val<-ts(data1[(corte+1):n,2],frequency = as.numeric(input$frec))
    
    if (input$grado!= "No Aplica" & input$estacional=="No Aplica")
    {accuracy(fitted(mod()),ajuste)}
    else
    {accuracy(fitted(st()),ajuste)}
    
  })
  
  output$diagnostico<-renderPlot({
    
    st<- st()
    mod<-mod()
    if (is.null(mod)){return(NULL)}
    if (input$grado=="No Aplica"){return(NULL)}
    if (input$estacional=="Trigonometricas" & input$frec==2){return(NULL)}
    
    if (input$estacional=="No Aplica"){
      if (input$grado=="Lineal"){residuales<-residuals(mod)}
      if (input$grado=="Cuadratico"){residuales<-residuals(mod)}
      if (input$grado=="Cubico"){residuales<-residuals(mod)}
    }
    
    if (input$estacional=="Variables Indicadoras"){residuales<-residuals(st)}
    if (input$estacional=="Trigonometricas"){residuales<-residuals(st)}
    
    
    par(mfrow=c(2,2))
    
    t=seq(1,length(residuales))
    plot(t,residuales,
         type='l',
         ylab='',main="Residuales Modelo Polinomico",
         col="red")
    
    abline(h=0,lty=2)       
    
    
    plot(density(residuales),       
         xlab='Residuales',
         main= 'Densidad Residuales Modelo Polinomico', 
         col="red")
    
    abline(v=0)
    
    par(new=T)
    
    x   <- rnorm(1000000,0,sd(residuales))
    plot(density(x),main="",xlab = "",ylab = "", axes=F)
    
    par(new=F)
    
    qqnorm(residuales)              
    qqline(residuales,col=2)         
    
    acf(residuales, ci.type="ma",60) 
    
  })
  
  output$grafica<-renderPlot({
    
    mod<-mod()
    st <- st()
    
    if (is.null(mod)){return(NULL)}
    if (input$estacional=="Trigonometricas" & input$frec==2){return(NULL)}
    
    if (input$interval=="Confianza"){interval="confidence"}
    if (input$interval=="Prediccion"){interval="prediction"}
    
    data1<-myData()
    if (is.null(data1)){return(NULL)}
    
    n<-length(data1[,2])
    corte<-n-ceiling((as.numeric(input$val)/100)*n)
    ajuste<-ts(data1[1:corte,2],frequency = as.numeric(input$frec))
    val<-ts(data1[(corte+1):n,2],frequency = as.numeric(input$frec))
    
    ## Pronostico TENDENCIA
    
    t.val <- seq(corte+1,length(data1[,2]))
    t.val2<-t.val*t.val
    t.val3<-t.val2*t.val
    
    if (input$grado=="No Aplica"){pron <- val}
    if (input$grado=="Lineal") {pron <- predict(mod,data.frame(t=t.val), interval=interval, level=input$confidence/100)}
    if (input$grado=="Cuadratico") {pron <- predict(mod,data.frame(t=t.val,t2=t.val2), interval=interval, level=input$confidence/100)}
    if (input$grado=="Cubico") {pron <- predict(mod,data.frame(t=t.val,t2=t.val2,t3=t.val3), interval=interval, level=input$confidence/100)}
    
    ## Pronostico ESTACIONALIDAD
    Itf = seasonaldummy(ajuste,n-length(ajuste))
    
    
    if (input$estacional=="Variables Indicadoras"){
      if (input$grado=="Lineal") {pron1 <- predict(st,data.frame(t=t.val,It=I(Itf)), interval=interval, level=input$confidence/100)}
      if (input$grado=="Cuadratico") {pron1 <- predict(st,data.frame(t=t.val,t2=t.val2,It=I(Itf)), interval=interval, level=input$confidence/100)}
      if (input$grado=="Cubico") {pron1 <- predict(st,data.frame(t=t.val,t2=t.val2,t3=t.val3,It=I(Itf)), interval=interval, level=input$confidence/100)}
      
    }
    
    if (input$estacional=="Trigonometricas")
      
      if(input$frec!=2){
        
        It.trigf=fourierf(ajuste,floor(as.numeric(input$frec)/2)-1,n-length(ajuste))
        
        
        if (input$grado=="Lineal") {pron1 <- predict(st,data.frame(t=t.val,It.trig=I(It.trigf)), interval=interval, level=input$confidence/100)}
        if (input$grado=="Cuadratico") {pron1 <- predict(st,data.frame(t=t.val,t2=t.val2,It.trig=I(It.trigf)), interval=interval, level=input$confidence/100)}
        if (input$grado=="Cubico") {pron1 <- predict(st,data.frame(t=t.val,t2=t.val2,t3=t.val3,It.trig=I(It.trigf)), interval=interval, level=input$confidence/100)}
      }
    
    ## Plot
    
    fecha=as.Date(data1[,1], '%d/%m/%Y')
    
    np = length(data1[,1])
    ejex.mes = seq(fecha[1],fecha[np],"months")
    ejex.year= seq(fecha[1],fecha[np],"years")
    
    if (is.null(pron)){lim<-pron1}else{lim<-pron}
    
    if (input$estacional=="No Aplica" & input$grado=="No Aplica"){
      plot(data1[,2],lwd=1.5,type="o",main="",
           ylab="Valores", xlab="Tiempo",pch=20)
    } else{
    
    plot(data1[,2],lwd=1.5,type="o",main="",
         ylab="Valores", xlab="Tiempo",pch=20,
         ylim=c(
           min(min(lim[,2]),min(data1[,2])),
           max(max(lim[,3]),max(data1[,2]))
         ))}
    
    if (input$estacional=="No Aplica"){
      if (input$grado=="No Aplica") {lines(0)}
      if (input$grado=="Lineal") {lines(fitted(mod),col="red",lwd=2)}
      if (input$grado=="Cuadratico"){lines(fitted(mod),col="red",lwd=2)}
      if (input$grado=="Cubico"){lines(fitted(mod),col="red",lwd=2)}}
    
    if (input$estacional=="Variables Indicadoras" | input$estacional=="Trigonometricas" ){
      if (input$grado=="No Aplica") {lines(0)}
      if (input$grado=="Lineal") {lines(fitted(st),col="red",lwd=2)}
      if (input$grado=="Cuadratico"){lines(fitted(st),col="red",lwd=2)}
      if (input$grado=="Cubico"){lines(fitted(st),col="red",lwd=2)}}
    
    abline(v=corte)
    
    if (input$estacional=="No Aplica"){
      if (input$grado=="No Aplica"){lines(0)}
      if (input$grado=="Lineal"){lines(t.val,pron[,1],col="blue",lwd=2)
        lines(t.val,pron[,2],col="blue",lwd=2,lty=2)
        lines(t.val,pron[,3],col="blue",lwd=2,lty=2)}
      if (input$grado=="Cuadratico"){lines(t.val,pron[,1],col="blue",lwd=2)
        lines(t.val,pron[,2],col="blue",lwd=2,lty=2)
        lines(t.val,pron[,3],col="blue",lwd=2,lty=2)}
      if (input$grado=="Cubico"){lines(t.val,pron[,1],col="blue",lwd=2)
        lines(t.val,pron[,2],col="blue",lwd=2,lty=2)
        lines(t.val,pron[,3],col="blue",lwd=2,lty=2)}}
    
    if (input$estacional=="Variables Indicadoras" |input$estacional=="Trigonometricas" ){
      if (input$grado=="No Aplica"){lines(0)}
      if (input$grado=="Lineal"){lines(t.val,pron1[,1],col="blue",lwd=2)
        lines(t.val,pron1[,2],col="blue",lwd=2,lty=2)
        lines(t.val,pron1[,3],col="blue",lwd=2,lty=2)}
      if (input$grado=="Cuadratico"){lines(t.val,pron1[,1],col="blue",lwd=2)
      lines(t.val,pron1[,2],col="blue",lwd=2,lty=2)
      lines(t.val,pron1[,3],col="blue",lwd=2,lty=2)}
      if (input$grado=="Cubico"){lines(t.val,pron1[,1],col="blue",lwd=2)
        lines(t.val,pron1[,2],col="blue",lwd=2,lty=2)
        lines(t.val,pron1[,3],col="blue",lwd=2,lty=2)}}
    
    
    ifelse ((input$grado=="No Aplica"),(legend("topleft", c("Serie real"),col="black",pch=20,lwd=1.5,bty = "n")),
            legend( "topleft",                                     
                    c("Serie real","Ajuste","Pronostico","Intervalo"),         
                    lwd = c(1.5, 2, 2, 2),   
                    col=c("black","red","blue","blue"),
                    pch = c(20,NA,NA,NA),
                    bty = "n",
                    lty=c(1,1,1,2)))                            
    
    
    
    
    grid()
    
})
  
  pron<- reactive({
    
    data1<-myData()
    mod<- mod()
    if (is.null(data1)){return(NULL)}
    
    if (input$interval=="Confianza"){interval="confidence"}
    if (input$interval=="Prediccion"){interval="prediction"}
    
    n<-length(data1[,2])
    corte<-n-ceiling((as.numeric(input$val)/100)*n)
    ajuste<-data1[1:corte,2]
    val<-data1[(corte+1):n,2]
    
    
    t.val <- seq(corte+1,length(data1[,2]))
    t.val2<-t.val*t.val
    t.val3<-t.val2*t.val
       
      if (input$grado=="No Aplica"){pron <- val}
      if (input$grado=="Lineal") {pron <- predict(mod,data.frame(t=t.val), interval=interval, level=input$confidence/100)}
      if (input$grado=="Cuadratico") {pron <- predict(mod,data.frame(t=t.val,t2=t.val2), interval=interval, level=input$confidence/100)}
      if (input$grado=="Cubico") {pron <- predict(mod,data.frame(t=t.val,t2=t.val2,t3=t.val3), interval=interval, level=input$confidence/100)}
    
      return(pron)
    
    })

  pron1<- reactive({
      
      
      data1<-myData()
      st <- st()	
      mod<- mod()
      if (is.null(data1)){return(NULL)}
      
      if (input$interval=="Confianza"){interval="confidence"}
      if (input$interval=="Prediccion"){interval="prediction"}
      
      n<-length(data1[,2])
      corte<-n-ceiling((as.numeric(input$val)/100)*n)
      ajuste<-ts(data1[1:corte,2],frequency = as.numeric(input$frec))
      val<-ts(data1[(corte+1):n,2],frequency = as.numeric(input$frec))
      
      
      t.val <- seq(corte+1,length(data1[,2]))
      t.val2<-t.val*t.val
      t.val3<-t.val2*t.val
      Itf = seasonaldummy(ajuste,n-length(ajuste))
      
      
      if (input$estacional=="Variables Indicadoras"){
        if (input$grado=="Lineal") {pron1 <- predict(st,data.frame(t=t.val,It=I(Itf)), interval=interval, level=input$confidence/100)}
        if (input$grado=="Cuadratico") {pron1 <- predict(st,data.frame(t=t.val,t2=t.val2,It=I(Itf)), interval=interval, level=input$confidence/100)}
        if (input$grado=="Cubico") {pron1 <- predict(st,data.frame(t=t.val,t2=t.val2,t3=t.val3,It=I(Itf)), interval=interval, level=input$confidence/100)}
        
      }
      
      if (input$estacional=="Trigonometricas")
        
        if(input$frec!=2){
          
          It.trigf=fourierf(ajuste,floor(as.numeric(input$frec)/2)-1,n-length(ajuste))
          
          
          if (input$grado=="Lineal") {pron1 <- predict(st,data.frame(t=t.val,It.trig=I(It.trigf)), interval=interval, level=input$confidence/100)}
          if (input$grado=="Cuadratico") {pron1 <- predict(st,data.frame(t=t.val,t2=t.val2,It.trig=I(It.trigf)), interval=interval, level=input$confidence/100)}
          if (input$grado=="Cubico") {pron1 <- predict(st,data.frame(t=t.val,t2=t.val2,t3=t.val3,It.trig=I(It.trigf)), interval=interval, level=input$confidence/100)}
        }
      
        return(pron1)
      
    })
    
  output$medfor<- renderTable({
      
      data1<-myData()
      pron <- pron() 
      pron1 <- pron1()
      if (is.null(data1)){return(NULL)}
      if (input$grado=="No Aplica"){return(NULL)}
      if (input$estacional=="Trigonometricas" & input$frec==2){return("Cambie la frecuencia")}
      
      n<-length(data1[,2])
      corte<-n-ceiling((as.numeric(input$val)/100)*n)
      ajuste<-ts(data1[1:corte,2],frequency = as.numeric(input$frec))
      val<-ts(data1[(corte+1):n,2],frequency = as.numeric(input$frec))
      
      if (input$grado!= "No Aplica" & input$estacional=="No Aplica")
      {accuracy(pron[,1],val)}
      else
      {accuracy(pron1[,1],val)}
      
      
    })
  
  fore<-reactive({
    if (input$estacional!='No Aplica'){
      
      data1<-myData()
      st <- st()	
      mod<- mod()
      if (is.null(data1)){return(NULL)}
      
      if (input$interval=="Confianza"){interval="confidence"}
      if (input$interval=="Prediccion"){interval="prediction"}
      
      n<-length(data1[,2])
      corte<-n-ceiling((as.numeric(input$val)/100)*n)
      ajuste<-ts(data1[1:corte,2],frequency = as.numeric(input$frec))
      val<-ts(data1[(corte+1):n,2],frequency = as.numeric(input$frec))
      
      t.val <- seq(n+1,n+input$n_pron)
      t.val2<-t.val*t.val
      t.val3<-t.val2*t.val
      Itf = seasonaldummy(ajuste,input$n_pron)
      
      
      if (input$estacional=="Variables Indicadoras"){
        if (input$grado=="Lineal") {pron1 <- predict(st,data.frame(t=t.val,It=I(Itf)), interval=interval, level=input$confidence/100)}
        if (input$grado=="Cuadratico") {pron1 <- predict(st,data.frame(t=t.val,t2=t.val2,It=I(Itf)), interval=interval, level=input$confidence/100)}
        if (input$grado=="Cubico") {pron1 <- predict(st,data.frame(t=t.val,t2=t.val2,t3=t.val3,It=I(Itf)), interval=interval, level=input$confidence/100)}
        
      }
      
      if (input$estacional=="Trigonometricas")
        
        if(input$frec!=2){
          
          It.trigf=fourierf(ajuste,floor(as.numeric(input$frec)/2)-1,input$n_pron)
          
          
          if (input$grado=="Lineal") {pron1 <- predict(st,data.frame(t=t.val,It.trig=I(It.trigf)), interval=interval, level=input$confidence/100)}
          if (input$grado=="Cuadratico") {pron1 <- predict(st,data.frame(t=t.val,t2=t.val2,It.trig=I(It.trigf)), interval=interval, level=input$confidence/100)}
          if (input$grado=="Cubico") {pron1 <- predict(st,data.frame(t=t.val,t2=t.val2,t3=t.val3,It.trig=I(It.trigf)), interval=interval, level=input$confidence/100)}
        }
      
      return(pron1)
      
      
    } 
    
    else {
    
      data1<-myData()
      mod<- mod()
      if (is.null(data1)){return(NULL)}
      
      if (input$interval=="Confianza"){interval="confidence"}
      if (input$interval=="Prediccion"){interval="prediction"}
      
      n<-length(data1[,2])
      corte<-n-ceiling((as.numeric(input$val)/100)*n)
      ajuste<-ts(data1[1:corte,2],frequency = as.numeric(input$frec))
      val<-ts(data1[(corte+1):n,2],frequency = as.numeric(input$frec))
      
      t.val <- seq(n+1,n+input$n_pron)
      t.val2<-t.val*t.val
      t.val3<-t.val2*t.val
      
      if (input$grado=="No Aplica"){pron <- val}
      if (input$grado=="Lineal") {pron <- predict(mod,data.frame(t=t.val), interval=interval, level=input$confidence/100)}
      if (input$grado=="Cuadratico") {pron <- predict(mod,data.frame(t=t.val,t2=t.val2), interval=interval, level=input$confidence/100)}
      if (input$grado=="Cubico") {pron <- predict(mod,data.frame(t=t.val,t2=t.val2,t3=t.val3), interval=interval, level=input$confidence/100)}
      
      return(pron)
    }
  })
  
  output$downloadData<-downloadHandler(
      filename = function() { 
      paste("pronosticos_polinomico","csv",sep='.') 
    },
    content = function(file) {
      write.table(fore(), file, sep = ';',
                  row.names = FALSE)
    })
    
  ###################### Suavizado
  
  mod_suav<- reactive({
      
      if (is.null(myData())){return(NULL)}
      
      data1<-myData()
      colnames(data1)=c("Fecha","Valor")
      n<-length(data1[,2])
      corte<-n-ceiling((as.numeric(input$val_suav)/100)*n)
      ajuste<-as.data.frame(data1[1:corte,2])
      val<-as.data.frame(data1[(corte+1):n,2])
      
      colnames(ajuste)<-c("Valor")
      colnames(val)<-c("Valor")
      
      if (input$method=="Aditivo"){mod_suav<-HoltWinters(ts(ajuste, frequency = as.numeric(input$frequency_suav)), alpha = input$alpha,
                                                         beta = input$beta, gamma = input$gamma, seasonal = "additive")}
      
      if (input$method=="Multiplicativo"){mod_suav<-HoltWinters(ts(ajuste, frequency = as.numeric(input$frequency_suav)), alpha = input$alpha,
                                                                beta = input$beta, gamma = input$gamma, seasonal = "multiplicative")}
      
      if (input$method=="Automatico"){mod_suav<-HoltWinters(ts(ajuste, frequency = as.numeric(input$frequency_suav)))}
      
      
      return(mod_suav)
      
    })
    
  output$coef_suav<-renderPrint({
      
      if (is.null(myData())){return(NULL)}
      
      if (input$method=="No Aplica"){return("Escoga metodo")}
      
      mod_suav()
      
    })
    
  output$diagnostico_holt<-renderPlot({
      
      mod<-mod_suav()
      if (is.null(mod_suav)){return(NULL)}
      
      if (input$method!="No Aplica"){
      
      residuales<-residuals(mod)
      
      par(mfrow=c(2,2))
      
      t=seq(1,length(residuales))
      
      plot(t,residuales,
           type='l',
           ylab='',main="Residuales Modelo Polinomico",
           col="red")
      
      abline(h=0,lty=2)       # Linea para la media
      
      x   <- rnorm(1000000,0,sd(residuales))
      
      plot(density(residuales),       # Grafica de densidad
           xlab='Residuales',
           main= 'Densidad Residuales Modelo Polinomico', 
           col="red", ylim=c(0,max(max((density(x))$y),max(density(residuales)$y))))
      
      abline(v=0)
      
      lines(density(x))
      
      qqnorm(residuales)               # Grafica qqnorm para probar normalidad
      qqline(residuales,col=2)         # Linea
      
      acf(residuales, ci.type="ma",60)}
      
      else {return(NULL)}
    })
    
  output$grafica_holt<-renderPlot({
      
      mod<-mod_suav()
      
      if (is.null(mod_suav)){return(NULL)}
      
      data1<-myData()
      if (is.null(data1)){return(NULL)}
      
      
      if (input$method=="No Aplica"){
        n<-length(data1[,2])
        t=seq(1,n)
          plot(t,data1[,2],lwd=1.5,type="o",
             ylab="Valores", xlab="Tiempo",pch=20, ylim=c(min(data1[,2]),max(data1[,2])))
          grid()
      }
      
      if (input$method != "No Aplica"){
      
      n<-length(data1[,2])
      t=seq(1,n)
      corte<-n-ceiling((as.numeric(input$val_suav)/100)*n)
      t.val <- seq(corte+1,n)
      val<-as.data.frame(data1[(corte+1):n,2])
      
      pron <- predict(mod, n.ahead=(n-corte), prediction.interval = T, level=input$confidence_suav/100)
      
      ## Plot
      
      plot(t,data1[,2],lwd=1.5,type="o",
             ylab="Valores", xlab="Tiempo",pch=20, ylim=c(min(min(pron[,3]),min(data1[,2])),max(max(pron[,2]),max(data1[,2]))))

      grid()
      abline(v=corte, lty=2)
      
      lines(y=mod$fitted[,1],x=seq(corte-length(mod$fitted[,1])+1,corte),col="red",lwd=2)
      lines(y=pron[,1],x=t.val,col="blue",lwd=2)
      lines(y=pron[,2],x=t.val,col="blue",lwd=2, lty=2)
      lines(y=pron[,3],x=t.val,col="blue",lwd=2, lty=2)}
      
      ifelse ((input$method=="No Aplica"),(legend("topleft", c("Serie real"),col="black",pch=20,lwd=1.5,bty = "n")),
              legend( "topleft",                                     
                      c("Serie real","Ajuste","Pronostico","Intervalo"),         
                      lwd = c(1.5, 2, 2, 2),   
                      col=c("black","red","blue","blue"),
                      pch = c(20,NA,NA,NA),
                      bty = "n",
                      lty=c(1,1,1,2)))  
      
    })
    
  output$medfor_suav<-renderTable({
      
      mod<-mod_suav()
      
      if (is.null(mod_suav)){return(NULL)}
      
      if (input$method=="No Aplica"){return(NULL)}
      
      data1<-myData()
      if (is.null(data1)){return(NULL)}
      
      n<-length(data1[,2])
      t=seq(1,n)
      corte<-n-ceiling((as.numeric(input$val_suav)/100)*n)
      t.val <- seq(corte+1,n)
      val<-as.data.frame(data1[(corte+1):n,2])
      
      pron <- predict(mod, n.ahead=(n-corte), prediction.interval = T, level=input$confidence_suav/100)
      
      xtable(as.table(accuracy(pron[,1],val[,1])))
      
    }, include.rownames=T)
    
  output$medfit_suav<-renderTable({
      
      data1<-myData()
      if (is.null(data1)){return(NULL)}
      if (input$method=="No Aplica"){return(NULL)}
      
      n<-length(data1[,2])
      corte<-n-ceiling((as.numeric(input$val_suav)/100)*n)
      ajuste<-data1[1:corte,2]
      val<-data1[(corte+1):n,2]
      
      accuracy(fitted(mod_suav()),ajuste)

    })
  
  fore_suav<-reactive({
    mod<-mod_suav()
    
    if (is.null(mod_suav)){return(NULL)}
    
    data1<-myData()
    
    if (is.null(data1)){return(NULL)}
    
    if (input$method != "No Aplica"){
      pron <- predict(mod, n.ahead=input$n_pron_suav, prediction.interval = T, level=input$confidence_suav/100)
      return(pron)
    }
    
  })
  
  output$downloadData_suav<-downloadHandler(
    filename = function() { 
      paste("pronosticos_holt","csv",sep='.') 
    },
    content = function(file) {
      write.table(fore_suav(), file, sep = ';',
                  row.names = F)
    })
    
  ###################### Autoregresivo
    
  mod_auto<- reactive({
      
      if (is.null(myData())){return(NULL)}
      
      data1<-myData()
      colnames(data1)=c("Fecha","Valor")
      n<-length(data1[,2])
      corte<-n-ceiling((as.numeric(input$val_auto)/100)*n)
      ajuste<-as.data.frame(data1[1:corte,2])
      val<-as.data.frame(data1[(corte+1):n,2])
      ajuste<-ts(ajuste,start=1,frequency = as.numeric(input$frequency_auto))
      
      colnames(ajuste)<-c("Valor")
      colnames(val)<-c("Valor")
      
      mod_auto<-auto.arima(ajuste, max.d = input$d, max.D = input$d_s, max.p = input$ar, max.q = input$ma,
                      max.P = input$ar_s, max.Q = input$ma_s, allowdrift = T, allowmean = T)

      # mod_auto<-Arima(ajuste, order=c(input$ar, input$d, input$ma), seasonal = list(order = c(input$ar_s, input$d_s, input$ma_s), period = input$frequency_auto), method = "ML")
      
      
      return(mod_auto)
      
    })
    
  output$coef_auto<-renderPrint({
      
      if (is.null(myData())){return(NULL)}
      
      mod_auto()
      
    })
    
  output$diagnostico_auto<-renderPlot({
      
      mod<-mod_auto()
      if (is.null(mod)){return(NULL)}
      
      residuales<-residuals(mod)
      
      par(mfrow=c(2,2))
      
      t=seq(1,length(residuales))
      
      plot(t,residuales,
           type='l',
           ylab='',main="Residuales Modelo Polinomico",
           col="red")
      
      abline(h=0,lty=2)       # Linea para la media
      
      x   <- rnorm(1000000,0,sd(residuales))
      
      plot(density(residuales),       # Grafica de densidad
           xlab='Residuales',
           main= 'Densidad Residuales Modelo Polinomico', 
           col="red", ylim=c(0,max(max((density(x))$y),max(density(residuales)$y))))
      
      abline(v=0)
      
      lines(density(x))
      
      qqnorm(residuales)               # Grafica qqnorm para probar normalidad
      qqline(residuales,col=2)         # Linea
      
      acf(residuales, ci.type="ma",60) # Prueba ACF
      
    })
    
  output$grafica_auto<-renderPlot({
      
      mod<-mod_auto()
      
      if (is.null(mod_auto)){return(NULL)}
      
      data1<-myData()
      if (is.null(data1)){return(NULL)}
      
      n<-length(data1[,2])
      t=seq(1,n)
      corte<-n-ceiling((as.numeric(input$val_auto)/100)*n)
      t.val <- seq(corte+1,n)
      val<-as.data.frame(data1[(corte+1):n,2])
      
      pron <- forecast(mod,h=(n-corte),level=input$confidence_auto/100)
      
      ## Plot
      
      plot(t,data1[,2],lwd=1.5, type="o",
           ylab="Valores", xlab="Tiempo",pch=20, ylim=c(min(min(as.numeric(pron$lower)),min(data1[,2])),max(max(as.numeric(pron$upper)),max(data1[,2]))))
      grid()
      abline(v=corte, lty=2)
      lines(y=fitted(mod),x=seq(1,corte),col="red",lwd=2)
      lines(y=as.numeric(pron$mean),x=t.val,col="blue",lwd=2)
      lines(y=as.numeric(pron$lower),x=t.val,col="blue",lwd=2,lty=2)
      lines(y=as.numeric(pron$upper),x=t.val,col="blue",lwd=2, lty=2)
      
      legend( "topleft",                                     
              c("Serie real","Ajuste","Pronostico","Intervalo"),         
              lwd = c(1.5, 2, 2, 2),   
              col=c("black","red","blue","blue"),
              pch = c(20,NA,NA,NA),
              bty = "n",
              lty=c(1,1,1,2))
      
    })
    
  output$medfor_auto<-renderTable({
      
      mod<-mod_auto()
      
      if (is.null(mod)){return(NULL)}
      
      data1<-myData()
      if (is.null(data1)){return(NULL)}
      
      n<-length(data1[,2])
      t=seq(1,n)
      corte<-n-ceiling((as.numeric(input$val_auto)/100)*n)
      t.val <- seq(corte+1,n)
      val<-as.data.frame(data1[(corte+1):n,2])
      
      pron <-  forecast(mod,h=(n-corte),level=input$confidence_auto/100)
      
      xtable(as.table(accuracy(pron$mean,val[,1])))
      
    })
    
  output$medfit_auto<-renderTable({
      
      data1<-myData()
      if (is.null(data1)){return(NULL)}
      
      n<-length(data1[,2])
      corte<-n-ceiling((as.numeric(input$val_auto)/100)*n)
      ajuste<-data1[1:corte,2]
      val<-data1[(corte+1):n,2]
      
      accuracy(fitted(mod_auto()),ajuste)
      
    })
  
  fore_auto<-reactive({
    mod<-mod_auto()
    
    if (is.null(mod_auto)){return(NULL)}
    
    data1<-myData()
    
    if (is.null(data1)){return(NULL)}
    
    pron <- forecast(mod,h=input$n_pron_auto,level=input$confidence_auto/100)
      return(pron)

})
  
  output$downloadData_auto<-downloadHandler(
    filename = function() { 
      paste("pronosticos_sarima","csv",sep='.') 
    },
    content = function(file) {
      write.table(fore_auto(), file, sep = ';',
                  row.names = F)
    })
    
})
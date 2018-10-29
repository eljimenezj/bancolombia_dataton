# Instalamos librerias
library(scales)
library(plotly)
library(ggplot2)
library(plotrix)
library(knitr)
library(plotly)
require(smooth)
library(data.table)

# Cargamos datos

#fread('https://bancoblob.blob.core.windows.net/data/dt_info_pagadores_descriptivo.csv')

path <- "data/consolidado.csv"
path2 <- "data/dt_info_pagadores_ingresos.csv"
path3 <- "data/dt_info_pagadores_segmento.csv"
path4 <- "data/dt_info_pagadores_descriptivo.csv"

df <- fread(path,sep=",", header = T, stringsAsFactors = F,
            encoding="UTF-8")

df2 <- fread(path2,sep=",", header = T, stringsAsFactors = F,
             encoding="UTF-8")

df3 <- fread(path3,sep=",", header = T, stringsAsFactors = F,
             encoding="UTF-8")

df4 <-  fread(path4,sep=",", header = T, stringsAsFactors = F,
              encoding="UTF-8")

colnames(df) <- c("id_trans","id_cliente","fecha","hora",
                  "valor_trx","ref1","ref2","ref3",
                  "sector","subsector","descripcion")

## Transformacioin columna fechas
df$fecha <- as.Date(as.character(df$fecha), "%Y%m%d")

server <- function(input, output, session) {
    
    output$segment <- renderValueBox({
        id <- as.integer(input$id)
        df_id <- df3[id_cliente==as.integer(id),]
        valueBox(
            value = df_id$segmento[1],
            subtitle = "Segmento",
            icon = icon("user", lib = "glyphicon"),
            color = "green"
        )
    })
    
    output$monto <- renderValueBox({
        id <- as.integer(input$id)
        df_id <- df3[id_cliente==as.integer(id),]
        valueBox(
            value = paste0("$", formatC(as.numeric(df_id$monetary[1]), format="f", digits=2, big.mark=",")),
            subtitle = "Monto",
            icon = icon("usd", lib = "glyphicon"),
            color = "blue"
        )
    })
    
    output$freq <- renderValueBox({
        id <- as.integer(input$id)
        df_id <- df3[id_cliente==as.integer(id),]
        valueBox(
            value = df_id$freq[1],
            subtitle = "Frecuencia",
            icon = icon("signal", lib = "glyphicon"),
            color = "aqua"
        )
    })
    
    output$recen <- renderValueBox({
        id <- as.integer(input$id)
        df_id <- df3[id_cliente==as.integer(id),]
        valueBox(
            value = df_id$recency[1],
            subtitle = "Recencia",
            icon = icon("star", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$consumo <- renderValueBox({
        id <- as.integer(input$id)
        df_id <- df3[id_cliente==as.integer(id),]
        valueBox(
            value = paste0("$", formatC(as.numeric(df_id$consumo_hist[1]), format="f", digits=2, big.mark=",")),
            subtitle = "Consumo historico",
            icon = icon("stats", lib = "glyphicon"),
            color = "red"
        )
    })
    
    output$points <- renderValueBox({
        id <- as.integer(input$id)
        df_id <- df3[id_cliente==as.integer(id),]
        valueBox(
            value = df_id$total_point[1],
            subtitle = "Puntos",
            icon = icon("heart", lib = "glyphicon"),
            color = "purple"
        )
    })
    
    
    output$plot1 <- renderPlotly({
        id <- as.integer(input$id)
        df_id <- df[id_cliente==as.integer(id),]
        
        if (input$period == 1){
            label <- factor(month(df_id$fecha))
        } else if (input$period == 2) {
            label <- factor(quarter(df_id$fecha))
        } else {
            label <- factor(year(df_id$fecha))
        }

        p10   <- ggplot(df_id, aes(x = label, 
                                   y = valor_trx)) +  geom_boxplot(fill = "#220095", colour = "black",
                                                                   alpha = 0.7)
        p10 <- p10 + ggtitle(paste("Transacciones por tiempo. ID: ",id))
        p10 <- p10 + scale_x_discrete(name = "Tiempo") +
            scale_y_continuous(labels = scales::comma, name = "$")
        
        ggplotly(p10)
    })
    
    output$plot2 <- renderPlotly({
        id <- as.integer(input$id)
        df_id <- df[id_cliente==as.integer(id),]
        
        p10   <- ggplot(df_id, aes(x = fecha, 
                                   y = valor_trx)) +  geom_line(fill = "#220095", colour = "black",
                                                                   alpha = 0.7)
        p10 <- p10 + ggtitle(paste("Transacciones. ID: ",id))
        p10 <- p10 + scale_x_date(name = "Tiempo") +
            scale_y_continuous(labels = scales::comma, name = "$")
        
        ggplotly(p10)
    })
    
    output$plot3 <- renderPlotly({
        id <- as.integer(input$id)
        df_id <- df[id_cliente==as.integer(id),]
        
        if (input$ref == 1){
            p10   <- plot_ly(df_id, labels = ~ref1, values = ~valor_trx, type = 'pie') 
        } else if (input$ref == 2){
            p10   <- plot_ly(df_id, labels = ~ref2, values = ~valor_trx, type = 'pie') 
        } else {
            p10   <- plot_ly(df_id, labels = ~ref3, values = ~valor_trx, type = 'pie') 
        }
        
        p10 <- p10 %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   showlegend = FALSE)
        
        p10
    })
    
    
    output$plot4 <- renderPlotly({
        id <- as.integer(input$id)
        df_id <- df[id_cliente==as.integer(id),]
        
        if (input$sector == 1){
            p10   <- plot_ly(df_id, x = ~sector, y = ~valor_trx, type = 'bar', color = ~sector) %>%  layout(xaxis = list(title = "Sector")) 
        } else if (input$sector == 2){
            p10   <- plot_ly(df_id, x = ~subsector, y = ~valor_trx, type = 'bar', color = ~subsector) %>%  layout(xaxis = list(title = "Subsector")) 
        } else {
            p10   <- plot_ly(df_id, x = ~descripcion, y = ~valor_trx, type = 'bar', color = ~descripcion) %>%  layout(xaxis = list(title = "Descripcion")) 
        }
        
        p10 <- p10 %>%  layout(showlegend = FALSE, yaxis = list(title = "$")) 
        
        p10
    })
    
    
    # Saving 
    
    fc <- function(id){
        df_id <- data.frame(valor_trx = df[id_cliente==id,]$valor_trx)
        dfc   <- sma(df_id$valor_trx , h=3, silent=FALSE)
        f     <- data.frame(valor_trx=as.numeric(dfc$forecast + (sd(df_id$valor_trx)*runif(1,0,0.1))*
                                                     round(runif(1,-1,1),digits = 0)))
        #return(rbind(df_id,f))
        return(mean(f$valor_trx))
    }
    
    fb <- function(id){
        df_id     <- data.frame(valor_trx = df[id_cliente==id,]$valor_trx)
        dfc       <- sma(df_id$valor_trx , h=3, silent=FALSE)
        egreso    <- data.frame(valor_trx=as.numeric(dfc$forecast + (sd(df_id$valor_trx)*runif(1,0,0.1))*
                                                         round(runif(1,-1,1),digits = 0)))
        ingresos  <- data.frame(ingreso = df2[id_cliente==id,]$ingreso)
        periodo   <- c(1:length(egreso$valor_trx))
        df_plot   <- data.frame(periodo,ingresos,egreso)
        return(df_plot)
    }
    
    sc <- function(id,y){
        df_id     <- data.frame(valor_trx = df[id_cliente==id,]$valor_trx)
        dfc       <- sma(df_id$valor_trx , h=3, silent=FALSE)
        ingresos  <- data.frame(ingreso = rep(df2[id_cliente==id,]$ingreso,3))
        egreso    <- data.frame(valor_trx=as.numeric(dfc$forecast + (sd(df_id$valor_trx)*runif(1,0,0.1))*
                                                         round(runif(1,-1,1),digits = 0)))
        ahorro           <- data.frame(ahorro=(ingresos*y)-egreso)
        colnames(ahorro) <- c("Ahorro")
        return(ahorro)
    }
    
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
    
    prestamo_a_otorgar <- function(id, p_ahorro){
        if(df4[id_cliente==id,]$seg_str == "OTRO"){
            tasa <- 20
            factor_mult <- 10.886508
        }else if(df4[id_cliente==id,]$seg_str == "PERSONAL"){
            tasa <- 18
            factor_mult <- 10.983414
        }else if(df4[id_cliente==id,]$seg_str == "PERSONAL PLUS"){
            tasa <- 16
            factor_mult <- 11.083126
        }else if(df4[id_cliente==id,]$seg_str == "EMPRENDEDOR"){
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
    
    lend_predict <- function(id){
        
        model <- readRDS("models/tree_model.rds")
        
        df4_1 <- copy(df4)
        
        df4_1[, id_cliente := as.integer(id_cliente)]
        suppressWarnings(df4_1[, edad := as.integer(edad)])
        edad_mean <- df4_1[, round(mean(edad, na.rm = T))]
        df4_1[is.na(edad), edad := edad_mean]
        df4_1[, seg_str := as.factor(seg_str)]
        df4_1[, ocupacion := as.factor(ocupacion)]
        df4_1[, tipo_vivienda := as.factor(tipo_vivienda)]
        df4_1[, nivel_academico := as.factor(nivel_academico)]
        df4_1[, estado_civil := as.factor(estado_civil)]
        df4_1[, ingreso_rango := as.factor(ingreso_rango)]
        
        cliente <- df4_1[id_cliente == id]
        if(nrow(cliente) == 0){
            return(0)
        }else{
            predict.val <- predict(model, cliente)
            return(ifelse((predict.val[, 2] < 0.7), 0, 1))
        }
    }
    
    output$plotcost <- renderPlotly({
        id <- as.integer(input$id)
        df_plot <- fb(id)
        
        # Grafica
        print(df_plot)
        scale_fac <- mean(df_plot$valor_trx)/min(df_plot$ingreso)
        
        p10 <- ggplot(df_plot, aes(x=periodo)) + 
            geom_line(df_plot, y = valor_trx) + 
            geom_line(df_plot, y = ingreso, aes(y=ingreso * scale_fac)) +
            scale_y_continuous(sec.axis= sec_axis(~.*scale, name="Ingresos"))
        
        p10 <- p10 + ggtitle(paste("Ingresos estimados vs Pronóstico Gastos PSE \n Usuario ID: ",id))
        p10 <- p10 + 
            scale_y_continuous(labels = scales::comma, name = "Cifras en pesos COP")
        
        ggplotly(p10)
    })
    
    output$month1 <- renderValueBox({
        id <- as.integer(input$id)
        df_plot <- sc(id, as.integer(input$saving_rate)/100)
        valueBox(
            value = paste0("$", formatC(as.numeric(df_plot$Ahorro[1]), format="f", digits=2, big.mark=",")),
            subtitle = "Ahorro mes 1",
            icon = icon("usd", lib = "glyphicon"),
            color = if (df_plot$Ahorro[1] <= as.integer(input$meta)) "red" else "green"
        )
    })
    
    output$month2 <- renderValueBox({
        id <- as.integer(input$id)
        df_plot <- sc(id, as.integer(input$saving_rate)/100)
        valueBox(
            value = paste0("$", formatC(as.numeric(df_plot$Ahorro[1]), format="f", digits=2, big.mark=",")),
            subtitle = "Ahorro mes 2",
            icon = icon("usd", lib = "glyphicon"),
            color = if (df_plot$Ahorro[2] <= as.integer(input$meta)) "red" else "green"
        )
    })
    
    output$month3 <- renderValueBox({
        id <- as.integer(input$id)
        df_plot <- sc(id, as.integer(input$saving_rate)/100)
        valueBox(
            value = paste0("$", formatC(as.numeric(df_plot$Ahorro[1]), format="f", digits=2, big.mark=",")),
            subtitle = "Ahorro mes 3",
            icon = icon("usd", lib = "glyphicon"),
            color = if (df_plot$Ahorro[3] <= as.integer(input$meta)) "red" else "green"
        )
    })

    # Credit 
    
    output$credit_lend <- reactive({
        lend_predict(input$id)
    })
    
    outputOptions(output, "credit_lend", suspendWhenHidden = FALSE) 
    
    output$rate <- renderValueBox({
        amount <- prestamo_a_otorgar(input$id, as.integer(input$saving_rate)/100)
        valueBox(
            value = amount$Tasa[1],
            subtitle = "Tasa",
            icon = icon("ok", lib = "glyphicon"),
            color = "green"
        )
    })
    
    output$amount <- renderValueBox({
        amount <- prestamo_a_otorgar(input$id, as.integer(input$saving_rate)/100)
        valueBox(
            value = paste0("$", formatC(as.numeric(amount$Credito[1]), format="f", digits=2, big.mark=",")),
            subtitle = "Monto",
            icon = icon("usd", lib = "glyphicon"),
            color = "green"
        )
    })
    
    output$periods <- renderValueBox({
        valueBox(
            value = 12,
            subtitle = "Meses",
            icon = icon("calendar", lib = "glyphicon"),
            color = "green"
        )
    })
    
    # Markdown download button
    
    output$downloadCert <- downloadHandler(
        filename = function() { 
            paste("credit-certificate-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
            rate='20%'
            user='20'
            monto=paste0("$", formatC(as.numeric('200000'), format="f", digits=2, big.mark=","))
            out = knit2pdf('pdf_shell.Rnw', clean = TRUE)
            file.rename(out, file) # move pdf to file for downloading
        }, contentType = 'application/pdf')
    
    # Observer to handle changes to the username
    observe({
        linePrefix <- function(){
            if (is.null(isolate(vars$chat))){
                return("")
            }
            return("<br />")
        }    
            # We want a reactive dependency on this variable, so we'll just list it here.
        input$user
        
        # Generamos los valores de los chats
        
        vars <- reactiveValues(chat=NULL, user=NULL)
        vars$user <- input$id
        vars$chat <- paste0(linePrefix(),
                            tags$span(class="username",
                                      tags$abbr(title=Sys.time(), "Asistente Virtual")
                            ),
                            ": ",
                            "Hola ", vars$user, "! En que te puedo ayudar?")
        
    
        
        # Listen for input$send changes (i.e. when the button is clicked)
        observe({
            if(input$send < 1){
                # The code must be initializing, b/c the button hasn't been clicked yet.
                return()
            }
            isolate({
                # Add the current entry to the chat log.
                vars$chat <- c(vars$chat, 
                                paste0(linePrefix(),
                                       tags$span(class="username",
                                                 tags$abbr(title=Sys.time(), vars$user)
                                       ),
                                       ": ",
                                       tagList(input$entry)))
                
                
                
                vars$chat <- c(vars$chat,  paste0(linePrefix(),
                                                  tags$span(class="username",
                                                            tags$abbr(title=Sys.time(), "Asistente Virtual")
                                                  ),
                                                  ": ",
                                                  tagList(input$entry)))
                # a("xx",href="http://www.xx.com")
            })
            # Clear out the text entry field.
            updateTextInput(session, "entry", value="")
        })
   
    
    # Dynamically create the UI for the chat window.
    output$chat <- renderUI({
        if (length(vars$chat) > 500){
            # Too long, use only the most recent 500 lines
            vars$chat <- vars$chat[(length(vars$chat)-500):(length(vars$chat))]
        }
        
        # Pass the chat log through as HTML
        HTML(vars$chat)
        })
    })
    
    
    
    # Messages
    
    messageData <- data.frame(from = c('Asistente Virtual', 'Puntos Colombia'), message = c('Invierte tu dinero aqui!', 'Aumentar de segmento te da mas puntos colombia!'))
        
    output$messageMenu <- renderMenu({
        # Code to generate each of the messageItems here, in a list. This assumes
        # that messageData is a data frame with two columns, 'from' and 'message'.
        msgs <- apply(messageData, 1, function(row) {
            messageItem(from = row[["from"]], message = row[["message"]])
        })
        
        # This is equivalent to calling:
        #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
        dropdownMenu(type = "messages", .list = msgs)
    })
    
    # Notifications
    
    notifications <- data.frame(text=c('Gastos muy altos', 'Tu credito ha sido aprobado!'), 
                                icon=c('usd', 'ok'), 
                                status=c('danger','success'))
    
    output$notificationsMenu <- renderMenu({
        
       nots <- apply(notifications, 1, function(row) {
            notificationItem(text = row[['text']], icon = icon(row[['icon']], lib = "glyphicon"), status=row[['status']])
        })
        
        dropdownMenu(type = "notification", .list = nots)
    })
    
    # Tasks
    
    el <- data.frame(value = c(95, 20), text = c('Ahorrando para tu moto!', 'Paga tu hipoteca'))
    el$color <- lapply(el$value, function(x){ifelse(x<30,"red",ifelse(x<80,"yellow","green"))})
    
    output$tasksMenu <- renderMenu({
        items <- apply(el, 1, function(el) {
            taskItem(value = el[['value']], color = el[['color']], text = el[['text']])
        })
        dropdownMenu(type = "tasks", .list = items)
    })
    
    # End of page image
    output$plumbers <- renderImage({
            list(
                src = "plumbers.jpg",
                width = 100,
                height = 75,
                align = 'center',
                contentType = "image/jpeg",
                alt = "Plumbers"
            )
    }, deleteFile = FALSE)
}

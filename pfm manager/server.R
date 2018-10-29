# Instalamos librerias
library(scales)
library(plotly)
library(ggplot2)
library(plotrix)
library(knitr)
library(plotly)

# Cargamos datos

fread('https://bancoblob.blob.core.windows.net/data/dt_info_pagadores_descriptivo.csv')

path <- "data/consolidado.csv"

df <- fread(path,sep=",", header = T, stringsAsFactors = F,
            encoding="UTF-8")

colnames(df) <- c("id_trans","id_cliente","fecha","hora",
                  "valor_trx","ref1","ref2","ref3",
                  "sector","subsector","descripcion")

## Transformacioin columna fechas
df$fecha <- as.Date(as.character(df$fecha), "%Y%m%d")

server <- function(input, output, session) {
    
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
                                      tags$abbr(title=Sys.time(), "Virtual Assistant")
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
                                                            tags$abbr(title=Sys.time(), "Virtual Assistant")
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
    
    messageData <- data.frame(from = c('Virtual Advisor'), message = c('Invest your money here!'))
        
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
    
    notifications <- data.frame(text=c('Expenses too high', 'Your credit has been aproved!'), 
                                icon=c('usd', 'ok'), 
                                status=c('danger','success'))
    
    output$notificationsMenu <- renderMenu({
        
       nots <- apply(notifications, 1, function(row) {
            notificationItem(text = row[['text']], icon = icon(row[['icon']], lib = "glyphicon"), status=row[['status']])
        })
        
        dropdownMenu(type = "notification", .list = nots)
    })
    
    # Tasks
    
    el <- data.frame(value = c(95, 20), text = c('Saving for your motorcycle', 'Pay your mortage loan'))
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

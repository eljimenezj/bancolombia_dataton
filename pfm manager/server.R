server <- function(input, output, session) {
    set.seed(122)
    histdata <- rnorm(500)
    #print(getwd())
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    
    
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
                                                  input$entry))
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

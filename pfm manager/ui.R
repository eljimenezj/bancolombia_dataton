library(shinydashboard)
library(data.table)
library(plotly)

ui <- dashboardPage(
    dashboardHeader(title = "PFM Manager",
                    dropdownMenuOutput("messageMenu"),
                    dropdownMenuOutput("notificationsMenu"),
                    dropdownMenuOutput("tasksMenu")
                    ),
    dashboardSidebar(
        sidebarMenu(
            HTML('<center><img src="index.png" width="200" height="120"></center>'),
            textInput("id", label = h4("Enter your ID:"), value = "12"),
            br(),
    
            menuItem("Control your expenses", tabName = "expenses", icon = icon("briefcase", lib = "glyphicon")),
            menuItem("Saving Capacity", tabName = "saving", icon = icon("piggy-bank", lib = "glyphicon")),
            menuItem("Want a credit?", tabName = "credit", icon = icon("usd",lib = "glyphicon")),
            menuItem("Invest your money!", tabName = "invest", icon = icon("btc", lib = "glyphicon")),
            br(), br(),  
            h5("App developed by The Plumbers "), HTML('<center><img src="plumbers.png" width="240" height="180"></center>'), h5(" for the Bancolombia's Dataton"),
            h5("competition.")
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "expenses",
                    fluidRow(
                        box(
                            h2('Como son sus gastos?'),
                            plotlyOutput("plot1", height = 250),
                            selectInput("period", h5("Selecciona el periodo de tiempo"), 
                                         choices = list("Mes" = 1, "Triemestre" = 2,
                                                        "Ano" = 3), selected = 1)),
                        box(h2('Visualice sus gastos'),
                            plotlyOutput("plot2", height = 250)
                        )),
                        box(
                            h2('En que se van los gastos?'),
                            plotlyOutput("plot3", height = 250),
                            selectInput("ref", h5("Selecciona la referencia"), 
                                        choices = list("Ref 1" = 1, "Ref 2" = 2,
                                                       "Ref 3" = 3), selected = 1)
                        ),
                        box(
                            h2('En donde se concentran los gastos?'),
                            plotlyOutput("plot4", height = 250),
                            selectInput("sector", h5("Selecciona el nivel sectorial"), 
                                        choices = list("Sector" = 1, "Subsector" = 2,
                                                       "Descripcion" = 3), selected = 1)
                        )
                    
            ),
            
            # Second tab content
            tabItem(tabName = "saving",
                    h2("Saving tab content")
            ),
            
            # Third tab content
            tabItem(tabName = "credit",
                    h2("Credit tab content"),
                    actionButton("link", "Aprovecha tu credito", onclick ="window.open('https://www.grupobancolombia.com/wps/portal/personas')"),
                    downloadButton("downloadCert", "Bajar certificado")
            ),
            
            # Fourth tab content
            tabItem(tabName = "invest",
                    column(12, align="center",
                    h2("Habla con el asesor!"),
                    # Create a spot for a dynamic UI containing the chat contents.
                    box(uiOutput("chat")),
                    box(h3('Lo que me puedes preguntar:'),
                        h5('En que puedo invertir?'),
                        h5('Cual es la tasa de rendimiento de mi inversion?'),
                        h5('Cuanto serian los intereses ganados?'),
                        h5('Cual es el tiempo de retorno de la inversion?')),
                    # Create the bottom bar to allow users to chat.
                    fluidRow(
                        div(textInput("entry", "")
                        ),
                        div(actionButton("send", "Send")
                        )
                    )
                )
            )
        )
    )
)
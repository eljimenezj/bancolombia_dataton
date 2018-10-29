library(shinydashboard)
library(data.table)
library(plotly)

ui <- dashboardPage(
    dashboardHeader(title = "PF Manager",
                    dropdownMenuOutput("messageMenu"),
                    dropdownMenuOutput("notificationsMenu"),
                    dropdownMenuOutput("tasksMenu")
                    ),
    dashboardSidebar(
        sidebarMenu(
            HTML('<center><img src="index.png" width="200" height="120"></center>'),
            textInput("id", label = h4("Ingresa tu ID:"), value = "12"),
            br(),
    
            menuItem("Controla tus gastos", tabName = "expenses", icon = icon("briefcase", lib = "glyphicon")),
            menuItem("Capacidad de ahorro", tabName = "saving", icon = icon("piggy-bank", lib = "glyphicon")),
            menuItem("Quieres un credito?", tabName = "credit", icon = icon("usd",lib = "glyphicon")),
            menuItem("Invierte tu dinero!", tabName = "invest", icon = icon("btc", lib = "glyphicon")),
            br(), br(),  
            h5("App desarrollada por The Plumbers "), HTML('<center><img src="plumbers.png" width="240" height="180"></center>'), h5(" para la competencia Dataton"),
            h5("Bancolombia 2018")
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "expenses",
                    fluidRow(
                        h2('Este es tu perfil!'),
                        fluidRow(
                            valueBoxOutput("segment"),
                            valueBoxOutput("monto"),
                            valueBoxOutput("freq"),
                            valueBoxOutput("recen"),
                            valueBoxOutput("consumo"),
                            valueBoxOutput("points")
                        ),
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
                    h2('Estos son tus estimados de ahorro!'),
                    fluidRow(
                        valueBoxOutput("month1"),
                        valueBoxOutput("month2"),
                        valueBoxOutput("month3"),
                        valueBoxOutput("income")
                    ),
                    #
                    div(style="display: inline-block;vertical-align:top; width: 250px;", numericInput("meta", h4("Ingresa tu meta de ahorro"), value = 500000)),
                    div(style="display: inline-block;vertical-align:top; width: 500px;", numericInput("saving_rate", h4("Ingresa tu % de ahorro deseada"), value = 20, min=5, max=50)),
                    h2('Observa tus ingresos y egresos'),
                    plotlyOutput("plotcost", height = 250)
                    
            ),
            
            # Third tab content
            tabItem(tabName = "credit",
                    conditionalPanel( condition = "output.credit_lend == 1",
                                      h2("Enhorabuena! Tu credito esta aprobado! :)"),
                                      h4('Debido a tu buen comportamiento y perfil con el banco, hemos decidido darte un credito a tasa preferencial.'),
                                      valueBoxOutput("rate"),
                                      valueBoxOutput("amount"),
                                      valueBoxOutput("periods"),
                                      h4('Puedes reclamar tu credito dando clic al boton "Aprovecha tu credito" o presentando el certificado en tu sucursal mas cercana.'),
                                      actionButton("link", "Aprovecha tu credito", onclick ="window.open('https://www.grupobancolombia.com/wps/portal/personas')"),
                                      downloadButton("downloadCert", "Descargar certificado")
                                      ),
                    conditionalPanel( condition = "output.credit_lend == 0",
                                      h2("Lo sentimos! :("),
                                      h4('En estos momentos no podemos otorgarte un credito rapido.'),
                                      HTML('<center><img src="plumber-broken.png"></center>'),
                                      h4('.')
                                      )
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
                        h5('Cuanto es el monto minimo de inversion para mi fondo?')),
                    # Create the bottom bar to allow users to chat.
                    fluidRow(
                        div(textInput("entry", "")
                        ),
                        div(actionButton("send", "Send")
                        )
                    )
                ),
                h2('Cuentanos tu experiencia con la app!'),
                h4('No lo pienses dos veces! Puedes escribirnos tus inquietudes, problemas o agradecimientos y podras ganar Puntos Colombia! '), a("soporte@theplumbers.com",href=""),
                h4('Para comunicarte con nosotros tambien puedes ingresar al siguiente link!'), a("PQRS Bancolombia",href="https://www.grupobancolombia.com/wps/portal/personas/contactanos/bancolombia")
            )
        )
    )
)
library(shinydashboard)
library(data.table)

ui <- dashboardPage(
    dashboardHeader(title = "PFM Manager",
                    dropdownMenuOutput("messageMenu"),
                    dropdownMenuOutput("notificationsMenu"),
                    dropdownMenuOutput("tasksMenu")
                    ),
    dashboardSidebar(
        sidebarMenu(
            HTML('<center><img src="index.png" width="200" height="120"></center>'),
            textInput("id", label = h4("Enter your ID:"), value = "Ex: 152436789"),
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
                        box(plotOutput("plot1", height = 250)),
                        
                        box(
                            title = "Controls",
                            sliderInput("slider", "Number of observations:", 1, 100, 50)
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "saving",
                    h2("Saving tab content")
            ),
            
            # Third tab content
            tabItem(tabName = "credit",
                    h2("Credit tab content")
            ),
            
            # Fourth tab content
            tabItem(tabName = "invest",
                    h2("Invest tab content")
            )
        )
    )
)
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
            menuItem("Control your expenses", tabName = "expenses", icon = icon("dashboard")),
            menuItem("Saving Capacity", tabName = "saving", icon = icon("th")),
            menuItem("Want a credit? Sure", tabName = "credit", icon = icon("th")),
            menuItem("Invest your money!", tabName = "invest", icon = icon("th"))
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
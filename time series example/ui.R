library(shiny)
library(shinyjs)
library(dygraphs)
library(magrittr)
library(xts)
library(forecast)
library(xtable)

navbarPage("Pronostico de Series Tiempo",
           tabPanel("Lectura de datos",
                    sidebarLayout(
                      sidebarPanel(
                        fileInput('datos', 'Escoja archivo CSV',
                                  accept=c('text/csv', 
                                           'text/comma-separated-values,text/plain', 
                                           '.csv')),
                        tags$hr(),
                        
                        
                        checkboxInput('header', 'Encabezado', TRUE),
                        
                        radioButtons('sep', 'Separador',
                                     c("Punto y Coma" =';',
                                       "Coma"=',',
                                       "Tabulador" ='\t'),
                                     ';'),
                        
                        radioButtons('quote', 'Entrecomillar',
                                     c(None='',
                                       'Doble Comilla'='"',
                                       'Comilla Simple'="'"),
                                     ''),
                        
                        radioButtons('decimal', 'Decimal',
                                     c('Punto'='.',
                                       'Coma'=","),
                                     ',')
                      )
                      ,
                      
                      mainPanel(  
                        
                        h2("Analisis inicial de datos",align="center"),
                        h3("Grafica de la serie de tiempo"),
                        dygraphOutput('plot'),
                        h3("Datos de la Serie de Tiempo",align="center"),
                        h4("Numero total de observaciones"),
                        textOutput('n'),
                        h4("Datos iniciales de la serie"),
                        tableOutput('contenido'),
                        h3("Descripcion Estadistica de la Serie de Tiempo",align="center"),
                        tableOutput('descripcion'),
                        h3("Histograma de la serie",align="center"),
                        plotOutput('hist'))
                    )
           ),
           
           tabPanel("Modelo Polinomico",
                    sidebarLayout(
                      sidebarPanel(
                        
                        # Frecuencia de los datos ts
                        radioButtons('frec', 'Frecuencia de la Serie',
                                     c('Semestral'=2,
                                       'Trimestral'=4,
                                       'Bimestral'=6,
                                       'Mensual'=12),
                                     'Trimestral'),
                        
                        # Validacion cruzada
                        sliderInput("val", "Porcentaje Validacion:", 
                                    min=5, max=50, value=20),
                        
                        selectInput("grado", "Estimacion Tendencia:", 
                                    choices = c("No Aplica","Lineal","Cuadratico","Cubico")),
                        
                        selectInput("estacional", "Estimacion Estacionalidad:", 
                                    choices = c("No Aplica","Variables Indicadoras","Trigonometricas")),
                        
                        selectInput("interval", "Tipo de Intervalo:", 
                                    choices = c("Confianza","Prediccion")),
                        
                        sliderInput("confidence", "Nivel de Confianza:", 
                                    min=50, max=99, value=95),
                        
                        numericInput('n_pron', 'Numero de Pronosticos', 4, min = 1, max = 99999999, step = 1),
                        
                        downloadButton('downloadData', 'Descarga Pronosticos')
                        
                      ),
                      
                      mainPanel(h2("Modelo Polinomico",align="center"),
                                h3("Grafica"),
                                plotOutput('grafica'),
                                h3("Coeficientes Ajustados",align="center"),
                                verbatimTextOutput('coef'),
                                h3("Medidas de Desempeno",align="center"),
                                h4("Ajuste"),
                                tableOutput('medfit'),
                                h4("Pronostico"),
                                tableOutput('medfor'),
                                h3("Diagnostico",align="center"),
                                plotOutput('diagnostico'))
                    )
           ),
           
           
           tabPanel("Suavizado Holt-Winters",
                    sidebarLayout(
                      sidebarPanel(
                        
                        sliderInput("val_suav", "Porcentaje Validacion:", 
                                    min=5, max=50, value=20),
                        selectInput("method","Metodo:",
                                    choices = c("No Aplica","Automatico","Aditivo","Multiplicativo")),
                        sliderInput("alpha","Alpha (Parametro Nivel):",
                                    min = 0.05, max = 1, step= 0.05, value = 0.5),
                        sliderInput("beta","Beta (Parametro Tendencia):",
                                    min = 0.05, max = 1, step= 0.05, value = 0.5),
                        sliderInput("gamma","Gamma (Parametro Estacionalidad):",
                                    min = 0.05, max = 1, step= 0.05, value = 0.5),
                        
                        sliderInput("confidence_suav", "Nivel de Confianza:", 
                                    min=50, max=99, value=95),
                        
                        radioButtons('frequency_suav', 'Frecuencia de la Serie',
                                     c(Semestral='2',
                                       Trimestral='4',
                                       Bimestral='6',
                                       Mensual='12'),
                                     '12'),
                        
                        numericInput('n_pron_suav', 'Numero de Pronosticos', 4, min = 1, max = 99999999, step = 1),
                        
                        downloadButton('downloadData_suav', 'Descarga Pronosticos')
                      ),
                      
                      mainPanel(h2("Modelo Holt-Winters",align="center"),
                                h3("Grafica"),
                                plotOutput('grafica_holt'),
                                h3("Coeficientes Ajustados",align="center"),
                                verbatimTextOutput('coef_suav'),
                                h3("Medidas de Desempeno",align="center"),
                                h4("Ajuste"),
                                tableOutput('medfit_suav'),
                                h4("Pronostico"),
                                tableOutput('medfor_suav'),
                                h3("Diagnostico",align="center"),
                                plotOutput('diagnostico_holt'))
                    )
           ),
           
           tabPanel("Modelo SARIMA",
                    sidebarLayout(
                      sidebarPanel(
                        
                        sliderInput("val_auto", "Porcentaje Validacion:", 
                                    min=5, max=50, value=20),
                        sliderInput("ar","Rezago Autoregresivo:",
                                    min = 0, max = 5, step= 1, value = 2),
                        sliderInput("ma","Rezago Media Movil:",
                                    min = 0, max = 5, step= 1, value = 2),
                        sliderInput("d","Integracion Ordinaria:",
                                    min = 0, max = 3, step= 1, value = 1),
                        sliderInput("ar_s","Rezago Autoregresivo Estacional:",
                                    min = 0, max = 5, step= 1, value = 2),
                        sliderInput("ma_s","Rezago Media Movil Estacional:",
                                    min = 0, max = 5, step= 1, value = 2),
                        sliderInput("d_s","Integracion Estacional:",
                                    min = 0, max = 3, step= 1, value = 1),
                        
                        sliderInput("confidence_auto", "Nivel de Confianza:", 
                                    min=50, max=99, value=95),
                        
                        radioButtons('frequency_auto', 'Frecuencia de la Serie',
                                     c(Anual='1',
                                       Semestral='2',
                                       Trimestral='4',
                                       Bimestral='6',
                                       Mensual='12',
                                       Semanal='52',
                                       Diaria='360',
                                       "Dias Habiles"='252'),
                                     '12'),
                        
                        numericInput('n_pron_auto', 'Numero de Pronosticos', 4, min = 1, max = 99999999, step = 1),
                        
                        downloadButton('downloadData_auto', 'Descarga Pronosticos')
                        
                      ),
                      
                      mainPanel(h2("Modelo SARIMA",align="center"),
                                h3("Grafica"),
                                plotOutput('grafica_auto'),
                                h3("Coeficientes Ajustados",align="center"),
                                verbatimTextOutput('coef_auto'),
                                h3("Medidas de Desempeno",align="center"),
                                h4("Ajuste"),
                                tableOutput('medfit_auto'),
                                h4("Pronostico"),
                                tableOutput('medfor_auto'),
                                h3("Diagnostico",align="center"),
                                plotOutput('diagnostico_auto'))
                      
                      
                      )
                    )
           )
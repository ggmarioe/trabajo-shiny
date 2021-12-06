#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(jsonlite))  install.packages("jsonlite") 
if(!require(DT))  install.packages("DT") 
if(!require(shinythemes))  install.packages("shinythemes") 
if(!require(shiny))  install.packages("shiny") 
if(!require(shinydashboard))  install.packages("shinydashboard") 

library(DT)
library(shiny)
library(tidyverse)
library(jsonlite)
library(shiny)
library(shinythemes)
library(shinydashboard)

lista_empresas <- c("NUEVAPOLAR", "SMU", "BESALCO", "COPEC", "FALABELLA", 
                    "BSANTANDER",  "CMPC", "CHILE", "SQM-B", "ENELAM", "CENCOSUD",
                    "BCI", "LTM",  "ENELCHILE", "SM-CHILE B", "CCU", "PARAUCO",
                    "ITAUCORP", "AGUAS-A",  "COLBUN", "ENTEL", "ECL", "CONCHATORO",
                    "RIPLEY", "AESGENER",  "ANDINA-B", "SONDA", "CAP", "ILC", 
                    "SALFACORP", "SECURITY", "VAPORES",  "ENELGXCH", "ANTARCHILE",
                    "BANMEDICA", "EMBONOR-B", "FORUS",  "IAM", "MASISA", "ORO BLANCO", 
                    "SK", "SMSAAM")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Application title
    dashboardHeader(),

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
        #sidebarPanel(
           selectInput("empresa", "Empresa", lista_empresas),
           sliderInput("periodo", "Periodo", min = 1, max = 5, value = 2)
    ),

    # Show a plot of the generated distribution
    dashboardBody(
        fluidRow(
            plotOutput("distPlot"),
            DT::dataTableOutput("tablaIndicadores")    
        )
    )
)


obtener_indicadores <- function(empresa) { 
    
    url <- stringr::str_c("https://www.elmercurio.com/inversiones/json/json.aspx?categoria=", 
                          empresa, "&time=10&indicador=2") 
    
    df <- jsonlite::read_json(url)$Data %>% 
        stringr::str_split(";") %>% 
        dplyr::first() %>%
        I() %>% 
        readr::read_delim(delim = ",", col_names = c("fecha", "precio", "vol")) 
    
    df <- df %>% 
        mutate(
            fecha = lubridate::ymd_hms(fecha),
            anio = lubridate::year(fecha)
        ) 
    df 
    
} 

 
# Define server logic required to draw a histogram
server <- function(input, output) {
    #
    # indicadores <- obtener_indicadores(x)  
    #datatable(indicadores)
    
    output$distPlot <- renderPlot({
        set.seed(2022)
        x <- input$empresa
        indicadores <<- obtener_indicadores(x)
        
        plot(indicadores$precio, type = "b")
    })
    output$tablaIndicadores <- DT::renderDataTable({
        indicadores %>% 
            group_by(anio) %>% 
            summarise(sum = sum(precio))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

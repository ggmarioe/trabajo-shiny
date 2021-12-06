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

library(shiny)
library(tidyverse)
library(jsonlite)


lista_empresas <- c("NUEVAPOLAR", "SMU", "BESALCO", "COPEC", "FALABELLA", 
                    "BSANTANDER",  "CMPC", "CHILE", "SQM-B", "ENELAM", "CENCOSUD",
                    "BCI", "LTM",  "ENELCHILE", "SM-CHILE B", "CCU", "PARAUCO",
                    "ITAUCORP", "AGUAS-A",  "COLBUN", "ENTEL", "ECL", "CONCHATORO",
                    "RIPLEY", "AESGENER",  "ANDINA-B", "SONDA", "CAP", "ILC", 
                    "SALFACORP", "SECURITY", "VAPORES",  "ENELGXCH", "ANTARCHILE",
                    "BANMEDICA", "EMBONOR-B", "FORUS",  "IAM", "MASISA", "ORO BLANCO", 
                    "SK", "SMSAAM")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           selectInput("empresa", "Empresa", lista_empresas),
           sliderInput("periodo", "Periodo", min = 1, max = 5, value = 2)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
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
    
    #x <- input$slider1
    #d <- obtener_indicadores(input$empresa)    
    output$distPlot <- renderPlot({
        set.seed(2022)
        x <- input$empresa
        data <- obtener_indicadores(x)
        print(data)
        plot(data$precio, type = "b")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

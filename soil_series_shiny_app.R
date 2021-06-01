library(shiny)
library(tidyverse)
library(soilDB)
library(sharpshootR)
library(sf)
library(sp)
library(here)
library(aqp)
library(rgeos)

## Placeholder for functions for the moment
get_point_data<-function(plotID){
  point_xy<-dplyr::filter(points, PLOT_NM==plotID)%>%
    as("Spatial")
  
  x <- SDA_spatialQuery(geom = point_xy, what = 'mukey', db = 'SSURGO')
  
  qq <- sprintf(
    "SELECT mukey, cokey, compname, comppct_r FROM component WHERE mukey IN %s ;", 
    format_SQL_in_statement(x$mukey)
  )
  
  s <- SDA_query(qq)
  
  osds <- fetchOSD(unique(s$compname))
  
  osds
}

points<-st_read(here("data/CO_TresRiosFO_2018_2022_5yrDesign.shp"))

points$PLOT_NM

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Get Soils Information For Your Plot"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "plot",
        label = "Select a Plot:",
        points$PLOT_NM,
        selected = NULL,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("soilsDendron"),
      htmlOutput("soilsDescription")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  osds_data<-reactive({
    get_point_data(input$plot)
  })
  
  output$soilsDendron <- renderPlot({
    par(mar = c(0, 0, 0, 0))
    
    plotSPC(osds_data(), plot.depth.axis = FALSE, name.style = 'center-center', width = 0.3, hz.depths = TRUE)
    
    SoilTaxonomyDendrogram(osds_data(), plot.depth.axis = FALSE, name.style = 'center-center', width = 0.3, hz.depths = TRUE, y.offset = 0.5)

  })
  output$soilsDescription<-renderUI({
    x<-osds_data()
    
    all_series_descriptions<-lapply(1:length(x@site$id), function(i){
      series<-dplyr::filter(x@horizons, id==x@site$id[i])
      narrative<-lapply(1:nrow(series), function(i){
        tags$p(x@horizons$narrative[i])
      })
      c(
        list(
          tags$h2(x@site$id[i])
        ),
        narrative
      )
    })
    
    all_series_descriptions
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

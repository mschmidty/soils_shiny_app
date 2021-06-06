library(shiny)
library(tidyverse)
library(soilDB)
library(sharpshootR)
library(sf)
library(sp)
##library(here)
library(aqp)
library(rgeos)
library(rgdal)

## Placeholder for functions for the moment
get_point_data<-function(plotID, buffer_distance){
  point_xy<-dplyr::filter(points, PLOT_NM==plotID)
  
  if(buffer_distance != 0){
    point_xy<-point_xy%>%
      st_transform(2163)%>%
      st_buffer(buffer_distance)
  }
  point_xy<-point_xy%>%
    as("Spatial")
  
  x <- SDA_spatialQuery(geom = point_xy, what = 'mukey', db = 'SSURGO')
  
  qq <- sprintf(
    "SELECT mukey, cokey, compname, comppct_r FROM component WHERE mukey IN %s ;", 
    format_SQL_in_statement(x$mukey)
  )
  
  s <- SDA_query(qq)
  
  osds <- fetchOSD(unique(s$compname))
  
  qq2<-sprintf(
    "SELECT * FROM coecoclass WHERE cokey IN %s ;",
    format_SQL_in_statement(s$cokey)
  )

  s2<-SDA_query(qq2)
  
  if(is.null(s2)){
    eco_data<-NA
  }else{
    eco_data<-s%>%
      left_join(s2,  by="cokey")%>%
      as_tibble()
  }
  final_data<-list()
  final_data[['osds']]<-osds
  final_data[['eco']]<-eco_data
  final_data[['mukey']]<-x%>%
    left_join(s, by="mukey")
  final_data
}

points<-st_read("data/CO_TresRiosFO_2018_2022_5yrDesign.shp")

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
      ),
      sliderInput("buffer", "Buffer Point (meters:",
                  min = 0, max = 2000,
                  value = 800)
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
    get_point_data(input$plot, input$buffer)
  })
  
  output$soilsDendron <- renderPlot({
    osds<-osds_data()$osds
    
    par(mar = c(0, 0, 0, 0))

    plotSPC(osds, plot.depth.axis = FALSE, name.style = 'center-center', width = 0.3, hz.depths = TRUE)

    SoilTaxonomyDendrogram(osds, plot.depth.axis = FALSE, name.style = 'center-center', width = 0.3, hz.depths = TRUE, y.offset = 0.5)

  })
  output$soilsDescription<-renderUI({
    osds<-osds_data()$osds
    eco<-osds_data()$eco
    mukey_tbl<-osds_data()$mukey
    no_data_esd<-c("No Ecological Site information available for this series.")

    all_series_descriptions<-lapply(1:length(unique(mukey_tbl$mukey)),function(k){
      
      current_mukey<-unique(mukey_tbl$mukey)[k]
      mapunit_name<-unique(mukey_tbl$muname)[k]
      
      filtered_mukey_table<-mukey_tbl%>%
        dplyr::filter(mukey==current_mukey)
      
      mapunit_data<-lapply(1:nrow(filtered_mukey_table), function(i){
        
        series_name<-filtered_mukey_table$compname[i]
        series_details<-dplyr::filter(osds@horizons, str_detect(id, regex(series_name, ignore_case=T)))
        
        filtered_eco<-dplyr::filter(eco, mukey==current_mukey & compname==series_name)
        
        if(is.na(filtered_eco)){
          esd_name<-no_data_esd
        }else{
          
          esd_data<-filtered_eco%>%
            dplyr::filter(str_detect(compname, regex(series_name, ignore_case=T)))%>%
            distinct(ecoclassid, .keep_all=T)
          
          if(nrow(esd_data)==0 | is.na(esd_data$ecoclassname)){
            
            esd_name<-no_data_esd
            
          }else{
            
            esd_name<-esd_data$ecoclassname
            
          }
          
        }
        
        narrative<-lapply(1:nrow(series_details), function(j){
          
          if(nrow(series_details)<1){
            
            return(tags$p("There is no horizon information for this series."))
            
          }else{
            
            tags$p(series_details$narrative[j])
            
          }
        })
        
        c(list(tags$h3(paste0(series_name, " (", filtered_mukey_table$comppct_r[i],"%)")), tags$h4(paste0("ESD: ",esd_name))), narrative)
        
      })
      
      c(list(tags$h2(mapunit_name)), mapunit_data)
      
      
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(leaflet)


ui <- fluidPage(
  
  titlePanel("Taiwan Dengue Historical Map"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons(inputId = "region", label = "Infected Region",
                   choices = c("Republic of China" = "1",
                     "Other Countries" = "0")
                   ),
      br(),
      
      sliderInput("n",
                  "Year:",
                  value = 2015,
                  min = 2013,
                  max = 2017)
      
    ),
    
    
    tabsetPanel(type = "tabs",
                tabPanel("Plot", leafletOutput(outputId = "plot")))
    )
    
  )
)

server <- function(input, output){
  current_region <- reactibve({
    filter(dengue, XXX==input$region)
  })
  current_year <- fuction(input, output){
    filter(current_region(), year==input$n)
  }
  
  output$plot <- renderLeaflet({
    leaflet(current_year()) %>% 
      addTiles() %>% 
      addCircleMarkers(~current_year()$lon, ~current_year()$lat, radius = 1, fillOpacity = 0.5)
  })
  
}

shinyApp(ui = ui, server = server)
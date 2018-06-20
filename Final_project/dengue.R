
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderTextInput(inputId = "timefordengue",
                      label = "時間點",
                      choices = unique(dengue$yearmonth),
                      selected = unique(dengue$yearmonth)[1],
                      animate = TRUE)
    ),
    mainPanel(
      leafletOutput(outputId = "dengueplot")
    )
    )
  )

  

server <- function(input,output){
  current_time_dengue <- reactive({
    filter(dengue, yearmonth==input$timefordengue)
  })
  output$dengueplot <- renderLeaflet({
    leaflet(current_time_dengue()) %>% 
      addTiles() %>% 
      addCircleMarkers(~current_time_dengue()$Enumeration_unit_long, ~current_time_dengue()$Enumeration_unit_lat, radius = 1, fillOpacity = 0.5) %>% 
      setView(lng = 120.41, lat = 23.54, zoom = 7)
  })
}

shinyApp(ui=ui,server=server)
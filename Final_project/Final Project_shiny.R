library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(ggmap)
library(DT)
library(leaflet)

#匯入資料
index <- readRDS("index.rds")
weather <- readRDS("weather.rds")
dengue <- readRDS("dengue.rds")
centerlon <- (max(index$VillageLon, na.rm = TRUE)+min(index$VillageLon, na.rm = TRUE))/2
centerlat <- (max(index$VillageLat, na.rm = TRUE)+min(index$VillageLat, na.rm = TRUE))/2
map <- get_map(location = c(lon = centerlon, lat = centerlat), language = "zh-TW")
selectdate <- function(x){substr(as.character(x), start = 6, stop = 10)}

#=============================================================================
ui = fluidPage(
  headerPanel("台南市登革熱疫情趨勢分析", windowTitle = "台南市登革熱疫情趨勢分析"),
###############################################################
    fluidRow(column(4,
                  "選取日期放這裡",
                  style='margin-bottom:30px;border:1px solid; padding: 10px;'),
           column(8,
                  "輸出圖表放這裡")),
###############################################################
  fluidRow(column(4,
                  h3("登革熱分布地區"),
                  sliderTextInput(inputId = "timefordengue",
                                  label = "時間點",
                                  choices = unique(dengue$yearmonth),
                                  selected = unique(dengue$yearmonth)[1],
                                  animate = TRUE),
                  style='margin-bottom:30px;border:1px solid; padding: 10px;'),
           column(8,
                  leafletOutput(outputId = "dengueplot"))
           
  ),br(),
  
  fluidRow(column(4,
                  h3("台南市每日溫度"),
                  dateRangeInput(inputId = "daterangefortemp",
                                 label = "時間區間",
                                 start = weather$date[1],
                                 end = tail(weather$date, n = 1),
                                 min = weather$date[1],
                                 max = tail(weather$date, n = 1),
                                 startview = "year"),
                  style='margin-bottom:30px;border:1px solid; padding: 10px;'),
           column(8,
                  tabsetPanel(
                    tabPanel("Plot", plotOutput(outputId = "tempplotdate")),
                    tabPanel("Data", dataTableOutput(outputId = "tempdatadate"))
                  ))
  ),br(),
  
  fluidRow(
    column(4,
           h3("台南市月均溫"),
           sliderTextInput(inputId = "yearfortemp",
                           label = "年份",
                           choices = unique(weather$year),
                           selected = unique(weather$year)[1],
                           animate = TRUE),
           style='margin-bottom:30px;border:1px solid; padding: 10px;'),
    column(8,
           tabsetPanel(
             tabPanel("Plot",plotOutput(outputId = "tempplotmonth")),
             tabPanel("Data",dataTableOutput(outputId = "tempdatamonth"))
           )
    )
  ),br(),
  
  fluidRow(
    column(4,
           h3("台南市降雨量"),
           sliderTextInput(inputId = "yearforprecp",
                           label = "年份",
                           choices = unique(weather$year),
                           selected = unique(weather$year)[1],
                           animate = TRUE),
           style='margin-bottom:30px;border:1px solid; padding: 10px;'),
    column(8,
           plotOutput(outputId = "precpplot"),
           uiOutput(outputId = "precpsummary"))
  ),br(),
  
  fluidRow(
    column(4,
           h3("台南市每日濕度"),
           dateRangeInput(inputId = "daterangeforhd",
                          label = "時間區間",
                          start = weather$date[1],
                          end = tail(weather$date, n = 1),
                          min = weather$date[1],
                          max = tail(weather$date, n = 1),
                          startview = "year"),
           style='margin-bottom:30px;border:1px solid; padding: 10px;'),
    column(8,
           tabsetPanel(
             tabPanel("Plot",plotOutput(outputId = "hdplot")),
             tabPanel("Data",dataTableOutput(outputId = "hddata"))
           ))
  ),br(),
  
  fluidRow(
    column(4,
           h3("台南市病媒蚊指數"),
           sliderTextInput(inputId = "yearforindex",
                           label = "年份",
                           choices = c(2013:2017),
                           selected = 2013),
           
           sliderTextInput(inputId = "dateforindex",
                           label = "日期",
                           choices = unique(selectdate(index$Date)),
                           selected = unique(selectdate(index$Date))[1],
                           animate = animationOptions(interval = 3000)
                           ),
           style='margin-bottom:30px;border:1px solid; padding: 10px;'
           ),
    column(8,
           tabsetPanel(
             tabPanel("Plot",plotOutput(outputId = "indexBI"),plotOutput(outputId = "indexHI")),
             tabPanel("Data",dataTableOutput(outputId = "indexdata"))
             )
           )
    )
  )



                      
#reactive data========================================================================
server <- function(input, output){
  current_time_dengue <- reactive({
    filter(dengue, yearmonth==input$timefordengue)
  })
  current_range_temp <- reactive({
    filter(weather, weather$date >= input$daterangefortemp[1] & 
             weather$date <= input$daterangefortemp[2])
  })
  current_year_temp <- reactive({
    filter(weather, weather$year == input$yearfortemp)
  })
  current_year_precp <- reactive({
    filter(weather, weather$year == input$yearforprecp)
  })
  current_range_hd <- reactive({
    filter(weather, weather$date >= input$daterangeforhd[1] & 
             weather$date <= input$daterangeforhd[2])
  })
  current_date_index <- reactive({
    filter(index, index$Date == as.Date(paste0(input$yearforindex,"-",input$dateforindex)))
  })
  
#Plot======================================================================

    output$dengueplot <- renderLeaflet({
    leaflet(current_time_dengue()) %>% 
      addTiles() %>% 
      addCircleMarkers(~current_time_dengue()$Enumeration_unit_long, ~current_time_dengue()$Enumeration_unit_lat, radius = 1, fillOpacity = 0.5) %>% 
      setView(lng = 120.41, lat = 23.54, zoom = 7)
  })
  output$tempplotdate <- renderPlot({
    ggplot(data = current_range_temp(), aes(x = date, y = temp))+
      geom_line()+
      scale_x_date(name = "Date", date_breaks = "1 years", date_minor_breaks = "1 months")+
      scale_y_continuous(limits = c(0,35),breaks = c(10,20,30))+
      ylab("Temperature(℃)")+
      ggtitle("台南市每日溫度")
  })
  output$tempdatadate <- DT::renderDataTable({
    DT::datatable(data = select(current_range_temp(),c("date","temp")),
                  colnames = c("Date","Tempeture"),
                  rownames = FALSE)
  })
  
  output$tempplotmonth <- renderPlot({
    ggplot(data = group_by(current_year_temp(), month) %>% summarise(avgtemp = mean(temp)),
           aes(x = month, y=avgtemp))+
      geom_bar(stat = "identity")+
      xlab("Month")+
      ylab("Temperature(℃)")+
      scale_y_continuous(limits = c(0,35), breaks = c(10,20,30))+
      ggtitle(label = paste0("台南市",input$yearfortemp,"年－月均溫"))
  })
  output$tempdatamonth <- renderDataTable({
    datatable(data = current_year_temp() %>% group_by(month) %>% summarise(averageTemp = round(mean(temp), digit = 2)),
    colnames = c("Month","Tempature(℃)"),
    rownames = FALSE,
    options = list(pageLength = 12))
    })
  
  output$precpplot <- renderPlot({
    ggplot(data = group_by(current_year_precp(), month) %>% summarise(sumprecp = sum(precp)),
           aes(x = month, y=sumprecp))+
      geom_bar(stat = "identity")+
      xlab("Month")+
      ylab("Rainfull(mm)")+
      ylim(0, 1250)+
      ggtitle(label = paste0("台南市",input$yearforprecp,"年－月降雨量"))
  })
  
  output$precpsummary <- renderUI({
    HTML(paste0(input$yearrange, "年度總降雨量", sum(current_year_precp()$precp)), "mm<br>",
         "平均月降雨量", round(sum(current_year_precp()$precp)/12, digits = 2), "mm")
  })
  
  output$hdplot <- renderPlot({
    ggplot(data = current_range_hd(), aes(x = date, y = hd))+
      geom_line()+
      xlab("Date")+
      ylab("humidity(%)")+
      ggtitle(label = "台南市每日濕度")
  })
  output$hddata <- renderDataTable({
    DT::datatable(select(current_range_hd(),c("date","hd")),
                  colnames = c("Date","Humidity(%)"),
                  rownames = FALSE)
  })
  
  output$indexBI <- renderPlot({
    ggmap(map)+
      geom_point(data = current_date_index(), aes(x=VillageLon, y=VillageLat, size = BI))+
      scale_size_continuous(limits=c(0,100),breaks=c(1,5,10,25,50,75))+
      ggtitle("BI指數")
  })
  
  output$indexHI <- renderPlot({
    ggmap(map)+
      geom_point(data = current_date_index(), aes(x=VillageLon, y=VillageLat, size = HI))+
      scale_size_continuous(limits=c(0,100),breaks=c(1,5,10,25,50,75))+
      ggtitle("HI指數")
  })
  output$indexdata <- renderDataTable({
    datatable(select(current_date_index(),c("Date","County","Town","Village","BI","HI")),
              colnames = c("Date","County","Town","Village","BI","HI"),
              rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)
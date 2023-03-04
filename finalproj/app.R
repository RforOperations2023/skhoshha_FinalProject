library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(sf)
library(reshape2)
library(shinyjs)
library(plotly)
library(dplyr)
library(htmltools)
library(readxl)
library(forcats)


# Load datasets
data = read_excel('public_housing.xlsx')
demdata = read.csv('housing.csv')

# Clean neighborhood column name in demographic data
colnames(demdata)[1] = "Neighborhood"

# Filter through Allegheny County and convert lat and long to numeric
data = filter(data, COUNTY_NAME == "Allegheny")

data$LONGITUDE = as.numeric(data$LONGITUDE)
data$LATITUDE = as.numeric(data$LATITUDE)


# Define UI for application
ui <- navbarPage("Pittsburgh Public Housing & Demographics",
                 theme = shinytheme("flatly"),
                 tabPanel("Public Housing Map",
                          sidebarLayout(
                            sidebarPanel(
                              # Headers
                              h3("Allegheny County & Pittsburgh Public Housing Units"),
                              h5("This map shows where public housing units are located across Allegheny
                              County, including Pittsburgh.
                                 It also displays the HUD inspection score given to each building."),
                              br(),
                              h6("Use the slider to select fro different inspection scores:"),
                              # Select Inspection Score Range
                              sliderInput(inputId = "score", 
                                          label = "Inspection Score Range:",
                                          min = min(data$INSPECTION_SCORE), 
                                          max = max(data$INSPECTION_SCORE),
                                          value = range(data$INSPECTION_SCORE)
                              ),
                              # Add Download Button
                              downloadButton(outputId = "downloadData", label = "Download", class = NULL)
                              ),

                            mainPanel(
                              # Using Shiny JS
                              shinyjs::useShinyjs(),
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #D4EFDF;}"),
                              # Map Output
                              leafletOutput("leaflet"),
                              br(), br()
                            )
                          )
                 ),
                 # Plots Panel
                 tabPanel("Demographic Plots",
                           sidebarLayout(
                             sidebarPanel(
                              # Inputs: select population range ---------------------------------------
                              sliderInput("Pop", "Population (2010)",
                                          min = min(demdata$Pop_2010), max = max(demdata$Pop_2010),
                                          value = range(demdata$Pop_2010)
                              ),
                              
                              # Input: select land area size ------------------------------------------
                              sliderInput("area", "Land Area (acre)",
                                          min = min(demdata$Area), max = max(demdata$Area),
                                          value = range(demdata$Area)
                              ),
                              
                              # Input: Select Sector ----------------------------------------------
                              selectInput("Sect", "Sector",
                                          choices = c("1", "2", "3", "4", "5", "6", "7", "8",
                                                      "9", "10", "11", "12", "13", "14", "15", "16"),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = '')),
                          mainPanel(
                          fluidPage(
                                  tabPanel("land", plotlyOutput("plot_land")),
                                  br(), br(),
                                   tabPanel("race", plotlyOutput("plot_race")),
                                  br(), br(),
                                   tabPanel("age", plotlyOutput("plot_age"))),
                          br(), br()
                          ))
                          ),
                 # Data Table Panel
                 tabPanel("Data",
                          fluidPage(
                            wellPanel(DT::dataTableOutput("table"),
                                      br(), br(),
                            DT::dataTableOutput("table2"))
                          )
                 )
)

# Define server logic required to create a map
server <- function(input, output) {
  
  # Color Palette for Map
  pal <- colorBin(
    palette = "RdYlBu",
    domain = data$INSPECTION_SCORE, 4, pretty = FALSE)
  
  #adding an outline layer
  outline <- data[chull(data$LONGITUDE, data$LATITUDE),]
  
  # Basic Map of the public housing buildings in Pittsburgh
  output$leaflet <- renderLeaflet({
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB) %>%
      addPolygons(data = outline, lng = ~LONGITUDE, lat = ~LATITUDE,
                  fill = F, weight = 2, color = "pink", group = "Outline") %>%
      addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, radius = 1.5, 
                       color = ~pal(INSPECTION_SCORE), 
                       label = ~htmlEscape(paste('Inspection Score:',data$INSPECTION_SCORE)))%>% 
      addLegend(position = "topright" , pal = pal,
                values = data$INSPECTION_SCORE, title = "Inspection Score")
    
  })
  
  # Green Infrastructure Filtered data
  mapInputs <- reactive({

    #req(input$score)
    # Scores
    data <- data %>%
      
      # Slider Filter ----------------------------------------------
    filter(
      INSPECTION_SCORE >= input$score[1] &
      INSPECTION_SCORE <= input$score[2])
    
    return(data)
  })
  
  # Replace layer with filtered data
  observe({
    data <- mapInputs()
    # Data is data
    leafletProxy("leaflet", data = data) %>%
      # In this case either lines 92 or 93 will work
      clearMarkers() %>%
      addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, radius = 1.5, 
                       color = ~pal(INSPECTION_SCORE), 
                       label = ~htmlEscape(paste('Inspection Score:',data$INSPECTION_SCORE)))
  })
  
  # Reactive data function -------------------------------------------
  data_subset <- reactive({
    demdata <- demdata %>%
      
      # Slider Filter ----------------------------------------------
    filter(
      Pop_2010 >= input$Pop[1] &
        Pop_2010 <= input$Pop[2],
      Area >= input$area[1] &
        Area <= input$area[2])
    
    # Sector Filter ----------------------------------------------
    if (length(input$Sect) > 0 ) {
      demdata <- subset(demdata, Sector %in% input$Sect)
    }
    
    # Return dataframe ----------------------------------------------
    return(demdata)
  })
  
  # A plot showing land size and population size -----------------------------
  output$plot_land <- renderPlotly({
    dat <- subset(data_subset() )
    ggplot(data = dat, aes(x = Area, y = Pop_2010, color = Neighborhood)) +
      labs(y= "Population Size (2010)", x = "Land Area (acres)",
           title = "Are Land Size and Population Size Correlated?") +
      geom_point()
  })
  
  # A plot showing the percent of White vs African American residents ----------
  output$plot_race <- renderPlotly({
    dat <- subset(data_subset() )
    ggplot(data = dat, aes(x = Perc_White, y = Perc_African_American, 
                           color = Neighborhood)) +
      labs(y= "Percent of African American Residents", 
           x = "Percent of White Residents",
           title = 
             "What is the Racial Distribution of Black and White Residents in Each Neighborhood?") +
      geom_point()
  })
  
  # A plot showing the Age Distribution -----------------------------------
  output$plot_age <- renderPlotly({
    dat <- subset(data_subset() )
    ggplot(data = dat, aes(x = fct_reorder(Neighborhood, Perc_Pop_Age_20.34), 
                           y = Perc_Pop_Age_20.34, fill = Neighborhood)) +
      labs(y= "Percent of Residents Age 20-34", x = "Neighborhood", title = 
             "How Young is the Population in Each Neighborhood?") +
      theme(axis.text.x = element_text(angle=50)) +
      geom_bar(stat = "identity")
  })
  
  # Table for the public housing data
  output$table <- DT::renderDataTable(mapInputs(), options = list(scrollX = T))
  
  # Table for the demographics data
  output$table2 <- DT::renderDataTable({
    subset(data_subset(), select = c(Neighborhood, Sector, Pop_2010, 
                                     Perc_Pop_Change_80.90,
                                     Perc_Pop_Change_90.00, Perc_Pop_Change_00.10,
                                     Area, Perc_African_American, Perc_White, 
                                     Perc_Pop_Age_20.34, Perc_Pop_Age.60.74))
  })
  # Download data button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(data, con)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)


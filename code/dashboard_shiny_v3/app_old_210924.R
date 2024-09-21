# Load required libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(shinydashboard)
library(shinyWidgets)

# Load the data
load("bocsar_poi_spatial.RData")

# Separate BOCSAR and POI variables
bocsar_vars <- sort(unique(bocsar_poi_spatial$BOCSAR_variable[!grepl("^POI_", bocsar_poi_spatial$BOCSAR_variable)]))
poi_vars <- sort(unique(bocsar_poi_spatial$BOCSAR_variable[grepl("^POI_", bocsar_poi_spatial$BOCSAR_variable)]))

# Combine variables with POI at the bottom
all_vars <- c(bocsar_vars, poi_vars)

# Define color palette (flipped direction)
#divergent_palette <- rev(c("#1e87c5", "#73b9c5", "#d0e1db", "#b8df91", "#fdbe26"))
divergent_palette <- (c("#1e87c5", "#73b9c5", "#d0e1db", "#b8df91", "#fdbe26"))

# UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Just Reinvest BOCSAR Dashboard (demo v3)",
  titleWidth = 450  # Adjust this value as needed
),
  dashboardSidebar(
    tags$div(
      style = "padding: 15px;",
      tags$img(src = "jr_logo.jpg", width = "100%")
    ),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      selectInput("variable_select", "Select BOCSAR Variable:",
                  choices = all_vars),  # Use the combined and sorted variables
      selectInput("year_select", "Select Year:", choices = NULL),
      selectInput("lga_select", "Select LGA:", 
                  choices = sort(unique(bocsar_poi_spatial$LGA_NAME23_standardized)))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-top: 3px solid #1e87c5;
        }
      "))
    ),
    fluidRow(
      box(
        width = 12,
        title = "Data Type",
        radioButtons("data_type", NULL,
                     choices = c("Aboriginal" = "Aboriginal",
                                 "Non-Aboriginal" = "Non.Aboriginal",
                                 "Total" = "Total"),
                     inline = TRUE)
      )
    ),
    fluidRow(
      box(
        width = 8,
        title = uiOutput("map_title"),
        leafletOutput("map", height = "600px")
      ),
      box(
        width = 4,
        title = "LGA Information",
        DTOutput("lga_table")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values for available years
  available_years <- reactive({
    bocsar_poi_spatial %>%
      filter(BOCSAR_variable == input$variable_select) %>%
      pull(Year) %>%
      unique() %>%
      sort(decreasing = TRUE)
  })
  
  # Update year choices based on selected variable
  observeEvent(input$variable_select, {
    updateSelectInput(session, "year_select", 
                      choices = available_years(),
                      selected = available_years()[1])
  })
  
  # Reactive values for selected data
  selected_data <- reactive({
    bocsar_poi_spatial %>%
      filter(Year == input$year_select,
             BOCSAR_variable == input$variable_select) %>%
      mutate(value = get(input$data_type))
  })
  
  # Dynamic map title
  output$map_title <- renderUI({
    paste("NSW Map -", input$variable_select)
  })
  
  # Leaflet map
  output$map <- renderLeaflet({
    data <- selected_data()
    pal <- colorNumeric(palette = divergent_palette, 
                        domain = as.numeric(data$value[data$value != "x"]), 
                        na.color = "#808080")
    
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ifelse(value == "x", "#808080", pal(as.numeric(value))),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 1,
        highlight = highlightOptions(
          weight = 5,
          color = "#000000",
          dashArray = "",
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        label = ~sprintf(
          "<strong>%s</strong><br/>%s: %s",
          LGA_NAME23_standardized,
          input$variable_select,
          value
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        layerId = ~LGA_NAME23_standardized
      ) %>%
      addLegend(pal = pal, 
                values = as.numeric(data$value[data$value != "x"]), 
                opacity = 1, 
                title = input$variable_select,
                position = "bottomright",
                labFormat = labelFormat(transform = function(x) round(x, 2)))
  })
  
  # LGA info table
  output$lga_table <- renderDT({
    selected_lga <- selected_data() %>%
      filter(LGA_NAME23_standardized == input$lga_select)
    
    if(nrow(selected_lga) > 0) {
      data.frame(
        Metric = c("LGA", "Year", "BOCSAR Variable", "Aboriginal", "Non-Aboriginal", "Total"),
        Value = c(selected_lga$LGA_NAME23_standardized,
                  selected_lga$Year,
                  selected_lga$BOCSAR_variable,
                  selected_lga$Aboriginal,
                  selected_lga$Non.Aboriginal,
                  selected_lga$Total)
      )
    } else {
      data.frame(Metric = character(0), Value = character(0))
    }
  }, options = list(dom = 't', pageLength = -1))
  
  # Update selected LGA when clicking on the map
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click)) {
      updateSelectInput(session, "lga_select", selected = click$id)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
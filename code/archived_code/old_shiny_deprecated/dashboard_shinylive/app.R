library(shiny)
library(dplyr)
library(ggplot2)
library(sf)

# Load preprocessed data
load("data/bocsar_poi_spatial.RData")

# UI
ui <- fluidPage(
  titlePanel("NSW Criminal Justice System Contacts Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("lga_select", "Select LGA:", choices = sort(unique(bocsar_poi_spatial$LGA_NAME23_standardized))),
      selectInput("variable_select", "Select Variable:", choices = sort(unique(bocsar_poi_spatial$BOCSAR_variable))),
      selectInput("year", "Select Year:", choices = sort(unique(bocsar_poi_spatial$Year), decreasing = TRUE)),
      radioButtons("population", "Population:", choices = c("Aboriginal", "Non-Aboriginal", "Total")),
      width = 3
    ),
    mainPanel(
      plotOutput("map", height = "500px"),
      tableOutput("lga_table"),
      width = 9
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  selected_data <- reactive({
    bocsar_poi_spatial %>% 
      filter(BOCSAR_variable == input$variable_select, Year == input$year) %>%
      select(LGA_NAME23_standardized, !!sym(input$population), geometry)
  })
  
  # Map plot
  output$map <- renderPlot({
    data_for_map <- selected_data()
    
    ggplot(data_for_map) +
      geom_sf(aes(fill = !!sym(input$population))) +
      scale_fill_viridis_c(na.value = "grey50") +
      theme_minimal() +
      labs(title = paste("Data for", input$variable_select, "in", input$year),
           fill = input$population)
  })
  
  # LGA info table
  output$lga_table <- renderTable({
    selected_lga_data <- selected_data() %>%
      filter(LGA_NAME23_standardized == input$lga_select) %>%
      st_drop_geometry()
    
    selected_lga_data
  })
}

# Run the app
shinyApp(ui = ui, server = server)
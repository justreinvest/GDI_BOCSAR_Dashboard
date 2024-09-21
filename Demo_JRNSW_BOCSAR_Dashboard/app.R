# Load required libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(rmarkdown)

# Load the data
load("bocsar_poi_spatial.RData")

# Separate BOCSAR and POI variables
bocsar_vars <- sort(unique(bocsar_poi_spatial$BOCSAR_variable[!grepl("^POI_", bocsar_poi_spatial$BOCSAR_variable)]))
poi_vars <- sort(unique(bocsar_poi_spatial$BOCSAR_variable[grepl("^POI_", bocsar_poi_spatial$BOCSAR_variable)]))

# Combine variables with POI at the bottom
all_vars <- c(bocsar_vars, poi_vars)

# Define color palette
divergent_palette <- c("#1e87c5", "#73b9c5", "#d0e1db", "#b8df91", "#fdbe26")

# Helper functions
calculate_ratio <- function(data, variable, year) {
  data %>%
    filter(BOCSAR_variable == variable, Year == year) %>%
    group_by(LGA_NAME23_standardized) %>%
    summarise(
      ratio = Aboriginal / Non.Aboriginal,
      .groups = 'drop'
    )
}

# UI
ui <- navbarPage(
  title = "Just Reinvest BOCSAR Dashboard (demo v4)",
  
  # Community Profile Tab
  tabPanel("Community Profile",
           sidebarLayout(
             sidebarPanel(
               selectInput("lga_select_profile", "Select LGA:", 
                           choices = sort(unique(bocsar_poi_spatial$LGA_NAME23_standardized))),
               sliderInput("reduction_percent", "Incarceration Reduction %:", 
                           min = 0, max = 100, value = 0, step = 1),
               actionButton("generate_report", "Generate PDF Report")
             ),
             mainPanel(
               plotlyOutput("time_series_court"),
               plotlyOutput("histogram_court"),
               plotlyOutput("time_series_custody"),
               plotlyOutput("histogram_custody"),
               plotlyOutput("time_series_youth"),
               plotlyOutput("histogram_youth"),
               verbatimTextOutput("cost_savings")
             )
           )
  ),
  
  # Map View Tab
  tabPanel("Map View",
           sidebarLayout(
             sidebarPanel(
               selectInput("variable_select", "Select BOCSAR Variable:", choices = all_vars),
               selectInput("year_select", "Select Year:", choices = NULL),
               selectInput("lga_select", "Select LGA:", 
                           choices = sort(unique(bocsar_poi_spatial$LGA_NAME23_standardized))),
               radioButtons("data_type", "Data Type:",
                            choices = c("Aboriginal" = "Aboriginal",
                                        "Non-Aboriginal" = "Non.Aboriginal",
                                        "Total" = "Total"),
                            inline = TRUE)
             ),
             mainPanel(
               leafletOutput("map", height = "600px"),
               DTOutput("lga_table")
             )
           )
  ),
  
  # National Comparison Tab
  tabPanel("National Comparison",
           h3("To be populated with ROGS state/territory comparison data")
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  selected_data <- reactive({
    bocsar_poi_spatial %>%
      filter(Year == input$year_select,
             BOCSAR_variable == input$variable_select) %>%
      mutate(value = get(input$data_type))
  })
  
  # Update year choices based on selected variable
  observeEvent(input$variable_select, {
    years <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == input$variable_select) %>%
      pull(Year) %>%
      unique() %>%
      sort(decreasing = TRUE)
    updateSelectInput(session, "year_select", choices = years, selected = years[1])
  })
  
  # Map View Tab
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
  
  # Community Profile Tab
  output$time_series_court <- renderPlotly({
    data <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == "Adults appearing in court",
             LGA_NAME23_standardized == input$lga_select_profile) %>%
      mutate(ratio = Aboriginal / Non.Aboriginal)
    
    plot_ly(data, x = ~Year, y = ~ratio, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Ratio of Aboriginal to Non-Aboriginal Adults Appearing in Court",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Ratio"))
  })
  
  output$histogram_court <- renderPlotly({
    latest_year <- max(bocsar_poi_spatial$Year[bocsar_poi_spatial$BOCSAR_variable == "Adults appearing in court"])
    data <- calculate_ratio(bocsar_poi_spatial, "Adults appearing in court", latest_year)
    selected_ratio <- data$ratio[data$LGA_NAME23_standardized == input$lga_select_profile]
    
    plot_ly(data, x = ~ratio, type = 'histogram') %>%
      add_markers(x = selected_ratio, y = 0, marker = list(size = 10, color = 'red'),
                  name = input$lga_select_profile) %>%
      layout(title = paste("Distribution of Ratios (", latest_year, ")"),
             xaxis = list(title = "Ratio"),
             yaxis = list(title = "Count"))
  })
  
  output$time_series_custody <- renderPlotly({
    data <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == "Adults in custody",
             LGA_NAME23_standardized == input$lga_select_profile) %>%
      mutate(ratio = Aboriginal / Non.Aboriginal)
    
    plot_ly(data, x = ~Year, y = ~ratio, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Ratio of Aboriginal to Non-Aboriginal Adults in Custody",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Ratio"))
  })
  
  output$histogram_custody <- renderPlotly({
    latest_year <- max(bocsar_poi_spatial$Year[bocsar_poi_spatial$BOCSAR_variable == "Adults in custody"])
    data <- calculate_ratio(bocsar_poi_spatial, "Adults in custody", latest_year)
    selected_ratio <- data$ratio[data$LGA_NAME23_standardized == input$lga_select_profile]
    
    plot_ly(data, x = ~ratio, type = 'histogram') %>%
      add_markers(x = selected_ratio, y = 0, marker = list(size = 10, color = 'red'),
                  name = input$lga_select_profile) %>%
      layout(title = paste("Distribution of Ratios (", latest_year, ")"),
             xaxis = list(title = "Ratio"),
             yaxis = list(title = "Count"))
  })
  
  output$time_series_youth <- renderPlotly({
    data <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == "Young people proceeded against",
             LGA_NAME23_standardized == input$lga_select_profile) %>%
      mutate(ratio = Aboriginal / Non.Aboriginal)
    
    plot_ly(data, x = ~Year, y = ~ratio, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Ratio of Aboriginal to Non-Aboriginal Young People Proceeded Against",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Ratio"))
  })
  
  output$histogram_youth <- renderPlotly({
    latest_year <- max(bocsar_poi_spatial$Year[bocsar_poi_spatial$BOCSAR_variable == "Young people proceeded against"])
    data <- calculate_ratio(bocsar_poi_spatial, "Young people proceeded against", latest_year)
    selected_ratio <- data$ratio[data$LGA_NAME23_standardized == input$lga_select_profile]
    
    plot_ly(data, x = ~ratio, type = 'histogram') %>%
      add_markers(x = selected_ratio, y = 0, marker = list(size = 10, color = 'red'),
                  name = input$lga_select_profile) %>%
      layout(title = paste("Distribution of Ratios (", latest_year, ")"),
             xaxis = list(title = "Ratio"),
             yaxis = list(title = "Count"))
  })
  
  output$cost_savings <- renderText({
    latest_year <- max(bocsar_poi_spatial$Year[bocsar_poi_spatial$BOCSAR_variable == "Adults in custody"])
    custody_data <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == "Adults in custody",
             LGA_NAME23_standardized == input$lga_select_profile,
             Year == latest_year)
    
    if (nrow(custody_data) == 0) {
      return("No data available for the selected LGA.")
    }
    
    current_prisoners <- as.numeric(custody_data$Aboriginal)
    daily_cost <- 299
    reduction <- current_prisoners * (input$reduction_percent / 100)
    annual_savings <- reduction * daily_cost * 365
    
    paste0("Estimated annual cost savings for a ", input$reduction_percent, "% reduction in Aboriginal incarceration: $", format(round(annual_savings), big.mark = ","))
  })
  
  # Generate PDF Report
  observeEvent(input$generate_report, {
    report_file <- tempfile(fileext = ".html")
    rmarkdown::render(
      "report_template.Rmd",
      output_file = report_file,
      params = list(
        lga = input$lga_select_profile,
        reduction_percent = input$reduction_percent
      )
    )
    showNotification("Report generated. Check your downloads folder.", type = "message")
  })
  
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
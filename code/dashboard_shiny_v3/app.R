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

source("generate_report.R")

# Load the data
load("bocsar_poi_spatial_with_pop_and_costs.RData")
print("Initial data load:")
print(head(bocsar_poi_spatial))
print(names(bocsar_poi_spatial))

print("Data types:")
print(sapply(bocsar_poi_spatial, class))

print("Unique BOCSAR variables:")
print(unique(bocsar_poi_spatial$BOCSAR_variable))

# Separate BOCSAR and POI variables
bocsar_vars <- sort(unique(bocsar_poi_spatial$BOCSAR_variable[!grepl("^POI_", bocsar_poi_spatial$BOCSAR_variable)]))
poi_vars <- sort(unique(bocsar_poi_spatial$BOCSAR_variable[grepl("^POI_", bocsar_poi_spatial$BOCSAR_variable)]))

# Combine variables with POI at the bottom
all_vars <- c(bocsar_vars, poi_vars)

# Define color palette
divergent_palette <- c("#1e87c5", "#73b9c5", "#d0e1db", "#b8df91", "#fdbe26")

# Helper function to safely calculate ratio
safe_ratio <- function(aboriginal, non_aboriginal) {
  if (aboriginal == "x" || non_aboriginal == "x") {
    return(NA)
  } else {
    return(as.numeric(aboriginal) / as.numeric(non_aboriginal))
  }
}

# UI
ui <- navbarPage(
  title = "Just Reinvest BOCSAR Dashboard (demo v4)",
  
  # Community Profile Tab
  tabPanel("Community Profile",
           sidebarLayout(
             sidebarPanel(width = 6,
               selectInput("lga_select_profile", "Select LGA:", selected = "Moree Plains",
                           choices = sort(unique(bocsar_poi_spatial$LGA_NAME23_standardized))),
               sliderInput("reduction_percent", "Incarceration Reduction %:", 
                           min = 0, max = 100, value = 0, step = 1),
               tags$div(style = "font-size: 0.9em; word-wrap: break-word;",
                        verbatimTextOutput("cost_savings")
               ),
               actionButton("generate_report", "Generate PDF Report"),
               textOutput("intro_text")#,
               #verbatimTextOutput("debug_print")  # Added for debugging
             ),
             mainPanel(width = 6,
               # tags$div(style = "margin-bottom: 20px;",
               #          textOutput("intro_text")
               # ),
               fluidRow(
                 column(6, plotlyOutput("time_series_court")),
                 column(6, plotlyOutput("histogram_court"))
               ),
               fluidRow(
                 column(6, plotlyOutput("time_series_custody")),
                 column(6, plotlyOutput("histogram_custody"))
               )#,
               #fluidRow(
                 #column(6, plotlyOutput("time_series_youth")),
                 #column(6, plotlyOutput("histogram_youth"))
               #)
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
               leafletOutput("map", height = "480px"),
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
  
  # Debug print
  output$debug_print <- renderPrint({
    req(input$lga_select_profile)
    data <- bocsar_poi_spatial %>%
      filter(LGA_NAME23_standardized == input$lga_select_profile)
    print("Debug: Filtered data for selected LGA")
    print(head(data))
    print("Unique BOCSAR variables for selected LGA:")
    print(unique(data$BOCSAR_variable))
  })
  
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
  
  # Introductory text
  output$intro_text <- renderText({
    req(input$lga_select_profile)
    
    custody_data <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == "Adults in custody",
             LGA_NAME23_standardized == input$lga_select_profile,
             Year == max(Year))
    
    # Handle cases where the data is censored ("x")
    aboriginal_value <- ifelse(custody_data$Aboriginal == "x", "between 1-4", custody_data$Aboriginal)
    non_aboriginal_value <- ifelse(custody_data$Non.Aboriginal == "x", "between 1-4", custody_data$Non.Aboriginal)
    
    if(nrow(custody_data) == 0 || aboriginal_value == "between 1-4" || non_aboriginal_value == "between 1-4") {
      return(sprintf("In %s, in %s there were %s adults in custody with censored data. Some numbers are reported as 'between 1-4'.", 
                     input$lga_select_profile, custody_data$Year, custody_data$Total))
    }
    
    ratio <- as.numeric(custody_data$Aboriginal) / as.numeric(custody_data$Non.Aboriginal)
    
    all_ratios <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == "Adults in custody", Year == max(Year)) %>%
      mutate(ratio = as.numeric(Aboriginal) / as.numeric(Non.Aboriginal)) %>%
      pull(ratio)
    percentile <- ecdf(all_ratios)(ratio) * 100
    
    sprintf("In %s, in %s there were %s adults in custody with a ratio of %.2f Aboriginal:Non-Aboriginal adults incarcerated, 
           putting %s higher than %.1f%% of all LGAs in NSW. The plots below visualize these and other key statistics.", 
            input$lga_select_profile, custody_data$Year, custody_data$Total, ratio, input$lga_select_profile, percentile)
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
    print("Debug: time_series_court")
    data <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == "Adults appearing in court",
             LGA_NAME23_standardized == input$lga_select_profile) %>%
      mutate(ratio = mapply(safe_ratio, Aboriginal, Non.Aboriginal))
    
    print("Filtered data for time series court:")
    print(data)
    
    if (nrow(data) == 0) {
      print("No data available for time series court")
      return(plot_ly() %>% add_annotations(text = "No data available", showarrow = FALSE))
    }
    
    print("Creating time series court plot")
    plot_ly(data, x = ~Year, y = ~ratio, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Ratio of Aboriginal to \nNon-Aboriginal Adults \nAppearing in Court",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Ratio"))
  })
  
  output$histogram_court <- renderPlotly({
    print("Debug: histogram_court")
    data <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == "Adults appearing in court") %>%
      mutate(ratio = mapply(safe_ratio, Aboriginal, Non.Aboriginal))
    
    if (nrow(data) == 0) {
      print("No data available for histogram court")
      return(plot_ly() %>% add_annotations(text = "No data available", showarrow = FALSE))
    }
    
    selected_lga_ratio <- data$ratio[data$LGA_NAME23_standardized == input$lga_select_profile]
    
    # Replace Inf values with NA or a large number (e.g., 10000) to handle Inf properly
    data$ratio[is.infinite(data$ratio)] <- NA  # Or choose a max value you want instead of NA
    
    # Focus bins between 0 and 5, but cover a wider range
    bins <- c(seq(0, 8, length.out = 80), seq(8.1, 10000, length.out = 10000))  # More bins between 0 and 5
    
    # Create histogram data for all LGAs
    hist_data <- hist(data$ratio, breaks = bins, plot = FALSE)
    
    # Create the plot with Plotly
    plot_ly() %>%
      add_bars(x = hist_data$mids, y = hist_data$counts, name = "All LGAs") %>%
      add_bars(x = hist_data$mids[findInterval(selected_lga_ratio, bins)], 
               y = hist_data$counts[findInterval(selected_lga_ratio, bins)], 
               name = input$lga_select_profile) %>%
      layout(title = "Distribution of Ratios (All Years)",
             xaxis = list(title = "Ratio", range = c(0, 8)),
             yaxis = list(title = "Count"),
             barmode = "overlay")
    
  })
  
  output$time_series_custody <- renderPlotly({
    print("Debug: time_series_custody")
    data <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == "Adults in custody",
             LGA_NAME23_standardized == input$lga_select_profile) %>%
      mutate(ratio = mapply(safe_ratio, Aboriginal, Non.Aboriginal))
    
    print("Filtered data for time series custody:")
    print(data)
    
    if (nrow(data) == 0) {
      print("No data available for time series custody")
      return(plot_ly() %>% add_annotations(text = "No data available", showarrow = FALSE))
    }
    
    print("Creating time series custody plot")
    plot_ly(data, x = ~Year, y = ~ratio, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Ratio of Aboriginal to \nNon-Aboriginal Adults \nin Custody",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Ratio"))
  })
  
  output$histogram_custody <- renderPlotly({
    print("Debug: histogram_custody")
    data <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == "Adults in custody") %>%
      mutate(ratio = mapply(safe_ratio, Aboriginal, Non.Aboriginal))
    
    if (nrow(data) == 0) {
      print("No data available for histogram custody")
      return(plot_ly() %>% add_annotations(text = "No data available", showarrow = FALSE))
    }
    
    selected_lga_ratio <- data$ratio[data$LGA_NAME23_standardized == input$lga_select_profile]
    
    bins <- seq(0, 5, length.out = 21)  # 20 bins from 0 to 5
    hist_data <- hist(data$ratio, breaks = bins, plot = FALSE)
    
    plot_ly() %>%
      add_bars(x = hist_data$mids, y = hist_data$counts, name = "All LGAs") %>%
      add_bars(x = hist_data$mids[findInterval(selected_lga_ratio, bins)], 
               y = hist_data$counts[findInterval(selected_lga_ratio, bins)], 
               name = input$lga_select_profile) %>%
      layout(title = "Distribution of Ratios (All Years)",
             xaxis = list(title = "Ratio", range = c(0, 5)),
             yaxis = list(title = "Count"),
             barmode = "overlay")
  })
  
  output$time_series_youth <- NULL #renderPlotly({
  #   print("Debug: time_series_youth")
  #   data <- bocsar_poi_spatial %>%
  #     filter(BOCSAR_variable == "Young people in detention",
  #            LGA_NAME23_standardized == input$lga_select_profile) %>%
  #     mutate(ratio = mapply(safe_ratio, Aboriginal, Non.Aboriginal))
  #   
  #   print("Filtered data for time series youth:")
  #   print(data)
  #   
  #   if (nrow(data) == 0) {
  #     print("No data available for time series youth")
  #     return(plot_ly() %>% add_annotations(text = "No data available", showarrow = FALSE))
  #   }
  #   
  #   print("Creating time series youth plot")
  #   plot_ly(data, x = ~Year, y = ~ratio, type = 'scatter', mode = 'lines+markers') %>%
  #     layout(title = "Ratio of Aboriginal to \nNon-Aboriginal Young People \nin Detention",
  #            xaxis = list(title = "Year"),
  #            yaxis = list(title = "Ratio"))
  # })
  
  output$histogram_youth <- NULL #renderPlotly({
  #   print("Debug: histogram_youth")
  #   data <- bocsar_poi_spatial %>%
  #     filter(BOCSAR_variable == "Young people in detention") %>%
  #     mutate(ratio = mapply(safe_ratio, Aboriginal, Non.Aboriginal))
  #   
  #   if (nrow(data) == 0) {
  #     print("No data available for histogram youth")
  #     return(plot_ly() %>% add_annotations(text = "No data available", showarrow = FALSE))
  #   }
  #   
  #   selected_lga_ratio <- data$ratio[data$LGA_NAME23_standardized == input$lga_select_profile]
  #   
  #   bins <- seq(0, 5, length.out = 21)  # 20 bins from 0 to 5
  #   hist_data <- hist(data$ratio, breaks = bins, plot = FALSE)
  #   
  #   plot_ly() %>%
  #     add_bars(x = hist_data$mids, y = hist_data$counts, name = "All LGAs") %>%
  #     add_bars(x = hist_data$mids[findInterval(selected_lga_ratio, bins)], 
  #              y = hist_data$counts[findInterval(selected_lga_ratio, bins)], 
  #              name = input$lga_select_profile) %>%
  #     layout(title = "Distribution of Ratios (All Years)",
  #            xaxis = list(title = "Ratio", range = c(0, 5)),
  #            yaxis = list(title = "Count"),
  #            barmode = "overlay")
  # })
  
  # Cost savings calculation
  cost_savings <- reactive({
    print("Debug: cost_savings")
    custody_data <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == "Adults in custody",
             LGA_NAME23_standardized == input$lga_select_profile) %>%
      arrange(desc(Year)) %>%
      slice(1)  # Take the most recent year
    
    print(paste("Selected LGA:", input$lga_select_profile))
    print(paste("Most recent year:", custody_data$Year))
    print("Custody data:")
    print(custody_data)
    
    if (nrow(custody_data) == 0) {
      print("No data available")
      return(list(error = "No data available for the selected LGA."))
    }
    
    if (custody_data$Aboriginal == "x") {
      print("Data is censored")
      return(list(
        current_prisoners = "x",
        error = "Data is censored for the selected LGA."
      ))
    }
    
    current_prisoners <- custody_data$Aboriginal_num
    daily_cost <- 299
    current_annual_cost <- current_prisoners * daily_cost * 365
    reduction <- current_prisoners * (input$reduction_percent / 100)
    annual_savings <- reduction * daily_cost * 365
    
    print(paste("Current prisoners:", current_prisoners))
    print(paste("Current annual cost:", current_annual_cost))
    print(paste("Reduction:", reduction))
    print(paste("Annual savings:", annual_savings))
    
    list(
      current_prisoners = custody_data$Aboriginal,
      current_prisoners_num = current_prisoners,
      current_annual_cost = current_annual_cost,
      reduction_percent = input$reduction_percent,
      annual_savings = annual_savings,
      prisoners_after_reduction = current_prisoners - reduction,
      cost_after_reduction = current_annual_cost - annual_savings,
      daily_cost = daily_cost
    )
  })
  
  output$cost_savings <- renderText({
    savings <- cost_savings()
    if (!is.null(savings$error)) {
      print(paste("Error in cost savings:", savings$error))
      return(savings$error)
    }
    
    if (savings$current_prisoners == "x") {
      return("Data is censored for the selected LGA.")
    }
    
    result <- paste0(
      "Current number of Aboriginal adults in custody: ", savings$current_prisoners, "\n",
      "Current annual cost of incarcerating Aboriginal adults: $", format(round(savings$current_annual_cost), big.mark = ","), "\n",
      "Expected annual cost savings of a ", savings$reduction_percent, "% reduction: $", format(round(savings$annual_savings), big.mark = ","), "\n",
      "Number of Aboriginal adults in custody after ", savings$reduction_percent, "% reduction: ", round(savings$prisoners_after_reduction), "\n",
      "Estimated annual cost after reduction: $", format(round(savings$cost_after_reduction), big.mark = ","), "\n",
      "\nBased on an estimated daily cost per prisoner of $", savings$daily_cost
    )
    
    print("Cost savings text:")
    print(result)
    
    return(result)
  })
  
  # Generate PDF Report
  observeEvent(input$generate_report, {
    print("Debug: Generating PDF report")
    showNotification("Generating PDF report...", type = "message", duration = NULL, id = "pdf_notification")
    
    # Prepare data for report
    report_data <- list(
      lga = input$lga_select_profile,
      cost_savings = cost_savings(),
      court_data = bocsar_poi_spatial %>% 
        filter(BOCSAR_variable == "Adults appearing in court", 
               LGA_NAME23_standardized == input$lga_select_profile) %>%
        mutate(ratio = mapply(safe_ratio, Aboriginal, Non.Aboriginal)),
      custody_data = bocsar_poi_spatial %>% 
        filter(BOCSAR_variable == "Adults in custody", 
               LGA_NAME23_standardized == input$lga_select_profile) %>%
        mutate(ratio = mapply(safe_ratio, Aboriginal, Non.Aboriginal)),
      youth_data = bocsar_poi_spatial %>% 
        filter(BOCSAR_variable == "Young people in detention", 
               LGA_NAME23_standardized == input$lga_select_profile) %>%
        mutate(ratio = mapply(safe_ratio, Aboriginal, Non.Aboriginal))
    )
    
    # Call the report generation function
    tryCatch({
      report_file <- generate_report(report_data)
      
      print(paste("Report file generated:", report_file))
      
      # Offer the file for download
      showNotification(
        ui = tagList(
          "PDF report generated. ",
          downloadLink("download_report", "Click here to download.")
        ),
        type = "message",
        duration = NULL,
        id = "pdf_notification"
      )
    }, error = function(e) {
      print(paste("Error generating report:", e$message))
      showNotification(paste("Error generating report:", e$message), type = "error", duration = NULL, id = "pdf_notification")
    })
  })
  
  # Download handler for the generated report
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("BOCSAR_report_", input$lga_select_profile, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      file.copy(report_file, file)
    }
  )
  
  # Update selected LGA when clicking on the map
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click)) {
      print(paste("Map clicked. Selected LGA:", click$id))
      updateSelectInput(session, "lga_select", selected = click$id)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
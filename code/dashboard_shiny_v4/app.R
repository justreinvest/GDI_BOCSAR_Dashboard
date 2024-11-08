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
library(readxl)
library(tidyverse)
library(stringr)
library(shinythemes)
library(shinycssloaders)

# Custom number formatting function
format_number <- function(x) {
  if(is.numeric(x)) {
    format(x, big.mark = ",", scientific = FALSE)
  } else {
    x
  }
}

source("generate_report.R")

# Load the data
load("bocsar_poi_spatial_with_pop_and_costs.RData")

# Separate BOCSAR and POI variables
bocsar_vars <- sort(unique(bocsar_poi_spatial$BOCSAR_variable[!grepl("^POI_", bocsar_poi_spatial$BOCSAR_variable)]))
poi_vars <- sort(unique(bocsar_poi_spatial$BOCSAR_variable[grepl("^POI_", bocsar_poi_spatial$BOCSAR_variable)]))

# Define color palette
divergent_palette <- c("#1e87c5", "#73b9c5", "#d0e1db", "#b8df91", "#fdbe26")

# Clean up variable names
clean_var_names <- c(
  "Infringement notices" = "Infringement Notices",
  "Young people proceeded against" = "Youth Proceeded Against",
  "Young people appearing in court" = "Youth in Court",
  "Young people in detention" = "Youth in Detention",
  "Adults appearing in court" = "Adults in Court",
  "Adults in custody" = "Adults in Custody",
  "Victims of violent crime" = "Victims of Violent Crime",
  "Move-on directions" = "Move-on Directions"
)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      tags$div(class = "logo-container", 
               tags$img(src = "jr_logo.jpg", height = "40px"),
               tags$div("JR NSW Dashboard", class = "logo-title")
      )
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Community Profile", tabName = "community_profile", icon = icon("users")),
                menuItem("Map View", tabName = "map_view", icon = icon("map")),
                menuItem("Persons of Interest", tabName = "poi", icon = icon("user-secret"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script("document.title = 'JR NSW Dashboard';"),
      tags$style(HTML("
        .logo-container {
          display: flex;
          flex-direction: column;
          align-items: center;
          padding: 5px 0;
        }
        .logo-title {
          margin-top: 5px;
          font-size: 14px;
          white-space: nowrap;
        }
        .main-header .logo {
          height: auto;
          width: 300px;
          line-height: 20px;
          padding-top: 0;
          padding-bottom: 0;
        }
        .main-sidebar, .left-side {
          padding-top: 100px;
        }
        .main-header .sidebar-toggle {
          padding-top: 25px;
        }
        .content-wrapper, .right-side, .main-footer {
          margin-left: 300px;
        }
        .main-header > .navbar {
          margin-left: 300px;
        }
        .dark-blue-box {
          background-color: #1a475f !important;
        }
        .info-box {
          min-height: 50px;
        }
        .histogram-explanation {
          font-size: 16px;
          margin-bottom: 15px;
        }
        .selected-lga {
          weight: 3;
          color: #fdbe26;
          fillOpacity: 0.7;
        }
      "))
    ),
    tabItems(
      # Home tab
      tabItem(tabName = "home",
              fluidRow(
                column(12,
                       h1("Welcome to the Just Reinvest NSW Dashboard", align = "center"),
                       p("This dashboard provides insights into Aboriginal interactions with the criminal justice system in New South Wales. Here's what you can find in each tab:", style = "font-size: 18px;")
                )
              ),
              fluidRow(
                column(4,
                       box(
                         width = NULL,
                         status = "warning",
                         solidHeader = TRUE,
                         title = "Community Profile",
                         p("Explore detailed statistics for each Local Government Area (LGA), including adult incarceration and court proceedings. Simulate the impact of reducing these numbers."),
                         actionButton("go_to_community_profile", "Go to Community Profile", class = "btn-block")
                       )
                ),
                column(4,
                       box(
                         width = NULL,
                         status = "info",
                         solidHeader = TRUE,
                         title = "Map View",
                         p("Visualize BOCSAR data across NSW using an interactive map. Select different variables and years to see how they change across the state."),
                         actionButton("go_to_map_view", "Go to Map View", class = "btn-block")
                       )
                ),
                column(4,
                       box(
                         width = NULL,
                         status = "success",
                         solidHeader = TRUE,
                         title = "Persons of Interest",
                         p(HTML("Explore detailed data on Persons of Interest, with the ability to filter by LGA and other variables.<br><br>")),
                         actionButton("go_to_poi", "Go to Persons of Interest", class = "btn-block")
                       )
                )
              ),
              fluidRow(
                column(12,
                       box(
                         width = NULL,
                         title = "About the Data",
                         p("This dashboard uses data from the following sources:"),
                         tags$ul(
                           tags$li(HTML("NSW Bureau of Crime Statistics and Research (BOCSAR) data provided directly from BOCSAR at LGA level disaggregated by Indigenous Status.")),
                           tags$li(HTML("Population data is sourced from the 2021 Census, exported from ABS TableBuilder")),
                           tags$li(HTML("Report on Government Services (ROGS) data: Cost and expenditure information"))
                         ),
                         p(HTML("Data Suppression: As per BOCSAR requirements, counts between 1 and 4 are suppressed and reported as 'x' to support confidentiality and privacy. This is done to protect individuals' identities in areas with small populations or low incident rates.<br><br>")),
                         h4("Data Sources:"),
                         tags$ul(
                           tags$li(HTML("BOCSAR data: <a href='https://bocsar.nsw.gov.au/statistics-dashboards' target='_blank'>https://bocsar.nsw.gov.au/statistics-dashboards</a>")),
                           tags$li(HTML("ROGS data: <a href='https://www.pc.gov.au/ongoing/report-on-government-services/2024/data-downloads' target='_blank'>https://www.pc.gov.au/ongoing/report-on-government-services/2024/data-downloads</a>")),
                           tags$li(HTML("ABS Census data (TableBuilder): <a href='https://www.abs.gov.au/statistics/microdata-tablebuilder/tablebuilder' target='_blank'>https://www.abs.gov.au/statistics/microdata-tablebuilder/tablebuilder</a>"))
                         )
                       )
                )
              )
      ),
      
      # Community Profile tab
      tabItem(tabName = "community_profile",
              fluidRow(
                column(12,
                       box(
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         title = "LGA Information",
                         selectInput("lga_select_profile", "Select LGA:", selected = "Moree Plains",
                                     choices = sort(unique(bocsar_poi_spatial$LGA_NAME23_standardized))),
                         htmlOutput("intro_text")
                       )
                )
              ),
              fluidRow(
                column(6,
                       box(
                         width = NULL,
                         status = "info",
                         solidHeader = TRUE,
                         title = "Summary Table",
                         DTOutput("summary_table")
                       ),
                       box(
                         width = NULL,
                         status = "info",
                         p("Note: As per BOCSAR requirements, counts between 1 and 4 are suppressed and reported as 'x' to support confidentiality and privacy.")
                       )
                ),
                column(6,
                       box(
                         width = NULL,
                         status = "info",
                         solidHeader = TRUE,
                         title = "Rates Comparison (per 1,000)",
                         plotlyOutput("rates_bar_graph"),
                         htmlOutput("rate_comparisons")
                       )
                )
              ),
              fluidRow(
                column(6,
                       box(
                         width = NULL,
                         status = "info",
                         solidHeader = TRUE,
                         title = "Adult Incarceration",
                         tagList(
                           uiOutput("incarceration_top_stats"),
                           sliderInput("reduction_percent_incarceration", "Incarceration Reduction %:", 
                                       min = 0, max = 100, value = 0, step = 1),
                           uiOutput("incarceration_bottom_stats")
                         )
                       )
                ),
                column(6,
                       box(
                         width = NULL,
                         status = "info",
                         title = "Distribution of NSW LGAs - Aboriginal Adults - Incarceration (Rate per 1,000)",
                         solidHeader = TRUE,
                         div(class = "histogram-explanation",
                             htmlOutput("incarceration_histogram_explanation")
                         ),
                         plotlyOutput("histogram_incarceration"),
                         p("Note: Histograms exclude suppressed cell counts ('x'), so the true number in the 0-5 range bin is larger than shown.")
                       )
                )
              ),
              fluidRow(
                column(6,
                       box(
                         width = NULL,
                         status = "success",  # Changed to green
                         solidHeader = TRUE,
                         title = "Court Proceedings",
                         tagList(
                           uiOutput("court_top_stats"),
                           sliderInput("reduction_percent_court", "Court Proceedings Reduction %:", 
                                       min = 0, max = 100, value = 0, step = 1),
                           uiOutput("court_bottom_stats")
                         )
                       )
                ),
                column(6,
                       box(
                         width = NULL,
                         status = "success",  # Changed to green
                         title = "Distribution of NSW LGAs - Aboriginal Adults - Court Proceedings (Rate per 1,000)",
                         solidHeader = TRUE,
                         div(class = "histogram-explanation",
                             htmlOutput("court_histogram_explanation")
                         ),
                         plotlyOutput("histogram_court"),
                         p("Note: Histograms exclude suppressed cell counts ('x'), so the true number in the 0-5 range bin is larger than shown.")
                       )
                )
              ),
              fluidRow(
                column(12,
                       box(
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         title = "Cost Estimates and Additional Information",
                         htmlOutput("outro_text")
                       )
                )
              )
      ),
      
      # Map View tab
      tabItem(tabName = "map_view",
              fluidRow(
                column(3,
                       box(
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         title = "Controls",
                         selectInput("variable_select", "Select BOCSAR Variable (rate per 1,000):", choices = bocsar_vars),
                         selectInput("year_select", "Select Year:", choices = NULL),
                         selectInput("lga_select", "Select LGA:", 
                                     choices = sort(unique(bocsar_poi_spatial$LGA_NAME23_standardized))),
                         radioButtons("data_type", "Data Type:",
                                      choices = c("Aboriginal" = "Aboriginal",
                                                  "Non-Aboriginal" = "Non.Aboriginal",
                                                  "Total" = "Total"),
                                      inline = TRUE),
                         htmlOutput("variable_definition"),
                         DTOutput("lga_table")
                       )
                ),
                column(9,
                       box(
                         width = NULL,
                         status = "info",
                         solidHeader = TRUE,
                         title = "NSW Map",
                         leafletOutput("map", height = "600px")
                       )
                )
              )
      ),
      
      # Persons of Interest tab
      tabItem(tabName = "poi",
              fluidRow(
                column(12,
                       box(
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         title = "Persons of Interest Data",
                         p("This table shows data on Persons of Interest (POI) by Local Government Area (LGA). Use the search boxes and dropdowns below each column header to filter the data."),
                         DTOutput("poi_table"),
                         tags$style(HTML("
                           #poi_table {width: 100% !important;}
                           .dataTables_scrollHeadInner, .dataTable {width: 100% !important;}
                           #poi_table thead tr:nth-child(1) th {font-weight: bold;}
                           #poi_table thead tr:nth-child(2) th, #poi_table thead tr:nth-child(3) th {font-weight: normal;}
                           #poi_table thead tr:nth-child(2) input, #poi_table thead tr:nth-child(3) select {width: 100%; box-sizing: border-box;}
                         "))
                       )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  session$sendCustomMessage("setDocumentTitle", "JR NSW Dashboard")
  
  # Navigation handlers
  observeEvent(input$go_to_community_profile, {
    updateTabItems(session, "tabs", selected = "community_profile")
  })
  
  observeEvent(input$go_to_map_view, {
    updateTabItems(session, "tabs", selected = "map_view")
  })
  
  observeEvent(input$go_to_poi, {
    updateTabItems(session, "tabs", selected = "poi")
  })
  
  # Reactive values for data selection
  selected_data <- reactive({
    req(input$year_select, input$variable_select)
    
    data <- bocsar_poi_spatial %>%
      filter(Year == input$year_select,
             BOCSAR_variable == input$variable_select)
    
    # Handle the Total calculation properly
    if(input$data_type == "Total") {
      data <- data %>%
        mutate(
          value = Total,  # Use the Total column directly instead of calculating
          # Calculate total rate using total population
          rate = case_when(
            Total == "x" ~ NA_real_,
            BOCSAR_variable %in% c("Young people proceeded against", "Young people appearing in court", "Young people in detention") ~ 
              as.numeric(Total) / (Population_Indigenous_10_17 + Population_Non_Indigenous_10_17) * 1000,
            TRUE ~ 
              as.numeric(Total) / (Population_Indigenous_over18 + Population_Non_Indigenous_over18) * 1000
          )
        )
    } else {
      data <- data %>%
        mutate(
          value = get(input$data_type),
          rate = case_when(
            input$data_type == "Aboriginal" ~ Aboriginal_Rate_per_1000,
            input$data_type == "Non.Aboriginal" ~ Non_Aboriginal_Rate_per_1000,
            TRUE ~ NA_real_
          )
        )
    }
    data
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
    
    lga_data <- bocsar_poi_spatial %>%
      filter(LGA_NAME23_standardized == input$lga_select_profile) %>%
      slice(1)
    
    total_adult_pop <- lga_data$Population_Indigenous_over18 + lga_data$Population_Non_Indigenous_over18
    total_youth_pop <- lga_data$Population_Indigenous_10_17 + lga_data$Population_Non_Indigenous_10_17
    
    HTML(sprintf(
      "In %s:<br><br>
      <strong>Adult Population (18+):</strong><br>
      - Aboriginal: %s (%s%% of all adults)<br>
      - Total: %s<br><br>
      <strong>Youth Population (10-17):</strong><br>
      - Aboriginal: %s (%s%% of all youth)<br>
      - Total: %s<br><br>",
      input$lga_select_profile,
      format_number(lga_data$Population_Indigenous_over18),
      round(lga_data$Population_Indigenous_over18 / total_adult_pop * 100, 1),
      format_number(total_adult_pop),
      format_number(lga_data$Population_Indigenous_10_17),
      round(lga_data$Population_Indigenous_10_17 / total_youth_pop * 100, 1),
      format_number(total_youth_pop)
    ))
  })
  
  # Histogram explanations
  output$incarceration_histogram_explanation <- renderText({
    req(input$lga_select_profile)
    HTML(paste0( # PLACEHOLDER FOR TALIA - Additional histogram explanation text goes here
      "<strong>Quick Guide:</strong> This chart shows how your selected LGA's Aboriginal incarceration rate compares to all other NSW LGAs. ",
      "Your LGA is highlighted in yellow. Different rates are displayed on the x-axis (horizontal), with higher rates further to the right. A taller bar on the chart means more LGAs have that rate.",
      "<br><br>"
    ))
  })
  
  output$court_histogram_explanation <- renderText({
    req(input$lga_select_profile)
    HTML(paste0( # PLACEHOLDER FOR TALIA - Additional histogram explanation text goes here
      "<strong>Quick Guide:</strong> This chart shows how your selected LGA's Aboriginal court appearance rate compares to all other NSW LGAs. ",
      "Your LGA is highlighted in yellow. Different rates are displayed on the x-axis (horizontal), with higher rates further to the right. A taller bar on the chart means more LGAs have that rate.",
      "<br><br>"
    ))
  })
  
  # Summary Table
  output$summary_table <- renderDT({
    req(input$lga_select_profile)
    
    lga_data <- bocsar_poi_spatial %>%
      filter(LGA_NAME23_standardized == input$lga_select_profile,
             BOCSAR_variable %in% names(clean_var_names),
             Year == 2023) %>%
      st_drop_geometry() %>%
      select(BOCSAR_variable, Aboriginal, Non.Aboriginal, Total, Aboriginal_Rate_per_1000, Non_Aboriginal_Rate_per_1000) %>%
      mutate(
        BOCSAR_variable = clean_var_names[BOCSAR_variable],
        Aboriginal_Rate_per_1000 = round(Aboriginal_Rate_per_1000, 1),
        Non_Aboriginal_Rate_per_1000 = round(Non_Aboriginal_Rate_per_1000, 1),
        Aboriginal = format_number(Aboriginal),
        Non.Aboriginal = format_number(Non.Aboriginal),
        Total = format_number(Total)
      ) %>%
      rename(
        "Variable" = BOCSAR_variable,
        "Aboriginal Rate (per 1,000 adults)" = Aboriginal_Rate_per_1000,
        "Non-Aboriginal Rate (per 1,000 adults)" = Non_Aboriginal_Rate_per_1000,
        "Non-Aboriginal" = Non.Aboriginal
      )
    
    datatable(lga_data, 
              options = list(
                dom = 't', 
                pageLength = -1, 
                columnDefs = list(list(className = 'dt-center', targets = 1:5))
              ),
              rownames = FALSE) %>%
      formatStyle(columns = 1:6, fontSize = '90%')
  })
  
  # Incarceration stats outputs
  output$incarceration_top_stats <- renderUI({
    custody_data <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == "Adults in custody",
             LGA_NAME23_standardized == input$lga_select_profile) %>%
      arrange(desc(Year)) %>%
      slice(1)
    
    if (nrow(custody_data) == 0 || custody_data$Aboriginal == "x") {
      return(h4("Data is censored or not available for the selected LGA."))
    }
    
    current_prisoners <- as.numeric(custody_data$Aboriginal)
    daily_cost <- custody_data$Incarceration_Cost_Per_Day
    current_annual_cost <- current_prisoners * daily_cost * 365
    
    tagList(
      fluidRow(
        column(12, 
               div(style = "text-align: center;",
                   tags$span(icon("user-lock"), class = "small-circle-icon"),
                   valueBox(
                     value = format_number(current_prisoners),
                     subtitle = "Aboriginal adults in custody (2023)",
                     color = "navy",
                     width = 12
                   )
               )
        ),
        column(12, 
               div(style = "text-align: center;",
                   tags$span(icon("dollar-sign"), class = "small-circle-icon"),
                   valueBox(
                     value = paste0("$", format_number(round(current_annual_cost))),
                     subtitle = "Estimated annual cost (annualised)",
                     color = "navy",
                     width = 12
                   )
               )
        )
      )
    )
  })
  
  output$incarceration_bottom_stats <- renderUI({
    custody_data <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == "Adults in custody",
             LGA_NAME23_standardized == input$lga_select_profile) %>%
      arrange(desc(Year)) %>%
      slice(1)
    
    if (nrow(custody_data) == 0 || custody_data$Aboriginal == "x") {
      return(NULL)
    }
    
    current_prisoners <- as.numeric(custody_data$Aboriginal)
    daily_cost <- custody_data$Incarceration_Cost_Per_Day
    reduction <- current_prisoners * (input$reduction_percent_incarceration / 100)
    annual_savings <- reduction * daily_cost * 365
    
    tagList(
      fluidRow(
        column(6, 
               div(style = "text-align: center;",
                   tags$span(icon("users"), class = "small-circle-icon"),
                   valueBox(
                     value = format_number(round(current_prisoners - reduction)),
                     subtitle = paste0("Number after ", input$reduction_percent_incarceration, "% reduction"),
                     color = "purple",
                     width = 12
                   )
               )
        ),
        column(6, 
               div(style = "text-align: center;",
                   tags$span(icon("piggy-bank"), class = "small-circle-icon"),
                   valueBox(
                     value = paste0("$", format_number(round(annual_savings))),
                     subtitle = paste0("Expected annualised savings"),
                     color = "green",
                     width = 12
                   )
               )
        )
      )
    )
  })
  
  # Court stats outputs
  output$court_top_stats <- renderUI({
    court_data <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == "Adults appearing in court",
             LGA_NAME23_standardized == input$lga_select_profile) %>%
      arrange(desc(Year)) %>%
      slice(1)
    
    if (nrow(court_data) == 0 || court_data$Aboriginal == "x") {
      return(h4("Data is censored or not available for the selected LGA."))
    }
    
    current_proceedings <- as.numeric(court_data$Aboriginal)
    cost_per_finalization <- court_data$Criminal_Courts_Cost_Per_Finalization
    current_annual_cost <- current_proceedings * cost_per_finalization
    
    tagList(
      fluidRow(
        column(12, 
               div(style = "text-align: center;",
                   tags$span(icon("gavel"), class = "small-circle-icon"),
                   valueBox(
                     value = format_number(current_proceedings),
                     subtitle = "Aboriginal adults appearing in NSW Criminal Courts (2023)",
                     color = "olive",
                     width = 12
                   )
               )
        ),
        column(12, 
               div(style = "text-align: center;",
                   tags$span(icon("dollar-sign"), class = "small-circle-icon"),
                   valueBox(
                     value = paste0("$", format_number(round(current_annual_cost))),
                     subtitle = "Estimated annual cost",
                     color = "olive",
                     width = 12
                   )
               )
        )
      )
    )
  })
  
  output$court_bottom_stats <- renderUI({
    court_data <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable == "Adults appearing in court",
             LGA_NAME23_standardized == input$lga_select_profile) %>%
      arrange(desc(Year)) %>%
      slice(1)
    
    if (nrow(court_data) == 0 || court_data$Aboriginal == "x") {
      return(NULL)
    }
    
    current_proceedings <- as.numeric(court_data$Aboriginal)
    cost_per_finalization <- court_data$Criminal_Courts_Cost_Per_Finalization
    reduction <- current_proceedings * (input$reduction_percent_court / 100)
    annual_savings <- reduction * cost_per_finalization
    
    tagList(
      fluidRow(
        column(6, 
               div(style = "text-align: center;",
                   tags$span(icon("users"), class = "small-circle-icon"),
                   valueBox(
                     value = format_number(round(current_proceedings - reduction)),
                     subtitle = paste0("Number after ", input$reduction_percent_court, "% reduction"),
                     color = "purple",
                     width = 12
                   )
               )
        ),
        column(6, 
               div(style = "text-align: center;",
                   tags$span(icon("piggy-bank"), class = "small-circle-icon"),
                   valueBox(
                     value = paste0("$", format_number(round(annual_savings))),
                     subtitle = paste0("Expected annual savings"),
                     color = "green",
                     width = 12
                   )
               )
        )
      )
    )
  })
  
  # Create histogram function
  create_histogram <- function(data, variable, selected_lga) {
    data_filtered <- data %>%
      filter(BOCSAR_variable == variable, Year == 2023) %>%
      mutate(Aboriginal_Rate_per_1000 = as.numeric(Aboriginal_Rate_per_1000))
    
    data_filtered <- data_filtered[!is.na(data_filtered$Aboriginal_Rate_per_1000) & 
                                     data_filtered$Aboriginal_Rate_per_1000 != "x", ]
    
    if(nrow(data_filtered) == 0) {
      return(NULL)
    }
    
    selected_lga_value <- data_filtered$Aboriginal_Rate_per_1000[data_filtered$LGA_NAME23_standardized == selected_lga]
    
    n_bins <- 20
    
    hist_data <- hist(data_filtered$Aboriginal_Rate_per_1000, 
                      breaks = n_bins, plot = FALSE)
    
    selected_bin <- findInterval(selected_lga_value, hist_data$breaks)
    
    colors <- rep(divergent_palette[1], length(hist_data$counts))
    if (selected_bin > 0 && selected_bin <= length(colors)) {
      colors[selected_bin] <- "#fdbe26"  # Yellow highlight
    }
    
    plot_ly() %>%
      add_trace(
        x = hist_data$mids,
        y = hist_data$counts,
        type = "bar",
        marker = list(color = colors),
        name = "All LGAs",
        text = ~paste("Range: ", round(hist_data$breaks[-length(hist_data$breaks)], 1), 
                      " - ", round(hist_data$breaks[-1], 1),
                      "<br>Count: ", hist_data$counts),
        hoverinfo = "text",
        textposition = "none"
      ) %>%
      layout(
        xaxis = list(title = "Rate per 1,000 Aboriginal Population"),
        yaxis = list(title = "Count of LGAs"),
        showlegend = FALSE,
        bargap = 0.1
      )
  }
  
  # Histogram outputs
  output$histogram_incarceration <- renderPlotly({
    create_histogram(bocsar_poi_spatial, "Adults in custody", input$lga_select_profile)
  })
  
  output$histogram_court <- renderPlotly({
    create_histogram(bocsar_poi_spatial, "Adults appearing in court", input$lga_select_profile)
  })
  
  # Map View Tab
  output$map <- renderLeaflet({
    data <- selected_data()
    
    # Create color palette for rates
    pal <- colorNumeric(
      palette = divergent_palette,
      domain = as.numeric(data$rate[!is.na(data$rate)]),
      na.color = "#808080"
    )
    
    selected_lga <- input$lga_select
    
    # Create the base map
    m <- leaflet(data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ifelse(is.na(rate), "#808080", pal(rate)),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = ~sprintf(
          "<strong>%s</strong><br/>%s: %s<br/>Rate per 1,000: %s",
          LGA_NAME23_standardized,
          input$variable_select,
          value,
          ifelse(is.na(rate), "N/A", round(rate, 1))
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        layerId = ~LGA_NAME23_standardized,
        group = "base"
      )
    
    # Add the highlight border in a separate layer on top
    if (!is.null(selected_lga)) {
      selected_data <- data %>%
        filter(LGA_NAME23_standardized == selected_lga)
      
      m <- m %>%
        addPolylines(
          data = selected_data,
          weight = 3,
          color = "#fdbe26",
          opacity = 1,
          group = "highlight"
        )
    }
    
    # Add legend
    m %>%
      addLegend(
        pal = pal,
        values = ~rate,
        opacity = 0.7,
        title = paste(input$variable_select, "<br>Rate per 1,000"),
        position = "bottomright",
        labFormat = labelFormat(transform = function(x) round(x, 1))
      )
  })
  
  output$variable_definition <- renderText({
    if(input$variable_select %in% c("Adults in custody", "Young people in detention")) {
      HTML(paste("The LGA is the Local Government Area of last known residence for the selected variable."))
    } else {
      return(NULL)
    }
  })
  
  output$lga_table <- renderDT({
    selected_lga <- selected_data() %>%
      filter(LGA_NAME23_standardized == input$lga_select) %>%
      select(BOCSAR_variable, Aboriginal, Non.Aboriginal, Total, rate)
    
    if(nrow(selected_lga) > 0) {
      data.frame(
        Metric = c("BOCSAR Variable", "Aboriginal", "Non-Aboriginal", "Total", "Rate per 1,000"),
        Value = c(
          selected_lga$BOCSAR_variable,
          format_number(selected_lga$Aboriginal),
          format_number(selected_lga$Non.Aboriginal),
          format_number(selected_lga$Total),
          ifelse(is.na(selected_lga$rate), "N/A", round(selected_lga$rate, 1))
        )
      )
    } else {
      data.frame(Metric = character(0), Value = character(0))
    }
  }, options = list(dom = 't', pageLength = -1, ordering = FALSE), rownames = FALSE)
  
  # POI Table
  output$poi_table <- renderDT({
    poi_data <- bocsar_poi_spatial %>%
      filter(grepl("^POI_", BOCSAR_variable)) %>%
      st_drop_geometry() %>%
      mutate(
        Aboriginal_Rate = Aboriginal_Rate_per_1000,
        Non_Aboriginal_Rate = Non_Aboriginal_Rate_per_1000,
        Total_Rate = case_when(
          Aboriginal == "x" | Non.Aboriginal == "x" ~ NA_real_,
          TRUE ~ (as.numeric(Aboriginal) + as.numeric(Non.Aboriginal)) / 
            (Population_Indigenous_over18 + Population_Non_Indigenous_over18) * 1000
        )
      ) %>%
      select(
        LGA_NAME23_standardized, Year, BOCSAR_variable,
        Aboriginal, Aboriginal_Rate,
        Non.Aboriginal, Non_Aboriginal_Rate,
        Total, Total_Rate
      ) %>%
      mutate(
        BOCSAR_variable = str_remove(BOCSAR_variable, "POI_"),
        across(ends_with("Rate"), ~round(., 1))
      ) %>%
      rename(
        "Local Government Area" = LGA_NAME23_standardized,
        "Offence type" = BOCSAR_variable,
        "Aboriginal - Count" = Aboriginal,
        "Aboriginal - Rate (per 1,000 adults)" = Aboriginal_Rate,
        "Non-Aboriginal - Count" = Non.Aboriginal,
        "Non-Aboriginal - Rate (per 1,000 adults)" = Non_Aboriginal_Rate,
        "Total - Count" = Total,
        "Total - Rate (per 1,000 adults)" = Total_Rate
      )
    
    datatable(
      poi_data,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        deferRender = TRUE,
        scrollY = 400,
        scroller = TRUE,
        scrollX = TRUE,
        autoWidth = TRUE,
        orderCellsTop = TRUE,
        fixedHeader = TRUE,
        initComplete = JS("
          function(settings, json) {
            var api = this.api();
            // Add a row for search inputs
            var searchRow = $('<tr>').appendTo($('#poi_table thead'));
            // Add a row for dropdowns
            var dropdownRow = $('<tr>').appendTo($('#poi_table thead'));
            
            api.columns().every(function(index) {
              var column = this;
              var title = $(column.header()).text();
              
              // Add search input
              $('<th>').append($('<input type=\"text\">').attr('placeholder', 'Search ' + title)
                .on('keyup change', function() {
                  if (column.search() !== this.value) {
                    column.search(this.value).draw();
                  }
                })
              ).appendTo(searchRow);
              
              // Add dropdown
              var select = $('<select><option value=\"\">All</option></select>')
                .on('change', function() {
                  var val = $.fn.dataTable.util.escapeRegex($(this).val());
                  column.search(val ? '^'+val+'$' : '', true, false).draw();
                });
              
              column.data().unique().sort().each(function(d, j) {
                select.append('<option value=\"'+d+'\">'+d+'</option>')
              });
              
              $('<th>').append(select).appendTo(dropdownRow);
            });
          }
        ")
      ),
      callback = JS("table.columns.adjust().draw();"),
      selection = 'none',
      rownames = FALSE
    ) %>%
      formatStyle(columns = 1:9, fontSize = '90%')
  }, server = FALSE)
  
  # Update selected LGA when clicking on the map
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click)) {
      print(paste("Map clicked. Selected LGA:", click$id))
      updateSelectInput(session, "lga_select", selected = click$id)
    }
  })
  
  output$rates_bar_graph <- renderPlotly({
    req(input$lga_select_profile)
    
    lga_data <- bocsar_poi_spatial %>%
      filter(LGA_NAME23_standardized == input$lga_select_profile,
             BOCSAR_variable %in% names(clean_var_names),
             Year == 2023) %>%
      mutate(BOCSAR_variable = clean_var_names[BOCSAR_variable],
             Aboriginal_Rate_per_1000 = as.numeric(Aboriginal_Rate_per_1000),
             Non_Aboriginal_Rate_per_1000 = as.numeric(Non_Aboriginal_Rate_per_1000)) %>%
      filter(!is.na(Aboriginal_Rate_per_1000) | !is.na(Non_Aboriginal_Rate_per_1000))
    
    # Check if data is empty
    if (nrow(lga_data) == 0) {
      showNotification("No data available for the selected LGA.", type = "warning")
      return(NULL)
    }
    
    # Create the bar plot
    plot_ly() %>%
      add_bars(data = lga_data,
               x = ~BOCSAR_variable, 
               y = ~Aboriginal_Rate_per_1000, 
               name = 'Aboriginal', 
               marker = list(color = '#1e87c5')) %>%
      add_bars(data = lga_data,
               x = ~BOCSAR_variable, 
               y = ~Non_Aboriginal_Rate_per_1000, 
               name = 'Non-Aboriginal', 
               marker = list(color = '#a9ccc9')) %>%
      layout(
        yaxis = list(title = 'Rate per 1,000', tickformat = ',.0f'),
        xaxis = list(title = "", tickangle = 45, tickfont = list(size = 12)),
        margin = list(b = 120, l = 60, r = 20, t = 40),
        barmode = 'group',
        showlegend = TRUE
      )
  })
  
  # Outro text with annualised savings explanation
  output$outro_text <- renderText({
    HTML("
      <h4>Cost Estimates:</h4>
      <p>Cost estimates are based on figures from the 2023 Report on Government Services (ROGS):</p>
      <ul>
        <li>Incarceration: $298.33 per adult per day</li>
        <li>Criminal court case finalization: $1,333.00 per case</li>
      </ul>
      <p>These figures are used to calculate potential cost savings from reducing incarceration rates and court proceedings.</p>
      
      <h4>Understanding Annualised Savings:</h4>
      <p>For incarceration costs, the annualised savings represent the estimated annual cost reduction based on:</p>
      <ul>
        <li>Daily cost per prisoner multiplied by 365 days</li>
        <li>Reduction calculated in annual increments (365 person-days in prison)</li>
        <li>Assumes consistent daily costs throughout the year</li>
      </ul>
      
      <h4>Why Some Items Are Costed and Others Aren't:</h4>
      <p>We provide cost estimates for adult incarceration and court proceedings because:</p>
      <ul>
        <li>These are major drivers of criminal justice system costs</li>
        <li>Reliable, standardized cost data is available from ROGS for these items</li>
        <li>They represent areas where policy changes could lead to significant cost savings</li>
      </ul>
      <p>Other items (e.g., infringement notices, move-on directions) are not costed due to lack of standardized cost data or because their costs are typically much lower and more variable.</p>
    ")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
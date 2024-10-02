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
                         p("This dashboard uses data from the NSW Bureau of Crime Statistics and Research (BOCSAR) and the Report on Government Services (ROGS)."),
                         p(HTML("Data Suppression: As per BOCSAR requirements, counts between 1 and 4 are suppressed and reported as 'x' to support confidentiality and privacy. This is done to protect individuals' identities in areas with small populations or low incident rates.<br><br>")),
                         h4("Data Sources:"),
                         tags$ul(
                           tags$li(HTML("BOCSAR data provided directly from BOCSAR at LGA level disaggregated by Indigenous Status. <br> Other BOCSAR data can be seen at <a href='https://bocsar.nsw.gov.au/statistics-dashboards' target='_blank'>https://bocsar.nsw.gov.au/statistics-dashboards</a>")),
                           tags$li(HTML("ROGS data: <a href='https://www.pc.gov.au/ongoing/report-on-government-services/2024/data-downloads' target='_blank'>https://www.pc.gov.au/ongoing/report-on-government-services/2024/data-downloads</a>"))
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
                         status = "warning",
                         solidHeader = TRUE,
                         title = "Adult Incarceration",
                         sliderInput("reduction_percent_incarceration", "Incarceration Reduction %:", 
                                     min = 0, max = 100, value = 0, step = 1),
                         uiOutput("cost_savings_incarceration")
                       )
                ),
                column(6,
                       box(
                         width = NULL,
                         status = "warning",
                         title = "Distribution of NSW LGAs - Aboriginal Adults - Incarceration (Rate per 1,000)",
                         solidHeader = TRUE,
                         plotlyOutput("histogram_incarceration"),
                         htmlOutput("incarceration_interpretation")
                       )
                )
              ),
              fluidRow(
                column(6,
                       box(
                         width = NULL,
                         status = "danger",
                         solidHeader = TRUE,
                         title = "Court Proceedings",
                         sliderInput("reduction_percent_court", "Court Proceedings Reduction %:", 
                                     min = 0, max = 100, value = 0, step = 1),
                         uiOutput("cost_savings_court")
                       )
                ),
                column(6,
                       box(
                         width = NULL,
                         status = "danger",
                         title = "Distribution of NSW LGAs - Aboriginal Adults - Court Proceedings (Rate per 1,000)",
                         solidHeader = TRUE,
                         plotlyOutput("histogram_court"),
                         htmlOutput("court_interpretation")
                       )
                )
              ),
              fluidRow(
                column(12,
                       box(
                         width = NULL,
                         status = "success",  # This gives a green border
                         solidHeader = TRUE,
                         title = "Additional Information",
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
  
  observeEvent(input$go_to_community_profile, {
    updateTabItems(session, "tabs", selected = "community_profile")
  })
  
  observeEvent(input$go_to_map_view, {
    updateTabItems(session, "tabs", selected = "map_view")
  })
  
  observeEvent(input$go_to_poi, {
    updateTabItems(session, "tabs", selected = "poi")
  })
  
  # Reactive values
  selected_data <- reactive({
    bocsar_poi_spatial %>%
      filter(Year == input$year_select,
             BOCSAR_variable == input$variable_select) %>%
      mutate(value = get(input$data_type),
             rate = case_when(
               input$data_type == "Aboriginal" ~ Aboriginal_Rate_per_1000,
               input$data_type == "Non.Aboriginal" ~ Non_Aboriginal_Rate_per_1000,
               TRUE ~ NA_real_
             ))
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
      format(lga_data$Population_Indigenous_over18, big.mark = ","),
      round(lga_data$Population_Indigenous_over18 / total_adult_pop * 100, 1),
      format(total_adult_pop, big.mark = ","),
      format(lga_data$Population_Indigenous_10_17, big.mark = ","),
      round(lga_data$Population_Indigenous_10_17 / total_youth_pop * 100, 1),
      format(total_youth_pop, big.mark = ",")
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
      mutate(BOCSAR_variable = clean_var_names[BOCSAR_variable],
             Aboriginal_Rate_per_1000 = round(Aboriginal_Rate_per_1000, 1),
             Non_Aboriginal_Rate_per_1000 = round(Non_Aboriginal_Rate_per_1000, 1)) %>%
      rename("Variable" = BOCSAR_variable,
             "Aboriginal Rate (per 1,000)" = Aboriginal_Rate_per_1000,
             "Non-Aboriginal Rate (per 1,000)" = Non_Aboriginal_Rate_per_1000,
             "Non-Aboriginal" = Non.Aboriginal)
    
    datatable(lga_data, 
              options = list(dom = 't', 
                             pageLength = -1, 
                             columnDefs = list(list(className = 'dt-center', targets = 1:5))),
              rownames = FALSE) %>%
      formatStyle(columns = 1:6, fontSize = '90%')
  })
  
  # Updated rates bar graph with specified variables
  output$rates_bar_graph <- renderPlotly({
    req(input$lga_select_profile)
    
    # Define the variables you want to display
    required_variables <- c("Adults in Court", "Adults in Custody", "Infringement Notices", 
                            "Move-on Directions", "Victims of Violent Crime", 
                            "Youth in Court", "Youth in Detention", "Youth Proceeded Against")
    
    # Filter and prepare the data
    lga_data <- bocsar_poi_spatial %>%
      filter(LGA_NAME23_standardized == input$lga_select_profile,
             BOCSAR_variable %in% names(clean_var_names),
             Year == 2023,
             clean_var_names[BOCSAR_variable] %in% required_variables) %>%
      mutate(BOCSAR_variable = clean_var_names[BOCSAR_variable]) %>%
      filter(!is.na(Aboriginal_Rate_per_1000) & Aboriginal_Rate_per_1000 != 0) %>%
      distinct(BOCSAR_variable, .keep_all = TRUE)
    
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
  
  
  
  
  # Rate comparisons
  output$rate_comparisons <- renderUI({
    req(input$lga_select_profile)
    
    lga_data <- bocsar_poi_spatial %>%
      filter(LGA_NAME23_standardized == input$lga_select_profile,
             BOCSAR_variable %in% names(clean_var_names),
             Year == 2023)
    
    all_lga_data <- bocsar_poi_spatial %>%
      filter(BOCSAR_variable %in% names(clean_var_names),
             Year == 2023)
    
    comparisons <- lapply(names(clean_var_names), function(var) {
      rate <- lga_data$Aboriginal_Rate_per_1000[lga_data$BOCSAR_variable == var]
      if(is.na(rate) || rate == "x") {
        return(NULL)
      }
      all_rates <- all_lga_data$Aboriginal_Rate_per_1000[all_lga_data$BOCSAR_variable == var]
      all_rates <- all_rates[!is.na(all_rates) & all_rates != "x"]
      percentile <- ecdf(all_rates)(rate)
      sprintf("%s: %.1f - %.1f%% percentile for NSW LGAs",
              clean_var_names[var], rate, percentile * 100)
    })
    
    comparisons <- comparisons[!sapply(comparisons, is.null)]
    
    tagList(
      h4("Aboriginal Rates (per 1,000):"),
      tags$ul(
        lapply(comparisons, tags$li)
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
      colors[selected_bin] <- "#fdbe26"  # Change to yellow
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
  
  # Histogram interpretations
  output$incarceration_interpretation <- renderText({
    req(input$lga_select_profile)
    
    lga_data <- bocsar_poi_spatial %>%
      filter(LGA_NAME23_standardized == input$lga_select_profile,
             BOCSAR_variable == "Adults in custody",
             Year == 2023)
    
    rate <- lga_data$Aboriginal_Rate_per_1000
    if(is.na(rate) || rate == "x") {
      return("Data is not available for the selected LGA.")
    }
    
    percentile <- ecdf(bocsar_poi_spatial$Aboriginal_Rate_per_1000[bocsar_poi_spatial$BOCSAR_variable == "Adults in custody" & bocsar_poi_spatial$Year == 2023])(rate)
    
    HTML(sprintf(
      "This chart shows where %s sits relative to the rest of the state.<br>
      %s's incarceration rate is %.1f per 1,000, which is higher than %.1f%% of other LGAs in NSW.<br>
      The selected LGA is in the range highlighted in Yellow.",
      input$lga_select_profile,
      input$lga_select_profile,
      rate,
      percentile * 100
    ))
  })
  
  output$court_interpretation <- renderText({
    req(input$lga_select_profile)
    
    lga_data <- bocsar_poi_spatial %>%
      filter(LGA_NAME23_standardized == input$lga_select_profile,
             BOCSAR_variable == "Adults appearing in court",
             Year == 2023)
    
    rate <- lga_data$Aboriginal_Rate_per_1000
    if(is.na(rate) || rate == "x") {
      return("Data is not available for the selected LGA.")
    }
    
    percentile <- ecdf(bocsar_poi_spatial$Aboriginal_Rate_per_1000[bocsar_poi_spatial$BOCSAR_variable == "Adults appearing in court" & bocsar_poi_spatial$Year == 2023])(rate)
    
    HTML(sprintf(
      "This chart shows where %s sits relative to the rest of the state.<br>
      %s's court proceedings rate is %.1f per 1,000, which is higher than %.1f%% of other LGAs in NSW.<br>
      The selected LGA is in the range highlighted in Yellow.",
      input$lga_select_profile,
      input$lga_select_profile,
      rate,
      percentile * 100
    ))
  })
  
  # Cost savings calculation (Adult Incarceration)
  output$cost_savings_incarceration <- renderUI({
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
    reduction <- current_prisoners * (input$reduction_percent_incarceration / 100)
    annual_savings <- reduction * daily_cost * 365
    
    tagList(
      fluidRow(
        column(6, 
               div(style = "text-align: center;",
                   tags$span(icon("user-lock"), class = "small-circle-icon"),
                   valueBox(
                     value = current_prisoners,
                     subtitle = "Aboriginal adults in custody (2023)",
                     width = 12
                   )
               )
        ),
        column(6, 
               div(style = "text-align: center;",
                   tags$span(icon("dollar-sign"), class = "small-circle-icon"),
                   valueBox(
                     value = paste0("$", format(round(current_annual_cost), big.mark = ",")),
                     subtitle = "Estimated annual cost",
                     width = 12
                   )
               )
        )
      ),
      fluidRow(
        column(6, 
               div(style = "text-align: center;",
                   tags$span(icon("piggy-bank"), class = "small-circle-icon"),
                   valueBox(
                     value = paste0("$", format(round(annual_savings), big.mark = ",")),
                     subtitle = paste0("Expected annual savings (", input$reduction_percent_incarceration, "% reduction)"),
                     width = 12
                   )
               )
        ),
        column(6, 
               div(style = "text-align: center;",
                   tags$span(icon("users"), class = "small-circle-icon"),
                   valueBox(
                     value = round(current_prisoners - reduction),
                     subtitle = paste0("Number after ", input$reduction_percent_incarceration, "% reduction"),
                     width = 12
                   )
               )
        )
      )
    )
  })
  
  output$cost_savings_court <- renderUI({
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
    reduction <- current_proceedings * (input$reduction_percent_court / 100)
    annual_savings <- reduction * cost_per_finalization
    
    tagList(
      fluidRow(
        column(6, 
               div(style = "text-align: center;",
                   tags$span(icon("gavel"), class = "small-circle-icon"),
                   valueBox(
                     value = current_proceedings,
                     subtitle = "Aboriginal adults appearing in court (2023)",
                     width = 12
                   )
               )
        ),
        column(6, 
               div(style = "text-align: center;",
                   tags$span(icon("dollar-sign"), class = "small-circle-icon"),
                   valueBox(
                     value = paste0("$", format(round(current_annual_cost), big.mark = ",")),
                     subtitle = "Estimated annual cost",
                     width = 12
                   )
               )
        )
      ),
      fluidRow(
        column(6, 
               div(style = "text-align: center;",
                   tags$span(icon("piggy-bank"), class = "small-circle-icon"),
                   valueBox(
                     value = paste0("$", format(round(annual_savings), big.mark = ",")),
                     subtitle = paste0("Expected annual savings (", input$reduction_percent_court, "% reduction)"),
                     width = 12
                   )
               )
        ),
        column(6, 
               div(style = "text-align: center;",
                   tags$span(icon("users"), class = "small-circle-icon"),
                   valueBox(
                     value = round(current_proceedings - reduction),
                     subtitle = paste0("Number after ", input$reduction_percent_court, "% reduction"),
                     width = 12
                   )
               )
        )
      )
    )
  })
  
  # Map View Tab
  output$map <- renderLeaflet({
    data <- selected_data()
    pal <- colorNumeric(palette = divergent_palette, 
                        domain = data$rate, 
                        na.color = "#808080")
    
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(rate),
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
          "<strong>%s</strong><br/>%s: %.1f per 1,000",
          LGA_NAME23_standardized,
          input$variable_select,
          rate
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        layerId = ~LGA_NAME23_standardized
      ) %>%
      addLegend(pal = pal, 
                values = ~rate, 
                opacity = 1, 
                title = paste(input$variable_select, "Rate per 1,000"),
                position = "bottomright",
                labFormat = labelFormat(transform = function(x) round(x, 1)))
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
        Value = c(selected_lga$BOCSAR_variable,
                  selected_lga$Aboriginal,
                  selected_lga$Non.Aboriginal,
                  selected_lga$Total,
                  round(selected_lga$rate, 1))
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
      select(LGA_NAME23_standardized, Year, BOCSAR_variable, Aboriginal, Non.Aboriginal, Total) %>%
      mutate(BOCSAR_variable = str_remove(BOCSAR_variable, "POI_")) %>%
      rename("Local Government Area" = LGA_NAME23_standardized,
             "Offence type" = BOCSAR_variable,
             "Non-Aboriginal" = Non.Aboriginal)
    
    datatable(poi_data, 
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
      formatStyle(columns = 1:6, fontSize = '90%')
  }, server = FALSE)
  
  # Update selected LGA when clicking on the map
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click)) {
      print(paste("Map clicked. Selected LGA:", click$id))
      updateSelectInput(session, "lga_select", selected = click$id)
    }
  })
  
  # National Comparison Tab
  
  # Load ROGS data
  ROGS_data <- read_excel("ROGS.xlsx")
  
  # Extract data for Q1a1
  rogsq1a1 <- ROGS_data %>% 
    filter(Table_Number == "6A.1") %>% 
    filter(Description3 == "Total recurrent expenditure") %>%
    mutate(across(NSW:NT, ~ as.numeric(gsub(",", "", .)))) %>%
    pivot_longer(cols = NSW:NT, names_to = "State", values_to = "Police") %>%
    select(Year, State, Police)
  rogsq1a1$Police <- rogsq1a1$Police * 1000
  
  # Extract data for Q1a2
  rogsq1a2 <- ROGS_data %>% 
    filter(Table_Number == "7A.11") %>% 
    filter(Description3 == "All criminal courts") %>%
    mutate(across(NSW:NT, ~ as.numeric(gsub(",", "", .)))) %>%
    pivot_longer(cols = NSW:NT, names_to = "State", values_to = "Criminal") %>%
    select(Year, State, Criminal)
  
  # Extract data for Q1a3
  rogsq1a3 <- ROGS_data %>% 
    filter(Table_Number == "7A.12") %>% 
    filter(str_detect(Description3, "All civil courts")) %>%
    mutate(across(NSW:NT, ~ as.numeric(gsub(",", "", .)))) %>%
    pivot_longer(cols = NSW:NT, names_to = "State", values_to = "Civil") %>%
    select(Year, State, Civil)
  
  # Extract data for Q1a4
  rogsq1a4 <- ROGS_data %>% 
    filter(Table_Number == "17A.10") %>% 
    filter(Description3 == "Detention-based services") %>%
    filter(Unit == "$'000") %>%
    mutate(across(NSW:NT, ~ as.numeric(gsub(",", "", .)))) %>%
    pivot_longer(cols = NSW:NT, names_to = "State", values_to = "Youth") %>%
    select(Year, State, Youth)
  
  # Extract data for Q1a5
  rogsq1a5 <- ROGS_data %>% 
    filter(Table_Number == "8A.1") %>% 
    filter(Description3 == "Net operating expenditure") %>% 
    filter(Description4 == "Total") %>%
    mutate(across(NSW:NT, ~ as.numeric(gsub(",", "", .)))) %>%
    pivot_longer(cols = NSW:NT, names_to = "State", values_to = "Incarceration") %>%
    select(Year, State, Incarceration)
  
  # Merge data for Q1
  q1alist <- list(rogsq1a1, rogsq1a2, rogsq1a3, rogsq1a4, rogsq1a5) 
  q1a <- Reduce(function(x,y) merge(x,y,all=TRUE), q1alist)
  q1a$Year <- substr(q1a$Year, start=1, stop=4)
  q1a <- pivot_longer(data=q1a, cols=Police:Incarceration, names_to="Sector", values_to="Expenditure")
  
  # Render plots for National Comparison
  output$line <- renderPlot({
    selected_State <- input$State
    selected_Sector <- input$Sector
    our_data1 <- q1a[q1a$State == selected_State & q1a$Sector == selected_Sector,]
    
    ggplot(our_data1, aes(x=Year, y=Expenditure)) +
      geom_line(color = divergent_palette[1], size = 1.5) +
      geom_point(color = divergent_palette[1], size = 3) +
      labs(title = paste("Expenditure in", selected_State, "for", selected_Sector),
           y = "Expenditure ($'000)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
  })
  
  output$bar <- renderPlot({
    selected_Sector <- input$Sector
    m <- max(q1a$Year)
    our_data2 <- q1a[q1a$Sector == selected_Sector & q1a$Year == m,]
    
    ggplot(our_data2, aes(x=State, y=Expenditure)) +
      geom_bar(aes(fill=ifelse(State==input$State, "Selected", "Other")), stat='identity') +
      scale_fill_manual(values = c(Selected = divergent_palette[5], Other = divergent_palette[1])) +
      labs(title = paste("Expenditure for", selected_Sector, "in", m),
           y = "Expenditure ($'000)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.position = "none")
  })
  
  # Update outro text
  output$outro_text <- renderText({
    HTML("
      <h4>Cost Estimates:</h4>
      <p>Cost estimates are based on figures from the 2023 Report on Government Services (ROGS):</p>
      <ul>
        <li>Incarceration: $298.33 per adult per day</li>
        <li>Criminal court case finalization: $1,333.00 per case</li>
      </ul>
      <p>These figures are used to calculate potential cost savings from reducing incarceration rates and court proceedings.</p>
      <br><br>
      <h4>Why Some Items Are Costed and Others Aren't:</h4>
      <p>We provide cost estimates for adult incarceration and court proceedings because:</p>
      <ul>
        <li>These are major drivers of criminal justice system costs</li>
        <li>Reliable, standardized cost data is available from ROGS for these items</li>
        <li>They represent areas where policy changes could lead to significant cost savings</li>
      </ul>
      <p>Other items (e.g., infringement notices, move-on directions) are not costed due to lack of standardized cost data or because their costs are typically much lower and more variable.</p>
      <br><br>
    ")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
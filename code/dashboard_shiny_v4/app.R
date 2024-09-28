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

source("generate_report.R")

# Load the data
load("bocsar_poi_spatial_with_pop_and_costs.RData")

# Separate BOCSAR and POI variables
bocsar_vars <- sort(unique(bocsar_poi_spatial$BOCSAR_variable[!grepl("^POI_", bocsar_poi_spatial$BOCSAR_variable)]))
poi_vars <- sort(unique(bocsar_poi_spatial$BOCSAR_variable[grepl("^POI_", bocsar_poi_spatial$BOCSAR_variable)]))

# Define color palette
divergent_palette <- c("#1e87c5", "#73b9c5", "#d0e1db", "#b8df91", "#fdbe26")


# UI
ui <- navbarPage(
  title = "Just Reinvest BOCSAR Dashboard (demo v6)",
  id = "navbarPage",  # Add this line
  
  # Splash Page / Landing Panel
  tabPanel("Home",
           fluidPage(
             fluidRow(
               column(12,
                      h1("Welcome to the Just Reinvest BOCSAR Dashboard", align = "center"),
                      br(),
                      p("This dashboard provides insights into Aboriginal interactions with the criminal justice system in New South Wales. Here's what you can find in each tab:", style = "font-size: 18px;"),
                      br()
               )
             ),
             fluidRow(
               column(3,
                      wellPanel(
                        actionLink("go_to_community_profile", 
                                   label = div(
                                     h3("Community Profile"),
                                     p("Explore detailed statistics for each Local Government Area (LGA), including adult incarceration and court proceedings. Simulate the impact of reducing these numbers.")
                                   )
                        )
                      )
               ),
               column(3,
                      wellPanel(
                        actionLink("go_to_map_view",
                                   label = div(
                                     h3("Map View"),
                                     p("Visualize BOCSAR data across NSW using an interactive map. Select different variables and years to see how they change across the state.")
                                   )
                        )
                      )
               ),
               column(3,
                      wellPanel(
                        actionLink("go_to_national_comparison",
                                   label = div(
                                     h3("National Comparison"),
                                     p("Compare NSW data with other states and territories (ROGS data).")
                                   )
                        )
                      )
               ),
               column(3,
                      wellPanel(
                        actionLink("go_to_poi",
                                   label = div(
                                     h3("Persons of Interest"),
                                     p("Explore detailed data on Persons of Interest, with the ability to filter by LGA and other variables.")
                                   )
                        )
                      )
               )
             ),
             fluidRow(
               column(12,
                      br(),
                      h3("Getting Started", align = "center"),
                      p("Begin by selecting a tab above. In the Community Profile and Map View tabs, you can select specific LGAs to explore in detail. Use the sliders in the Community Profile tab to see potential impacts of reducing incarceration and court appearances.", style = "font-size: 16px;"),
                      br(),
                      h3("About the Data", align = "center"),
                      p("This dashboard uses data from the NSW Bureau of Crime Statistics and Research (BOCSAR) and the Report on Government Services (ROGS). Some data may be censored (shown as 'x') to protect privacy when numbers are low.", style = "font-size: 16px;")
               )
             )
           )
  ),
  
  # Community Profile Tab
  # Community Profile Tab
  tabPanel("Community Profile",
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
                      title = "Adult Incarceration",
                      sliderInput("reduction_percent_incarceration", "Incarceration Reduction %:", 
                                  min = 0, max = 100, value = 0, step = 1),
                      uiOutput("cost_savings_incarceration")
                    )
             ),
             column(6,
                    box(
                      width = NULL,
                      status = "info",
                      solidHeader = TRUE,
                      title = "Incarceration Rate Distribution",
                      plotlyOutput("histogram_incarceration")
                    )
             )
           ),
           fluidRow(
             column(6,
                    box(
                      width = NULL,
                      status = "warning",
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
                      status = "warning",
                      solidHeader = TRUE,
                      title = "Court Proceedings Rate Distribution",
                      plotlyOutput("histogram_court")
                    )
             )
           ),
           fluidRow(
             column(12,
                    box(
                      width = NULL,
                      status = "primary",
                      solidHeader = TRUE,
                      title = "Additional Information",
                      htmlOutput("outro_text")
                    )
             )
           )
  ),
  
  # Map View Tab
  tabPanel("Map View",
           sidebarLayout(
             sidebarPanel(
               selectInput("variable_select", "Select BOCSAR Variable:", choices = bocsar_vars),
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
           fluidPage(
             titlePanel(title = h4("Total Sector Expenditure", align="center")),
             sidebarPanel(
               selectInput("State", "Select State",
                           choices = c("NSW", "Vic","Qld","WA","SA","Tas","ACT","NT"),
                           selected = "NSW"),
               selectInput("Sector", "Select Sector",
                           choices = c("Police", "Criminal","Civil","Youth","Incarceration"),
                           selected = "Police")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot", plotOutput("line"), plotOutput("bar"))
               )
             )
           )
  ),
  
  # Persons of Interest Tab
  tabPanel("Persons of Interest",
           fluidRow(
             column(12,
                    h2("Persons of Interest Data"),
                    p("This table shows data on Persons of Interest (POI) by Local Government Area (LGA). Use the filters below to explore the data."),
                    DTOutput("poi_table")
             )
           )
  )
)

# Server
server <- function(input, output, session) {
  observeEvent(input$go_to_community_profile, {
    updateNavbarPage(session, "navbarPage", selected = "Community Profile")
  })
  
  observeEvent(input$go_to_map_view, {
    updateNavbarPage(session, "navbarPage", selected = "Map View")
  })
  
  observeEvent(input$go_to_national_comparison, {
    updateNavbarPage(session, "navbarPage", selected = "National Comparison")
  })
  
  observeEvent(input$go_to_poi, {
    updateNavbarPage(session, "navbarPage", selected = "Persons of Interest")
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
    
    lga_data <- bocsar_poi_spatial %>%
      filter(LGA_NAME23_standardized == input$lga_select_profile) %>%
      slice(1)
    
    HTML(sprintf(
      "In %s, the Aboriginal population is %s, with an adult Aboriginal population (18+) of %s.<br><br>
    Use the sliders below to see the estimated cost saving associated with a percentage reduction<br>
    in number of adults in custody, and number of court proceedings in the selected LGA.",
      input$lga_select_profile,
      format(lga_data$Aboriginal_Population, big.mark = ","),
      format(lga_data$Aboriginal_Adult_Population, big.mark = ",")
    ))
  })
  
  output$outro_text <- renderText({HTML("Cost estimates are based on figures of $298.33 for incarceration per adult per day, and<br>
      $1,333.00 per criminal court case finalisation (2023, ROGS)")})
  
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
        column(6, valueBox(
          value = current_prisoners,
          subtitle = "Aboriginal adults in custody (2023)",
          icon = icon("user-lock"),
          color = "blue",
          width = 12
        )),
        column(6, valueBox(
          value = paste0("$", format(round(current_annual_cost), big.mark = ",")),
          subtitle = "Estimated annual cost",
          icon = icon("dollar-sign"),
          color = "green",
          width = 12
        ))
      ),
      fluidRow(
        column(6, valueBox(
          value = paste0("$", format(round(annual_savings), big.mark = ",")),
          subtitle = paste0("Expected annual savings (", input$reduction_percent_incarceration, "% reduction)"),
          icon = icon("piggy-bank"),
          color = "purple",
          width = 12
        )),
        column(6, valueBox(
          value = round(current_prisoners - reduction),
          subtitle = paste0("Number after ", input$reduction_percent_incarceration, "% reduction"),
          icon = icon("users"),
          color = "red",
          width = 12
        ))
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
        column(6, valueBox(
          value = current_proceedings,
          subtitle = "Aboriginal adults appearing in court (2023)",
          icon = icon("gavel"),
          color = "yellow",
          width = 12
        )),
        column(6, valueBox(
          value = paste0("$", format(round(current_annual_cost), big.mark = ",")),
          subtitle = "Estimated annual cost",
          icon = icon("dollar-sign"),
          color = "green",
          width = 12
        ))
      ),
      fluidRow(
        column(6, valueBox(
          value = paste0("$", format(round(annual_savings), big.mark = ",")),
          subtitle = paste0("Expected annual savings (", input$reduction_percent_court, "% reduction)"),
          icon = icon("piggy-bank"),
          color = "purple",
          width = 12
        )),
        column(6, valueBox(
          value = round(current_proceedings - reduction),
          subtitle = paste0("Number after ", input$reduction_percent_court, "% reduction"),
          icon = icon("users"),
          color = "red",
          width = 12
        ))
      )
    )
  })
  
  create_histogram <- function(data, variable, selected_lga) {
    data_filtered <- data %>%
      filter(BOCSAR_variable == variable, Year == 2023) %>%
      mutate(
        Aboriginal = ifelse(Aboriginal == "x", 0, as.numeric(Aboriginal)),
        Rate = Aboriginal / Aboriginal_Adult_Population * 1000
      )
    
    selected_lga_value <- data_filtered$Rate[data_filtered$LGA_NAME23_standardized == selected_lga]
    
    n_bins <- 20
    
    hist_data <- hist(data_filtered$Rate[data_filtered$Rate > 0], 
                      breaks = n_bins, plot = FALSE)
    
    selected_bin <- findInterval(selected_lga_value, hist_data$breaks)
    
    colors <- rep("rgba(31, 119, 180, 0.6)", length(hist_data$counts))
    if (selected_bin > 0 && selected_bin <= length(colors)) {
      colors[selected_bin] <- "rgba(255, 0, 0, 0.6)"
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
        title = paste0(variable, " (Rate per 1,000) - NSW LGAs"),
        xaxis = list(title = "Rate per 1,000 Adult Aboriginal Population"),
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
  
  # Persons of Interest table
  output$poi_table <- renderDT({
    poi_data <- bocsar_poi_spatial %>%
      filter(grepl("^POI_", BOCSAR_variable)) %>%
      select(LGA_NAME23_standardized, Year, BOCSAR_variable, Aboriginal, Non.Aboriginal, Total)
    
    datatable(poi_data, 
              filter = 'top',
              options = list(
                pageLength = 15,
                autoWidth = TRUE,
                scrollX = TRUE
              )
    )
  })
  
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
      geom_line() +
      labs(title = paste("Expenditure in", selected_State, "for", selected_Sector),
           y = "Expenditure ($'000)")
  })
  
  output$bar <- renderPlot({
    selected_Sector <- input$Sector
    m <- max(q1a$Year)
    our_data2 <- q1a[q1a$Sector == selected_Sector & q1a$Year == m,]
    
    ggplot(our_data2, aes(x=State, y=Expenditure)) +
      geom_bar(aes(fill=ifelse(State==input$State, "high", "default")), stat='identity', show.legend=FALSE) +
      scale_fill_manual(values = c(high = "yellow", default = "grey30")) +
      labs(title = paste("Expenditure for", selected_Sector, "in", m),
           y = "Expenditure ($'000)")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
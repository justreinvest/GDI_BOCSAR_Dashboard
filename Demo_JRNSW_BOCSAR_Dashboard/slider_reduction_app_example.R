#library(shiny)
library(sf)
library(tidyverse)
library(viridis)
library(DT)
library(leaflet)
library(raster)
library(terra)

load("NSW_LGAs_precomputed")

# UI
ui <- fluidPage(
  titlePanel("NSW Indigenous Incarceration Rates"),
  sidebarLayout(
    sidebarPanel(
      selectInput("lga_select", "Select LGA:", choices = sort(unique(NSW_LGAs_precomputed$LGA_NAME23_standardized))),
      sliderInput("reduction_percent", "Percentage Reduction:", min = 0, max = 100, value = 0, step = 1),
      verbatimTextOutput("cost_savings_detail"),
      width = 4
    ),
    mainPanel(
      radioButtons("map_type", "Map Type:",
                   choices = c("Prisoner Count (2022)" = "2022",
                               "Prisoner Count (2023)" = "2023",
                               "Rate per 100,000 (2022)" = "rate2022",
                               "Rate per 100,000 (2023)" = "rate2023")),
      leafletOutput("map", height = "500px"),
      DTOutput("lga_table"),
      width = 8
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  selected_data <- reactive({
    if (input$map_type == "2022") {
      return(list(col = NSW_LGAs_precomputed$PrisonerCount_2022, title = "Indigenous Prisoner Count (2022)", suffix = ""))
    } else if (input$map_type == "2023") {
      return(list(col = NSW_LGAs_precomputed$PrisonerCount_2023, title = "Indigenous Prisoner Count (2023)", suffix = ""))
    } else if (input$map_type == "rate2022") {
      return(list(col = NSW_LGAs_precomputed$Rate_2022, title = "Rate per 100,000 (2022)", suffix = " per 100,000"))
    } else {
      return(list(col = NSW_LGAs_precomputed$Rate_2023, title = "Rate per 100,000 (2023)", suffix = " per 100,000"))
    }
  })
  
  # Leaflet map
  output$map <- renderLeaflet({
    pal <- colorNumeric(palette = "viridis", domain = selected_data()$col)
    
    leaflet(NSW_LGAs_precomputed) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(selected_data()$col),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~sprintf(
          "<strong>%s</strong><br/>%s: %s%s",
          LGA_NAME23_standardized,
          selected_data()$title,
          formatC(selected_data()$col, format="f", digits=1, big.mark=","),
          selected_data()$suffix
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        layerId = ~LGA_NAME23_standardized
      ) %>%
      addLegend(pal = pal, values = selected_data()$col, opacity = 0.7, title = selected_data()$title,
                position = "bottomright")
  })
  
  # LGA info table
  output$lga_table <- renderDT({
    selected_lga <- NSW_LGAs_precomputed[NSW_LGAs_precomputed$LGA_NAME23_standardized == input$lga_select, ]
    data.frame(
      Metric = c("Indigenous Prisoner Count (2022)", "Indigenous Prisoner Count (2023)", 
                 "Rate per 100,000 (2022)", "Rate per 100,000 (2023)"),
      Value = c(selected_lga$PrisonerCount_2022, selected_lga$PrisonerCount_2023, 
                selected_lga$Rate_2022, selected_lga$Rate_2023)
    )
  }, options = list(dom = 't', pageLength = -1))
  
  # Cost savings calculation with detailed information
  output$cost_savings_detail <- renderText({
    selected_lga <- NSW_LGAs_precomputed[NSW_LGAs_precomputed$LGA_NAME23_standardized == input$lga_select, ]
    prisoners_2023 <- selected_lga$PrisonerCount_2023
    daily_cost <- 298  # Cost per prisoner per day
    annual_cost <- prisoners_2023 * daily_cost * 365
    reduction <- prisoners_2023 * (input$reduction_percent / 100)
    prisoners_after_reduction <- round(prisoners_2023 - reduction)
    savings <- (prisoners_2023 - prisoners_after_reduction) * daily_cost * 365
    
    paste0("LGA: ", input$lga_select,"\n",
      "Number of Indigenous prisoners ", 
      "in 2023: ", format(prisoners_2023, big.mark = ","), "\n",
      "Annual estimated cost of ", 
      "incarceration: $", format(round(annual_cost), big.mark = ","), "\n",
      "Expected cost saving of ", 
      input$reduction_percent, "% reduction: $", 
      format(round(savings), big.mark = ","), "\n",
      "Number of prisoners after ", 
      input$reduction_percent, "% reduction: ", 
      format(round(prisoners_after_reduction), big.mark = ","),
      "\n \nBased on an estimated daily cost \nper prisoner of $", daily_cost
    )
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
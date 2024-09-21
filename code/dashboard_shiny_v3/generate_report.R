library(rmarkdown)
library(knitr)
library(ggplot2)

generate_report <- function(report_data) {
  # Create a temporary file for the report
  report_file <- tempfile(fileext = ".pdf")
  
  # Create a temporary Rmd file
  rmd_file <- tempfile(fileext = ".Rmd")
  
  # Write the report content to the Rmd file
  writeLines(
    c(
      "---",
      "title: BOCSAR Dashboard Report",
      paste0("subtitle: ", report_data$lga),
      "date: \"`r Sys.Date()`\"",
      "output: pdf_document",
      "---",
      "",
      "## Cost Savings Analysis",
      "",
      "```{r, echo=FALSE}",
      "library(knitr)",
      "if (!is.null(report_data$cost_savings$error)) {",
      "  cat(report_data$cost_savings$error)",
      "} else {",
      "  kable(data.frame(",
      "    Metric = c('Current number of Aboriginal adults in custody',",
      "               'Current annual cost of incarceration',",
      "               paste0('Expected annual cost savings (', report_data$cost_savings$reduction_percent, '% reduction)'),",
      "               paste0('Number of adults in custody after ', report_data$cost_savings$reduction_percent, '% reduction'),",
      "               'Estimated annual cost after reduction'),",
      "    Value = c(report_data$cost_savings$current_prisoners,",
      "              paste0('$', format(round(report_data$cost_savings$current_annual_cost), big.mark = ',')),",
      "              paste0('$', format(round(report_data$cost_savings$annual_savings), big.mark = ',')),",
      "              round(report_data$cost_savings$prisoners_after_reduction),",
      "              paste0('$', format(round(report_data$cost_savings$cost_after_reduction), big.mark = ','))",
      "    )",
      "  ))",
      "}",
      "```",
      "",
      "\\*Based on an estimated daily cost per prisoner of $", report_data$cost_savings$daily_cost,
      "",
      "## Plots",
      "",
      "```{r, echo=FALSE, fig.width=10, fig.height=6}",
      "par(mfrow = c(2, 3))",
      "plot(ggplot(report_data$court_data, aes(x = Year, y = ratio)) + geom_line() + ggtitle('Adults appearing in court'))",
      "plot(ggplot(report_data$court_data, aes(x = ratio)) + geom_histogram() + ggtitle('Histogram - Court appearances'))",
      "plot(ggplot(report_data$custody_data, aes(x = Year, y = ratio)) + geom_line() + ggtitle('Adults in custody'))",
      "plot(ggplot(report_data$custody_data, aes(x = ratio)) + geom_histogram() + ggtitle('Histogram - Custody'))",
      "plot(ggplot(report_data$youth_data, aes(x = Year, y = ratio)) + geom_line() + ggtitle('Young people in detention'))",
      "plot(ggplot(report_data$youth_data, aes(x = ratio)) + geom_histogram() + ggtitle('Histogram - Youth detention'))",
      "```"
    ),
    rmd_file
  )
  
  # Render the report
  rmarkdown::render(rmd_file, output_file = report_file, quiet = TRUE)
  
  return(report_file)
}
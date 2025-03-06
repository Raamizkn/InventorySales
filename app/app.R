# Main application file for the Retail Sales and Inventory Forecasting Dashboard

# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(forecast)
library(prophet)
library(logger)

# Create logs directory if it doesn't exist
if (!dir.exists("logs")) {
  dir.create("logs", recursive = TRUE)
}

# Initialize logging
log_appender(appender_file("logs/shiny_app.log"))
log_info("Starting application")

# Source UI and server files
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server) 
# Data Ingestion Script
# This script loads data from various sources (CSV, databases) into R

# Load required libraries
library(readr)
library(dplyr)
library(lubridate)
library(logger)

# Initialize logging
log_appender(appender_file("logs/data_ingestion.log"))
log_info("Starting data ingestion process")

# Function to load CSV data
load_csv_data <- function(file_path) {
  log_info(paste("Loading CSV file:", file_path))
  tryCatch({
    data <- read_csv(file_path, col_types = cols())
    log_info(paste("Successfully loaded", nrow(data), "rows from", file_path))
    return(data)
  }, error = function(e) {
    log_error(paste("Error loading file:", file_path, "-", e$message))
    stop(paste("Error loading file:", file_path, "-", e$message))
  })
}

# Function to load data from a database (placeholder - customize as needed)
load_db_data <- function(connection_string, query) {
  log_info("Loading data from database")
  # This is a placeholder. In a real implementation, you would:
  # 1. Connect to the database using DBI or RODBC
  # 2. Execute the query
  # 3. Fetch the results
  # 4. Close the connection
  
  # Example (commented out):
  # library(DBI)
  # conn <- dbConnect(odbc::odbc(), .connection_string = connection_string)
  # data <- dbGetQuery(conn, query)
  # dbDisconnect(conn)
  # return(data)
  
  log_warn("Database connection not implemented - using sample data instead")
  return(NULL)
}

# Main function to ingest all required data
ingest_all_data <- function(base_path = "data/raw") {
  log_info("Starting to ingest all data sources")
  
  # Create directory for processed data if it doesn't exist
  if (!dir.exists("data/processed")) {
    dir.create("data/processed", recursive = TRUE)
  }
  
  # Load sales data
  sales_file <- file.path(base_path, "sample_retail_sales.csv")
  sales_data <- load_csv_data(sales_file)
  
  # Load product information
  product_file <- file.path(base_path, "product_info.csv")
  product_data <- load_csv_data(product_file)
  
  # Load store information
  store_file <- file.path(base_path, "store_info.csv")
  store_data <- load_csv_data(store_file)
  
  # Return all loaded data as a list
  result <- list(
    sales = sales_data,
    products = product_data,
    stores = store_data
  )
  
  log_info("Data ingestion completed successfully")
  return(result)
}

# If this script is run directly (not sourced), execute the main function
if (!interactive()) {
  raw_data <- ingest_all_data()
  # Save the raw data for the next step in the pipeline
  saveRDS(raw_data, "data/processed/raw_data.rds")
} 
# Data Cleaning Script
# This script cleans and preprocesses the raw data for analysis

# Load required libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(logger)

# Initialize logging
log_appender(appender_file("logs/data_cleaning.log"))
log_info("Starting data cleaning process")

# Function to clean sales data
clean_sales_data <- function(sales_data) {
  log_info("Cleaning sales data")
  
  # Convert date column to proper date format
  sales_data <- sales_data %>%
    mutate(date = as.Date(date))
  
  # Check for missing values
  missing_count <- sum(is.na(sales_data))
  if (missing_count > 0) {
    log_warn(paste("Found", missing_count, "missing values in sales data"))
    
    # Handle missing values (in this case, we'll remove rows with any NA)
    sales_data <- sales_data %>%
      filter(complete.cases(.))
    
    log_info(paste("Removed rows with missing values. Remaining rows:", nrow(sales_data)))
  } else {
    log_info("No missing values found in sales data")
  }
  
  # Check for duplicate entries
  duplicate_count <- sum(duplicated(sales_data))
  if (duplicate_count > 0) {
    log_warn(paste("Found", duplicate_count, "duplicate entries in sales data"))
    
    # Remove duplicates
    sales_data <- sales_data %>%
      distinct()
    
    log_info(paste("Removed duplicate entries. Remaining rows:", nrow(sales_data)))
  } else {
    log_info("No duplicate entries found in sales data")
  }
  
  # Calculate total sales amount
  sales_data <- sales_data %>%
    mutate(sales_amount = sales_quantity * unit_price)
  
  log_info("Sales data cleaning completed")
  return(sales_data)
}

# Function to clean product data
clean_product_data <- function(product_data) {
  log_info("Cleaning product data")
  
  # Check for missing values
  missing_count <- sum(is.na(product_data))
  if (missing_count > 0) {
    log_warn(paste("Found", missing_count, "missing values in product data"))
    
    # Handle missing values
    product_data <- product_data %>%
      filter(complete.cases(.))
    
    log_info(paste("Removed rows with missing values. Remaining rows:", nrow(product_data)))
  } else {
    log_info("No missing values found in product data")
  }
  
  log_info("Product data cleaning completed")
  return(product_data)
}

# Function to clean store data
clean_store_data <- function(store_data) {
  log_info("Cleaning store data")
  
  # Convert opening_date to proper date format
  store_data <- store_data %>%
    mutate(opening_date = as.Date(opening_date))
  
  # Check for missing values
  missing_count <- sum(is.na(store_data))
  if (missing_count > 0) {
    log_warn(paste("Found", missing_count, "missing values in store data"))
    
    # Handle missing values
    store_data <- store_data %>%
      filter(complete.cases(.))
    
    log_info(paste("Removed rows with missing values. Remaining rows:", nrow(store_data)))
  } else {
    log_info("No missing values found in store data")
  }
  
  log_info("Store data cleaning completed")
  return(store_data)
}

# Main function to clean all data
clean_all_data <- function(raw_data) {
  log_info("Starting to clean all data")
  
  # Clean each dataset
  cleaned_sales <- clean_sales_data(raw_data$sales)
  cleaned_products <- clean_product_data(raw_data$products)
  cleaned_stores <- clean_store_data(raw_data$stores)
  
  # Return all cleaned data as a list
  result <- list(
    sales = cleaned_sales,
    products = cleaned_products,
    stores = cleaned_stores
  )
  
  log_info("Data cleaning completed successfully")
  return(result)
}

# If this script is run directly (not sourced), execute the main function
if (!interactive()) {
  # Load the raw data
  raw_data <- readRDS("data/processed/raw_data.rds")
  
  # Clean the data
  cleaned_data <- clean_all_data(raw_data)
  
  # Save the cleaned data for the next step in the pipeline
  saveRDS(cleaned_data, "data/processed/cleaned_data.rds")
} 
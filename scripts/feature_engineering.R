# Feature Engineering Script
# This script creates features for time series forecasting models

# Load required libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(tsibble)
library(logger)

# Initialize logging
log_appender(appender_file("logs/feature_engineering.log"))
log_info("Starting feature engineering process")

# Function to create time-based features
create_time_features <- function(sales_data) {
  log_info("Creating time-based features")
  
  # Extract date components
  sales_data <- sales_data %>%
    mutate(
      year = year(date),
      month = month(date),
      day = day(date),
      day_of_week = wday(date, label = TRUE),
      week_of_year = isoweek(date),
      is_weekend = ifelse(wday(date) %in% c(1, 7), 1, 0),
      quarter = quarter(date)
    )
  
  # Create holiday flags (simplified example - in practice, use a holiday calendar)
  # This is a placeholder - you would typically use a more comprehensive holiday list
  holidays <- as.Date(c("2020-01-01", "2020-01-20", "2020-02-17", "2020-05-25"))
  sales_data <- sales_data %>%
    mutate(is_holiday = ifelse(date %in% holidays, 1, 0))
  
  log_info("Time-based features created")
  return(sales_data)
}

# Function to create lag features
create_lag_features <- function(sales_data) {
  log_info("Creating lag features")
  
  # Convert to tsibble for time series operations
  # Group by product and store for separate time series
  ts_data <- sales_data %>%
    as_tsibble(key = c(product_id, store_id), index = date)
  
  # Create lag features (previous day, previous week)
  ts_data <- ts_data %>%
    group_by(product_id, store_id) %>%
    mutate(
      sales_lag1 = lag(sales_quantity, 1),
      sales_lag7 = lag(sales_quantity, 7)
    ) %>%
    ungroup()
  
  # Create rolling averages (7-day and 14-day)
  ts_data <- ts_data %>%
    group_by(product_id, store_id) %>%
    mutate(
      sales_ma7 = slider::slide_dbl(sales_quantity, mean, .before = 6, .complete = FALSE),
      sales_ma14 = slider::slide_dbl(sales_quantity, mean, .before = 13, .complete = FALSE)
    ) %>%
    ungroup()
  
  log_info("Lag features created")
  return(ts_data)
}

# Function to prepare data for specific models
prepare_model_data <- function(ts_data) {
  log_info("Preparing data for specific models")
  
  # Data for ARIMA model
  # ARIMA typically works with a single time series, so we'll aggregate by date
  arima_data <- ts_data %>%
    index_by(date) %>%
    summarise(total_sales = sum(sales_quantity, na.rm = TRUE)) %>%
    as_tsibble(index = date)
  
  # Data for Prophet model
  # Prophet requires columns named 'ds' (date) and 'y' (target variable)
  prophet_data <- ts_data %>%
    index_by(date) %>%
    summarise(total_sales = sum(sales_quantity, na.rm = TRUE)) %>%
    as_tibble() %>%
    rename(ds = date, y = total_sales)
  
  # Data for LSTM model
  # LSTM typically requires normalized data and specific formatting
  # This is a simplified version - actual implementation would be more complex
  lstm_data <- ts_data %>%
    index_by(date) %>%
    summarise(total_sales = sum(sales_quantity, na.rm = TRUE)) %>%
    as_tibble()
  
  # Return all prepared datasets
  result <- list(
    full_features = ts_data,
    arima_data = arima_data,
    prophet_data = prophet_data,
    lstm_data = lstm_data
  )
  
  log_info("Model-specific data preparation completed")
  return(result)
}

# Main function to engineer features
engineer_features <- function(cleaned_data) {
  log_info("Starting feature engineering for all data")
  
  # Get the cleaned sales data
  sales_data <- cleaned_data$sales
  
  # Create time-based features
  sales_with_time_features <- create_time_features(sales_data)
  
  # Create lag features
  sales_with_all_features <- create_lag_features(sales_with_time_features)
  
  # Prepare data for specific models
  model_ready_data <- prepare_model_data(sales_with_all_features)
  
  # Add the original cleaned data to the result
  model_ready_data$cleaned_data <- cleaned_data
  
  log_info("Feature engineering completed successfully")
  return(model_ready_data)
}

# If this script is run directly (not sourced), execute the main function
if (!interactive()) {
  # Load the cleaned data
  cleaned_data <- readRDS("data/processed/cleaned_data.rds")
  
  # Engineer features
  model_ready_data <- engineer_features(cleaned_data)
  
  # Save the feature-engineered data for the modeling step
  saveRDS(model_ready_data, "data/processed/model_ready_data.rds")
} 
# Helper functions for retail sales forecasting

# Load required libraries
library(dplyr)
library(lubridate)
library(logger)

#' Format a number with thousands separator and optional decimal places
#'
#' @param x The number to format
#' @param decimals Number of decimal places
#' @return A formatted string
format_number <- function(x, decimals = 0) {
  format(round(x, decimals), big.mark = ",", nsmall = decimals)
}

#' Format a currency value
#'
#' @param x The number to format
#' @param symbol Currency symbol
#' @param decimals Number of decimal places
#' @return A formatted string
format_currency <- function(x, symbol = "$", decimals = 2) {
  paste0(symbol, format_number(x, decimals))
}

#' Format a date
#'
#' @param date The date to format
#' @param format The format string
#' @return A formatted date string
format_date <- function(date, format = "%B %d, %Y") {
  format(as.Date(date), format)
}

#' Calculate percentage change
#'
#' @param new_value The new value
#' @param old_value The old value
#' @param decimals Number of decimal places
#' @return A formatted percentage string
calc_percent_change <- function(new_value, old_value, decimals = 1) {
  pct_change <- (new_value - old_value) / old_value * 100
  paste0(ifelse(pct_change >= 0, "+", ""), format(round(pct_change, decimals), nsmall = decimals), "%")
}

#' Get date range for a report
#'
#' @param period The period type ("daily", "weekly", "monthly", "quarterly", "yearly")
#' @param end_date The end date (defaults to today)
#' @return A list with start_date and end_date
get_report_date_range <- function(period = "weekly", end_date = Sys.Date()) {
  end_date <- as.Date(end_date)
  
  start_date <- case_when(
    period == "daily" ~ end_date - days(1),
    period == "weekly" ~ end_date - days(7),
    period == "monthly" ~ end_date - months(1),
    period == "quarterly" ~ end_date - months(3),
    period == "yearly" ~ end_date - years(1),
    TRUE ~ end_date - days(7)  # Default to weekly
  )
  
  return(list(start_date = start_date, end_date = end_date))
}

#' Calculate mean absolute percentage error (MAPE)
#'
#' @param actual The actual values
#' @param predicted The predicted values
#' @return The MAPE value
calc_mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

#' Calculate root mean square error (RMSE)
#'
#' @param actual The actual values
#' @param predicted The predicted values
#' @return The RMSE value
calc_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

#' Calculate mean absolute error (MAE)
#'
#' @param actual The actual values
#' @param predicted The predicted values
#' @return The MAE value
calc_mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

#' Create a color palette for charts
#'
#' @param n Number of colors needed
#' @param type Type of palette ("sequential", "diverging", "qualitative")
#' @return A vector of color codes
get_color_palette <- function(n, type = "qualitative") {
  if (type == "sequential") {
    # Blue sequential palette
    return(colorRampPalette(c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C"))(n))
  } else if (type == "diverging") {
    # Red-Blue diverging palette
    return(colorRampPalette(c("#B2182B", "#EF8A62", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#67A9CF", "#2166AC"))(n))
  } else {
    # Qualitative palette
    qual_colors <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")
    if (n <= length(qual_colors)) {
      return(qual_colors[1:n])
    } else {
      # If more colors are needed, repeat with slight adjustments
      return(colorRampPalette(qual_colors)(n))
    }
  }
}

#' Check if all required data is available
#'
#' @param required_files Vector of required file paths
#' @return TRUE if all files exist, FALSE otherwise
check_data_availability <- function(required_files = c(
  "data/processed/model_ready_data.rds",
  "data/processed/arima_results.rds",
  "data/processed/prophet_results.rds",
  "data/processed/lstm_results.rds",
  "data/processed/inventory_recommendations.rds"
)) {
  all(file.exists(required_files))
}

#' Load all model data
#'
#' @return A list with all data and model results
load_all_data <- function() {
  tryCatch({
    # Check if all required files exist
    if (!check_data_availability()) {
      log_error("Missing required data files")
      return(NULL)
    }
    
    # Load all data
    model_ready_data <- readRDS("data/processed/model_ready_data.rds")
    arima_results <- readRDS("data/processed/arima_results.rds")
    prophet_results <- readRDS("data/processed/prophet_results.rds")
    lstm_results <- readRDS("data/processed/lstm_results.rds")
    inventory_recommendations <- readRDS("data/processed/inventory_recommendations.rds")
    
    # Return all data as a list
    return(list(
      model_ready_data = model_ready_data,
      arima_results = arima_results,
      prophet_results = prophet_results,
      lstm_results = lstm_results,
      inventory_recommendations = inventory_recommendations
    ))
  }, error = function(e) {
    log_error(paste("Error loading data:", e$message))
    return(NULL)
  })
}

#' Calculate ensemble forecast from multiple models
#'
#' @param arima_forecast ARIMA forecast values
#' @param prophet_forecast Prophet forecast values
#' @param lstm_forecast LSTM forecast values
#' @param weights Weights for each model (must sum to 1)
#' @return Ensemble forecast values
calculate_ensemble_forecast <- function(
  arima_forecast,
  prophet_forecast,
  lstm_forecast,
  weights = c(arima = 0.33, prophet = 0.33, lstm = 0.34)
) {
  # Check weights sum to 1
  if (abs(sum(weights) - 1) > 0.001) {
    log_warn("Weights do not sum to 1, normalizing")
    weights <- weights / sum(weights)
  }
  
  # Calculate weighted average
  ensemble_forecast <- (
    arima_forecast * weights["arima"] +
    prophet_forecast * weights["prophet"] +
    lstm_forecast * weights["lstm"]
  )
  
  return(ensemble_forecast)
} 
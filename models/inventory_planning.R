# Inventory Planning Script
# This script generates inventory recommendations based on sales forecasts

# Load required libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(logger)

# Initialize logging
log_appender(appender_file("logs/inventory_planning.log"))
log_info("Starting inventory planning process")

# Function to calculate safety stock
calculate_safety_stock <- function(forecast_data, service_level = 0.95, lead_time_days) {
  log_info(paste("Calculating safety stock with service level", service_level))
  
  # Calculate forecast standard deviation
  forecast_sd <- sd(forecast_data$forecast)
  
  # Calculate safety factor (z-score) based on service level
  # For 95% service level, z = 1.645
  # For 99% service level, z = 2.326
  if (service_level == 0.95) {
    z_score <- 1.645
  } else if (service_level == 0.99) {
    z_score <- 2.326
  } else {
    # For other service levels, approximate using the normal distribution
    z_score <- qnorm(service_level)
  }
  
  # Calculate safety stock
  # Safety Stock = Z * σ * √(Lead Time)
  safety_stock <- z_score * forecast_sd * sqrt(lead_time_days)
  
  log_info(paste("Safety stock calculated:", round(safety_stock, 2)))
  return(safety_stock)
}

# Function to calculate reorder point
calculate_reorder_point <- function(forecast_data, safety_stock, lead_time_days) {
  log_info("Calculating reorder point")
  
  # Calculate average daily demand
  avg_daily_demand <- mean(forecast_data$forecast)
  
  # Calculate reorder point
  # Reorder Point = (Average Daily Demand * Lead Time) + Safety Stock
  reorder_point <- (avg_daily_demand * lead_time_days) + safety_stock
  
  log_info(paste("Reorder point calculated:", round(reorder_point, 2)))
  return(reorder_point)
}

# Function to calculate economic order quantity (EOQ)
calculate_eoq <- function(annual_demand, ordering_cost, holding_cost_percentage, unit_cost) {
  log_info("Calculating economic order quantity (EOQ)")
  
  # Calculate holding cost per unit
  holding_cost <- unit_cost * holding_cost_percentage
  
  # Calculate EOQ
  # EOQ = √(2 * Annual Demand * Ordering Cost / Holding Cost)
  eoq <- sqrt((2 * annual_demand * ordering_cost) / holding_cost)
  
  log_info(paste("EOQ calculated:", round(eoq, 2)))
  return(eoq)
}

# Function to generate inventory recommendations
generate_inventory_recommendations <- function(forecast_results, product_data, forecast_horizon = 7) {
  log_info("Generating inventory recommendations")
  
  # Extract forecasts from each model
  arima_forecast <- forecast_results$arima$forecast
  prophet_forecast <- forecast_results$prophet$forecast
  lstm_forecast <- forecast_results$lstm$forecast
  
  # Create a combined forecast (simple average of all models)
  # First, create a common date range
  start_date <- max(forecast_results$arima$input_data$date) + days(1)
  end_date <- start_date + days(forecast_horizon - 1)
  forecast_dates <- seq(start_date, end_date, by = "day")
  
  # Extract forecast values
  arima_values <- as.numeric(arima_forecast$mean)[1:forecast_horizon]
  
  prophet_values <- prophet_forecast %>%
    filter(ds > max(forecast_results$prophet$input_data$ds)) %>%
    head(forecast_horizon) %>%
    pull(yhat)
  
  lstm_values <- lstm_forecast %>%
    head(forecast_horizon) %>%
    pull(forecast)
  
  # Calculate ensemble forecast (average of all models)
  ensemble_forecast <- data.frame(
    date = forecast_dates,
    arima_forecast = arima_values,
    prophet_forecast = prophet_values,
    lstm_forecast = lstm_values
  ) %>%
    mutate(ensemble_forecast = (arima_forecast + prophet_forecast + lstm_forecast) / 3)
  
  # Generate recommendations for each product
  recommendations <- list()
  
  for (i in 1:nrow(product_data)) {
    product <- product_data[i, ]
    product_id <- product$product_id
    
    log_info(paste("Generating recommendations for product:", product_id))
    
    # Get product-specific parameters
    lead_time_days <- product$lead_time_days
    min_order_quantity <- product$min_order_quantity
    
    # Assume some default values for cost parameters
    # In a real application, these would come from the product data
    ordering_cost <- 50  # Cost to place an order
    holding_cost_percentage <- 0.25  # 25% of unit cost per year
    unit_cost <- 10  # Cost per unit
    
    # Calculate safety stock
    safety_stock <- calculate_safety_stock(
      ensemble_forecast,
      service_level = 0.95,
      lead_time_days = lead_time_days
    )
    
    # Calculate reorder point
    reorder_point <- calculate_reorder_point(
      ensemble_forecast,
      safety_stock,
      lead_time_days
    )
    
    # Calculate annual demand (extrapolate from forecast)
    daily_demand <- mean(ensemble_forecast$ensemble_forecast)
    annual_demand <- daily_demand * 365
    
    # Calculate EOQ
    eoq <- calculate_eoq(
      annual_demand,
      ordering_cost,
      holding_cost_percentage,
      unit_cost
    )
    
    # Adjust order quantity to meet minimum order quantity
    order_quantity <- max(eoq, min_order_quantity)
    
    # Store recommendations
    recommendations[[product_id]] <- list(
      product_id = product_id,
      product_name = product$product_name,
      lead_time_days = lead_time_days,
      safety_stock = safety_stock,
      reorder_point = reorder_point,
      economic_order_quantity = eoq,
      recommended_order_quantity = order_quantity,
      min_order_quantity = min_order_quantity,
      forecast_horizon = forecast_horizon,
      daily_forecast = ensemble_forecast$ensemble_forecast,
      forecast_dates = forecast_dates
    )
  }
  
  log_info("Inventory recommendations generated")
  return(list(
    recommendations = recommendations,
    ensemble_forecast = ensemble_forecast
  ))
}

# Main function to run inventory planning
run_inventory_planning <- function(forecast_results, model_ready_data, forecast_horizon = 7) {
  log_info("Starting inventory planning process")
  
  # Get product data
  product_data <- model_ready_data$cleaned_data$products
  
  # Generate inventory recommendations
  inventory_recommendations <- generate_inventory_recommendations(
    forecast_results,
    product_data,
    forecast_horizon
  )
  
  log_info("Inventory planning process completed")
  return(inventory_recommendations)
}

# If this script is run directly (not sourced), execute the main function
if (!interactive()) {
  # Load the model-ready data and forecast results
  model_ready_data <- readRDS("data/processed/model_ready_data.rds")
  arima_results <- readRDS("data/processed/arima_results.rds")
  prophet_results <- readRDS("data/processed/prophet_results.rds")
  lstm_results <- readRDS("data/processed/lstm_results.rds")
  
  # Combine forecast results
  forecast_results <- list(
    arima = arima_results,
    prophet = prophet_results,
    lstm = lstm_results
  )
  
  # Run inventory planning
  inventory_recommendations <- run_inventory_planning(
    forecast_results,
    model_ready_data
  )
  
  # Save the inventory recommendations
  saveRDS(inventory_recommendations, "data/processed/inventory_recommendations.rds")
} 
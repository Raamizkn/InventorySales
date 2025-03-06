# Prophet Model Script
# This script trains a Prophet model and generates forecasts

# Load required libraries
library(prophet)
library(dplyr)
library(lubridate)
library(ggplot2)
library(logger)

# Initialize logging
log_appender(appender_file("logs/prophet_model.log"))
log_info("Starting Prophet model training and forecasting")

# Function to train Prophet model
train_prophet_model <- function(prophet_data, forecast_horizon = 7) {
  log_info("Training Prophet model")
  
  # Create a Prophet model
  log_info("Creating Prophet model with daily seasonality")
  prophet_model <- prophet(
    prophet_data,
    daily.seasonality = TRUE,
    weekly.seasonality = TRUE,
    yearly.seasonality = FALSE,  # Not enough data for yearly seasonality
    seasonality.mode = "multiplicative"  # Multiplicative seasonality often works well for retail
  )
  
  # Create future dataframe for forecasting
  log_info(paste("Creating future dataframe for", forecast_horizon, "days"))
  future <- make_future_dataframe(prophet_model, periods = forecast_horizon)
  
  # Generate forecasts
  log_info("Generating forecasts")
  prophet_forecast <- predict(prophet_model, future)
  
  # Return model and forecast
  result <- list(
    model = prophet_model,
    forecast = prophet_forecast,
    input_data = prophet_data
  )
  
  log_info("Prophet model training and forecasting completed")
  return(result)
}

# Function to evaluate Prophet model
evaluate_prophet_model <- function(prophet_data, test_start_date) {
  log_info("Evaluating Prophet model with holdout data")
  
  # Convert test_start_date to Date if it's not already
  if (!inherits(test_start_date, "Date")) {
    test_start_date <- as.Date(test_start_date)
  }
  
  # Split data into training and test sets
  train_data <- prophet_data %>%
    filter(ds < test_start_date)
  
  test_data <- prophet_data %>%
    filter(ds >= test_start_date)
  
  # Train model on training data
  prophet_model <- prophet(
    train_data,
    daily.seasonality = TRUE,
    weekly.seasonality = TRUE,
    yearly.seasonality = FALSE,
    seasonality.mode = "multiplicative"
  )
  
  # Create future dataframe for the test period
  forecast_horizon <- nrow(test_data)
  future <- make_future_dataframe(prophet_model, periods = forecast_horizon)
  
  # Generate forecasts
  prophet_forecast <- predict(prophet_model, future)
  
  # Extract forecasts for the test period
  test_forecasts <- prophet_forecast %>%
    filter(ds >= test_start_date)
  
  # Calculate evaluation metrics
  actual_values <- test_data$y
  predicted_values <- test_forecasts$yhat
  
  # RMSE (Root Mean Square Error)
  rmse <- sqrt(mean((actual_values - predicted_values)^2))
  
  # MAE (Mean Absolute Error)
  mae <- mean(abs(actual_values - predicted_values))
  
  # MAPE (Mean Absolute Percentage Error)
  mape <- mean(abs((actual_values - predicted_values) / actual_values)) * 100
  
  # Log evaluation metrics
  log_info(paste("Prophet model evaluation metrics:"))
  log_info(paste("RMSE:", rmse))
  log_info(paste("MAE:", mae))
  log_info(paste("MAPE:", mape, "%"))
  
  # Return evaluation results
  evaluation <- list(
    model = prophet_model,
    forecast = prophet_forecast,
    test_forecasts = test_forecasts,
    actual = actual_values,
    predicted = predicted_values,
    metrics = list(
      rmse = rmse,
      mae = mae,
      mape = mape
    )
  )
  
  log_info("Prophet model evaluation completed")
  return(evaluation)
}

# Function to plot Prophet forecast
plot_prophet_forecast <- function(prophet_results) {
  log_info("Plotting Prophet forecast")
  
  # Extract model and forecast
  prophet_model <- prophet_results$model
  prophet_forecast <- prophet_results$forecast
  
  # Create the plot
  forecast_plot <- prophet_plot_components(prophet_model, prophet_forecast)
  
  # Plot the forecast
  plot(prophet_model, prophet_forecast)
  
  log_info("Prophet forecast plotting completed")
  return(forecast_plot)
}

# Main function to run Prophet modeling
run_prophet_modeling <- function(model_ready_data, forecast_horizon = 7, evaluate = TRUE) {
  log_info("Starting Prophet modeling process")
  
  # Get Prophet-ready data
  prophet_data <- model_ready_data$prophet_data
  
  # Train model and generate forecast
  prophet_results <- train_prophet_model(prophet_data, forecast_horizon)
  
  # Evaluate model if requested
  if (evaluate) {
    # Use the last 7 days as test data
    test_start_date <- max(prophet_data$ds) - days(7)
    evaluation_results <- evaluate_prophet_model(prophet_data, test_start_date)
    prophet_results$evaluation <- evaluation_results
  }
  
  # Plot the forecast
  prophet_results$plots <- plot_prophet_forecast(prophet_results)
  
  log_info("Prophet modeling process completed")
  return(prophet_results)
}

# If this script is run directly (not sourced), execute the main function
if (!interactive()) {
  # Load the model-ready data
  model_ready_data <- readRDS("data/processed/model_ready_data.rds")
  
  # Run Prophet modeling
  prophet_results <- run_prophet_modeling(model_ready_data)
  
  # Save the Prophet model results
  saveRDS(prophet_results, "data/processed/prophet_results.rds")
} 
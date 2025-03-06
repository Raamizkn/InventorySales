# ARIMA Model Script
# This script trains an ARIMA model and generates forecasts

# Load required libraries
library(forecast)
library(dplyr)
library(tsibble)
library(fable)
library(feasts)
library(logger)

# Initialize logging
log_appender(appender_file("logs/arima_model.log"))
log_info("Starting ARIMA model training and forecasting")

# Function to train ARIMA model
train_arima_model <- function(arima_data, forecast_horizon = 7) {
  log_info("Training ARIMA model")
  
  # Convert to ts object for forecast package
  ts_data <- as.ts(arima_data$total_sales)
  
  # Automatically select the best ARIMA model
  log_info("Fitting auto.arima model")
  arima_model <- auto.arima(ts_data, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
  
  # Model summary
  model_summary <- capture.output(summary(arima_model))
  log_info(paste("ARIMA model summary:", paste(model_summary, collapse = "\n")))
  
  # Perform model diagnostics
  log_info("Performing model diagnostics")
  
  # Ljung-Box test for autocorrelation in residuals
  lb_test <- Box.test(residuals(arima_model), lag = 10, type = "Ljung-Box")
  log_info(paste("Ljung-Box test p-value:", lb_test$p.value))
  
  if (lb_test$p.value < 0.05) {
    log_warn("Ljung-Box test indicates residuals may still contain autocorrelation")
  } else {
    log_info("Ljung-Box test indicates residuals are white noise (good)")
  }
  
  # Generate forecasts
  log_info(paste("Generating", forecast_horizon, "day forecast"))
  arima_forecast <- forecast(arima_model, h = forecast_horizon)
  
  # Return model and forecast
  result <- list(
    model = arima_model,
    forecast = arima_forecast,
    diagnostics = list(
      ljung_box = lb_test
    )
  )
  
  log_info("ARIMA model training and forecasting completed")
  return(result)
}

# Function to evaluate ARIMA model
evaluate_arima_model <- function(arima_data, test_start_date) {
  log_info("Evaluating ARIMA model with holdout data")
  
  # Split data into training and test sets
  train_data <- arima_data %>%
    filter(date < test_start_date) %>%
    as_tibble()
  
  test_data <- arima_data %>%
    filter(date >= test_start_date) %>%
    as_tibble()
  
  # Convert training data to ts object
  ts_train <- as.ts(train_data$total_sales)
  
  # Train model on training data
  arima_model <- auto.arima(ts_train)
  
  # Generate forecasts for test period
  forecast_horizon <- nrow(test_data)
  arima_forecast <- forecast(arima_model, h = forecast_horizon)
  
  # Calculate evaluation metrics
  actual_values <- test_data$total_sales
  predicted_values <- as.numeric(arima_forecast$mean)
  
  # RMSE (Root Mean Square Error)
  rmse <- sqrt(mean((actual_values - predicted_values)^2))
  
  # MAE (Mean Absolute Error)
  mae <- mean(abs(actual_values - predicted_values))
  
  # MAPE (Mean Absolute Percentage Error)
  mape <- mean(abs((actual_values - predicted_values) / actual_values)) * 100
  
  # Log evaluation metrics
  log_info(paste("ARIMA model evaluation metrics:"))
  log_info(paste("RMSE:", rmse))
  log_info(paste("MAE:", mae))
  log_info(paste("MAPE:", mape, "%"))
  
  # Return evaluation results
  evaluation <- list(
    model = arima_model,
    forecast = arima_forecast,
    actual = actual_values,
    predicted = predicted_values,
    metrics = list(
      rmse = rmse,
      mae = mae,
      mape = mape
    )
  )
  
  log_info("ARIMA model evaluation completed")
  return(evaluation)
}

# Main function to run ARIMA modeling
run_arima_modeling <- function(model_ready_data, forecast_horizon = 7, evaluate = TRUE) {
  log_info("Starting ARIMA modeling process")
  
  # Get ARIMA-ready data
  arima_data <- model_ready_data$arima_data
  
  # Train model and generate forecast
  arima_results <- train_arima_model(arima_data, forecast_horizon)
  
  # Evaluate model if requested
  if (evaluate) {
    # Use the last 7 days as test data
    test_start_date <- max(arima_data$date) - days(7)
    evaluation_results <- evaluate_arima_model(arima_data, test_start_date)
    arima_results$evaluation <- evaluation_results
  }
  
  log_info("ARIMA modeling process completed")
  return(arima_results)
}

# If this script is run directly (not sourced), execute the main function
if (!interactive()) {
  # Load the model-ready data
  model_ready_data <- readRDS("data/processed/model_ready_data.rds")
  
  # Run ARIMA modeling
  arima_results <- run_arima_modeling(model_ready_data)
  
  # Save the ARIMA model results
  saveRDS(arima_results, "data/processed/arima_results.rds")
} 
# LSTM Model Script
# This script trains an LSTM model and generates forecasts

# Load required libraries
library(keras)
library(tensorflow)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(logger)

# Initialize logging
log_appender(appender_file("logs/lstm_model.log"))
log_info("Starting LSTM model training and forecasting")

# Function to create sequences for LSTM
create_sequences <- function(data, seq_length) {
  log_info(paste("Creating sequences with length", seq_length))
  
  # Extract the target variable
  x <- data$total_sales
  
  # Create sequences
  n_samples <- length(x) - seq_length
  
  # Initialize matrices
  X <- matrix(0, nrow = n_samples, ncol = seq_length)
  y <- numeric(n_samples)
  
  # Fill matrices
  for (i in 1:n_samples) {
    X[i, ] <- x[i:(i + seq_length - 1)]
    y[i] <- x[i + seq_length]
  }
  
  # Return as list
  result <- list(X = X, y = y)
  
  log_info(paste("Created", n_samples, "sequences"))
  return(result)
}

# Function to normalize data
normalize_data <- function(data) {
  log_info("Normalizing data")
  
  # Calculate mean and standard deviation
  data_mean <- mean(data)
  data_sd <- sd(data)
  
  # Normalize data
  normalized_data <- (data - data_mean) / data_sd
  
  # Return normalized data and normalization parameters
  result <- list(
    normalized_data = normalized_data,
    mean = data_mean,
    sd = data_sd
  )
  
  log_info("Data normalization completed")
  return(result)
}

# Function to denormalize data
denormalize_data <- function(normalized_data, mean, sd) {
  denormalized_data <- normalized_data * sd + mean
  return(denormalized_data)
}

# Function to train LSTM model
train_lstm_model <- function(lstm_data, seq_length = 7, epochs = 50, batch_size = 32) {
  log_info("Training LSTM model")
  
  # Extract the target variable
  target_data <- lstm_data$total_sales
  
  # Normalize data
  norm_result <- normalize_data(target_data)
  normalized_data <- norm_result$normalized_data
  
  # Create sequences
  sequences <- create_sequences(data.frame(total_sales = normalized_data), seq_length)
  
  # Split data into training and validation sets (80/20 split)
  train_size <- floor(0.8 * nrow(sequences$X))
  
  X_train <- sequences$X[1:train_size, ]
  y_train <- sequences$y[1:train_size]
  
  X_val <- sequences$X[(train_size + 1):nrow(sequences$X), ]
  y_val <- sequences$y[(train_size + 1):nrow(sequences$X)]
  
  # Reshape input data for LSTM [samples, time steps, features]
  X_train <- array_reshape(X_train, c(nrow(X_train), seq_length, 1))
  X_val <- array_reshape(X_val, c(nrow(X_val), seq_length, 1))
  
  # Define LSTM model
  log_info("Defining LSTM model architecture")
  model <- keras_model_sequential() %>%
    layer_lstm(units = 50, return_sequences = TRUE, input_shape = c(seq_length, 1)) %>%
    layer_dropout(rate = 0.2) %>%
    layer_lstm(units = 50, return_sequences = FALSE) %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 1)
  
  # Compile model
  model %>% compile(
    loss = "mean_squared_error",
    optimizer = optimizer_adam(learning_rate = 0.001),
    metrics = c("mean_absolute_error")
  )
  
  # Print model summary
  summary(model)
  
  # Train model
  log_info(paste("Training LSTM model for", epochs, "epochs"))
  history <- model %>% fit(
    X_train, y_train,
    epochs = epochs,
    batch_size = batch_size,
    validation_data = list(X_val, y_val),
    verbose = 1
  )
  
  # Return model and training information
  result <- list(
    model = model,
    history = history,
    normalization = norm_result,
    seq_length = seq_length
  )
  
  log_info("LSTM model training completed")
  return(result)
}

# Function to generate forecasts with LSTM model
generate_lstm_forecast <- function(lstm_model_results, lstm_data, forecast_horizon = 7) {
  log_info(paste("Generating", forecast_horizon, "day forecast with LSTM model"))
  
  # Extract model and parameters
  model <- lstm_model_results$model
  norm_result <- lstm_model_results$normalization
  seq_length <- lstm_model_results$seq_length
  
  # Get the last sequence from the data
  target_data <- lstm_data$total_sales
  normalized_data <- (target_data - norm_result$mean) / norm_result$sd
  
  # Use the last seq_length values as input for forecasting
  last_sequence <- tail(normalized_data, seq_length)
  
  # Initialize forecast array
  forecast <- numeric(forecast_horizon)
  
  # Generate forecasts one step at a time
  current_sequence <- last_sequence
  
  for (i in 1:forecast_horizon) {
    # Reshape for prediction
    X <- array_reshape(current_sequence, c(1, seq_length, 1))
    
    # Predict next value
    next_value <- model %>% predict(X)
    
    # Store forecast
    forecast[i] <- next_value
    
    # Update sequence for next prediction
    current_sequence <- c(current_sequence[-1], next_value)
  }
  
  # Denormalize forecasts
  denormalized_forecast <- denormalize_data(forecast, norm_result$mean, norm_result$sd)
  
  # Create forecast dataframe
  last_date <- max(lstm_data$date)
  forecast_dates <- seq(last_date + days(1), by = "day", length.out = forecast_horizon)
  
  forecast_df <- data.frame(
    date = forecast_dates,
    forecast = denormalized_forecast,
    lower_bound = denormalized_forecast * 0.9,  # Simple confidence interval (10% below)
    upper_bound = denormalized_forecast * 1.1   # Simple confidence interval (10% above)
  )
  
  log_info("LSTM forecast generation completed")
  return(forecast_df)
}

# Function to evaluate LSTM model
evaluate_lstm_model <- function(lstm_data, test_start_date, seq_length = 7, epochs = 50, batch_size = 32) {
  log_info("Evaluating LSTM model with holdout data")
  
  # Split data into training and test sets
  train_data <- lstm_data %>%
    filter(date < test_start_date)
  
  test_data <- lstm_data %>%
    filter(date >= test_start_date)
  
  # Train model on training data
  lstm_model_results <- train_lstm_model(
    train_data,
    seq_length = seq_length,
    epochs = epochs,
    batch_size = batch_size
  )
  
  # Generate forecasts for test period
  forecast_horizon <- nrow(test_data)
  
  # Extract model and parameters
  model <- lstm_model_results$model
  norm_result <- lstm_model_results$normalization
  
  # Get the last sequence from the training data
  target_data <- train_data$total_sales
  normalized_data <- (target_data - norm_result$mean) / norm_result$sd
  
  # Use the last seq_length values as input for forecasting
  last_sequence <- tail(normalized_data, seq_length)
  
  # Initialize forecast array
  forecast <- numeric(forecast_horizon)
  
  # Generate forecasts one step at a time
  current_sequence <- last_sequence
  
  for (i in 1:forecast_horizon) {
    # Reshape for prediction
    X <- array_reshape(current_sequence, c(1, seq_length, 1))
    
    # Predict next value
    next_value <- model %>% predict(X)
    
    # Store forecast
    forecast[i] <- next_value
    
    # Update sequence for next prediction
    current_sequence <- c(current_sequence[-1], next_value)
  }
  
  # Denormalize forecasts
  denormalized_forecast <- denormalize_data(forecast, norm_result$mean, norm_result$sd)
  
  # Calculate evaluation metrics
  actual_values <- test_data$total_sales
  predicted_values <- denormalized_forecast
  
  # RMSE (Root Mean Square Error)
  rmse <- sqrt(mean((actual_values - predicted_values)^2))
  
  # MAE (Mean Absolute Error)
  mae <- mean(abs(actual_values - predicted_values))
  
  # MAPE (Mean Absolute Percentage Error)
  mape <- mean(abs((actual_values - predicted_values) / actual_values)) * 100
  
  # Log evaluation metrics
  log_info(paste("LSTM model evaluation metrics:"))
  log_info(paste("RMSE:", rmse))
  log_info(paste("MAE:", mae))
  log_info(paste("MAPE:", mape, "%"))
  
  # Return evaluation results
  evaluation <- list(
    model = lstm_model_results$model,
    actual = actual_values,
    predicted = predicted_values,
    metrics = list(
      rmse = rmse,
      mae = mae,
      mape = mape
    )
  )
  
  log_info("LSTM model evaluation completed")
  return(evaluation)
}

# Main function to run LSTM modeling
run_lstm_modeling <- function(model_ready_data, forecast_horizon = 7, evaluate = TRUE) {
  log_info("Starting LSTM modeling process")
  
  # Get LSTM-ready data
  lstm_data <- model_ready_data$lstm_data
  
  # Train model
  lstm_model_results <- train_lstm_model(lstm_data)
  
  # Generate forecast
  lstm_forecast <- generate_lstm_forecast(lstm_model_results, lstm_data, forecast_horizon)
  
  # Evaluate model if requested
  if (evaluate) {
    # Use the last 7 days as test data
    test_start_date <- max(lstm_data$date) - days(7)
    evaluation_results <- evaluate_lstm_model(lstm_data, test_start_date)
    lstm_model_results$evaluation <- evaluation_results
  }
  
  # Add forecast to results
  lstm_model_results$forecast <- lstm_forecast
  
  log_info("LSTM modeling process completed")
  return(lstm_model_results)
}

# If this script is run directly (not sourced), execute the main function
if (!interactive()) {
  # Load the model-ready data
  model_ready_data <- readRDS("data/processed/model_ready_data.rds")
  
  # Run LSTM modeling
  lstm_results <- run_lstm_modeling(model_ready_data)
  
  # Save the LSTM model results
  saveRDS(lstm_results, "data/processed/lstm_results.rds")
} 
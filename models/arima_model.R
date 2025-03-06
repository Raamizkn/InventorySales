# ARIMA Model Script
# This script trains an ARIMA model and generates forecasts

# Load required libraries
library(forecast)
library(dplyr)
library(tsibble)
library(fable)
library(feasts)
library(logger)
library(lubridate)
library(parallel)

# Initialize logging
log_appender(appender_file("logs/arima_model.log"))
log_info("Starting ARIMA model training and forecasting")

# Function to perform grid search for optimal ARIMA parameters
grid_search_arima <- function(ts_data, max_p = 3, max_d = 2, max_q = 3, 
                             max_P = 2, max_D = 1, max_Q = 2, m = 7) {
  log_info(paste("Performing grid search for optimal ARIMA parameters with max p =", max_p,
                "max d =", max_d, "max q =", max_q))
  
  best_aic <- Inf
  best_params <- list(p = 0, d = 0, q = 0, P = 0, D = 0, Q = 0)
  
  # Use parallel processing if available
  cores <- min(parallel::detectCores() - 1, 4)  # Use at most 4 cores
  log_info(paste("Using", cores, "cores for parallel processing"))
  
  cl <- makeCluster(cores)
  on.exit(stopCluster(cl))
  
  # Export necessary variables to the cluster
  clusterExport(cl, c("ts_data", "m"), envir = environment())
  
  # Load required libraries on each node
  clusterEvalQ(cl, {
    library(forecast)
  })
  
  # Generate all parameter combinations
  param_grid <- expand.grid(
    p = 0:max_p,
    d = 0:max_d,
    q = 0:max_q,
    P = 0:max_P,
    D = 0:max_D,
    Q = 0:max_Q
  )
  
  # Remove invalid combinations (p=0, q=0) and (P=0, Q=0)
  param_grid <- param_grid[!(param_grid$p == 0 & param_grid$q == 0), ]
  param_grid <- param_grid[!(param_grid$P == 0 & param_grid$Q == 0), ]
  
  log_info(paste("Grid search with", nrow(param_grid), "parameter combinations"))
  
  # Function to evaluate a single ARIMA model
  evaluate_model <- function(params) {
    tryCatch({
      # Fit model with specific parameters
      model <- Arima(ts_data, order = c(params$p, params$d, params$q),
                     seasonal = list(order = c(params$P, params$D, params$Q), period = m))
      
      return(list(params = params, aic = model$aic, model = model))
    }, error = function(e) {
      # Return high AIC value if model fails
      return(list(params = params, aic = Inf, model = NULL))
    })
  }
  
  # Evaluate all parameter combinations in parallel
  results <- parLapply(cl, split(param_grid, 1:nrow(param_grid)), function(params) {
    evaluate_model(params)
  })
  
  # Find the best model
  for (res in results) {
    if (res$aic < best_aic) {
      best_aic <- res$aic
      best_params <- res$params
      best_model <- res$model
    }
  }
  
  log_info(paste("Best ARIMA parameters found: p =", best_params$p, "d =", best_params$d, 
                "q =", best_params$q, "P =", best_params$P, "D =", best_params$D, 
                "Q =", best_params$Q, "AIC =", best_aic))
  
  return(list(
    order = c(best_params$p, best_params$d, best_params$q),
    seasonal = list(order = c(best_params$P, best_params$D, best_params$Q), period = m),
    aic = best_aic
  ))
}

# Function to add external regressors to ARIMA model
add_external_regressors <- function(arima_data) {
  log_info("Adding external regressors to ARIMA model")
  
  # Extract time series data
  ts_data <- arima_data %>%
    as_tibble()
  
  # Create day of week regressor (one-hot encoded)
  ts_data <- ts_data %>%
    mutate(
      is_monday = as.numeric(wday(date) == 2),
      is_tuesday = as.numeric(wday(date) == 3),
      is_wednesday = as.numeric(wday(date) == 4),
      is_thursday = as.numeric(wday(date) == 5),
      is_friday = as.numeric(wday(date) == 6),
      is_saturday = as.numeric(wday(date) == 7),
      is_sunday = as.numeric(wday(date) == 1)
    )
  
  # Create month of year regressor (one-hot encoded)
  ts_data <- ts_data %>%
    mutate(
      month = month(date),
      is_month_1 = as.numeric(month == 1),
      is_month_2 = as.numeric(month == 2),
      is_month_3 = as.numeric(month == 3),
      is_month_4 = as.numeric(month == 4),
      is_month_5 = as.numeric(month == 5),
      is_month_6 = as.numeric(month == 6),
      is_month_7 = as.numeric(month == 7),
      is_month_8 = as.numeric(month == 8),
      is_month_9 = as.numeric(month == 9),
      is_month_10 = as.numeric(month == 10),
      is_month_11 = as.numeric(month == 11),
      is_month_12 = as.numeric(month == 12)
    )
  
  # Add Fourier terms for multiple seasonality
  ts_data <- ts_data %>%
    mutate(
      # Weekly seasonality
      fourier_w1 = sin(2 * pi * 1 * as.numeric(date) / 7),
      fourier_w2 = cos(2 * pi * 1 * as.numeric(date) / 7),
      fourier_w3 = sin(2 * pi * 2 * as.numeric(date) / 7),
      fourier_w4 = cos(2 * pi * 2 * as.numeric(date) / 7),
      
      # Monthly seasonality
      fourier_m1 = sin(2 * pi * 1 * as.numeric(date) / 30.44),
      fourier_m2 = cos(2 * pi * 1 * as.numeric(date) / 30.44)
    )
  
  # Create holiday flags (example: using major US holidays)
  # In a real application, you would use a more comprehensive holiday list
  us_holidays_2020 <- as.Date(c(
    "2020-01-01",  # New Year's Day
    "2020-01-20",  # Martin Luther King Jr. Day
    "2020-02-17",  # Presidents' Day
    "2020-05-25",  # Memorial Day
    "2020-07-04",  # Independence Day
    "2020-09-07",  # Labor Day
    "2020-10-12",  # Columbus Day
    "2020-11-11",  # Veterans Day
    "2020-11-26",  # Thanksgiving
    "2020-12-25"   # Christmas
  ))
  
  ts_data <- ts_data %>%
    mutate(
      is_holiday = as.numeric(date %in% us_holidays_2020),
      is_day_before_holiday = as.numeric(date %in% (us_holidays_2020 - days(1))),
      is_day_after_holiday = as.numeric(date %in% (us_holidays_2020 + days(1)))
    )
  
  # Create lag features
  ts_data <- ts_data %>%
    mutate(
      lag1 = lag(total_sales, 1),
      lag7 = lag(total_sales, 7),
      lag30 = lag(total_sales, 30)
    )
  
  # Remove rows with NA (due to lags)
  ts_data <- ts_data %>%
    filter(!is.na(lag30))
  
  # Return data frame with all external regressors
  return(ts_data)
}

# Function to train ARIMA model
train_arima_model <- function(arima_data, external_regressors = TRUE, forecast_horizon = 7, grid_search = TRUE) {
  log_info("Training ARIMA model")
  
  if (external_regressors) {
    # Add external regressors
    ts_data_with_regressors <- add_external_regressors(arima_data)
    
    # Extract the target variable
    ts_data <- ts_data_with_regressors$total_sales
    
    # Extract external regressors
    xreg_cols <- c(
      "is_monday", "is_tuesday", "is_wednesday", "is_thursday", "is_friday", "is_saturday", 
      "fourier_w1", "fourier_w2", "fourier_w3", "fourier_w4", 
      "fourier_m1", "fourier_m2",
      "is_holiday", "is_day_before_holiday", "is_day_after_holiday"
    )
    
    xreg <- as.matrix(ts_data_with_regressors[, xreg_cols])
    
    # Generate future regressors for forecasting
    last_date <- max(ts_data_with_regressors$date)
    future_dates <- seq(last_date + days(1), by = "day", length.out = forecast_horizon)
    
    future_xreg <- matrix(0, nrow = forecast_horizon, ncol = length(xreg_cols))
    colnames(future_xreg) <- xreg_cols
    
    for (i in 1:forecast_horizon) {
      date <- future_dates[i]
      
      # Day of week
      future_xreg[i, "is_monday"] <- as.numeric(wday(date) == 2)
      future_xreg[i, "is_tuesday"] <- as.numeric(wday(date) == 3)
      future_xreg[i, "is_wednesday"] <- as.numeric(wday(date) == 4)
      future_xreg[i, "is_thursday"] <- as.numeric(wday(date) == 5)
      future_xreg[i, "is_friday"] <- as.numeric(wday(date) == 6)
      future_xreg[i, "is_saturday"] <- as.numeric(wday(date) == 7)
      
      # Fourier terms
      future_xreg[i, "fourier_w1"] <- sin(2 * pi * 1 * as.numeric(date) / 7)
      future_xreg[i, "fourier_w2"] <- cos(2 * pi * 1 * as.numeric(date) / 7)
      future_xreg[i, "fourier_w3"] <- sin(2 * pi * 2 * as.numeric(date) / 7)
      future_xreg[i, "fourier_w4"] <- cos(2 * pi * 2 * as.numeric(date) / 7)
      
      future_xreg[i, "fourier_m1"] <- sin(2 * pi * 1 * as.numeric(date) / 30.44)
      future_xreg[i, "fourier_m2"] <- cos(2 * pi * 1 * as.numeric(date) / 30.44)
      
      # Holidays (assuming same holidays as in training data)
      future_xreg[i, "is_holiday"] <- as.numeric(date %in% us_holidays_2020)
      future_xreg[i, "is_day_before_holiday"] <- as.numeric(date %in% (us_holidays_2020 - days(1)))
      future_xreg[i, "is_day_after_holiday"] <- as.numeric(date %in% (us_holidays_2020 + days(1)))
    }
    
    log_info("Using ARIMAX model with external regressors")
  } else {
    # Convert to ts object for forecast package
    ts_data <- as.ts(arima_data$total_sales)
    xreg <- NULL
    future_xreg <- NULL
  }
  
  # Create a time series object
  if (!is.ts(ts_data)) {
    ts_data <- ts(ts_data, frequency = 7)  # Assuming weekly seasonality
  }
  
  # Find optimal parameters if grid search is enabled
  if (grid_search) {
    log_info("Performing grid search for optimal ARIMA parameters")
    best_params <- grid_search_arima(ts_data, max_p = 2, max_d = 1, max_q = 2, 
                                    max_P = 1, max_D = 1, max_Q = 1, m = 7)
    
    # Fit ARIMA model with optimal parameters
    log_info(paste("Fitting ARIMA model with optimal parameters: p =", best_params$order[1],
                  "d =", best_params$order[2], "q =", best_params$order[3],
                  "P =", best_params$seasonal$order[1], "D =", best_params$seasonal$order[2],
                  "Q =", best_params$seasonal$order[3]))
    
    arima_model <- Arima(ts_data, 
                        order = best_params$order,
                        seasonal = best_params$seasonal,
                        xreg = xreg)
  } else {
    # Automatically select the best ARIMA model
    log_info("Fitting auto.arima model")
    arima_model <- auto.arima(ts_data, 
                             seasonal = TRUE, 
                             stepwise = FALSE,
                             approximation = FALSE,
                             xreg = xreg)
  }
  
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
  
  # Check if residuals are normally distributed
  shapiro_test <- shapiro.test(residuals(arima_model))
  log_info(paste("Shapiro-Wilk test p-value:", shapiro_test$p.value))
  
  if (shapiro_test$p.value < 0.05) {
    log_warn("Shapiro-Wilk test indicates residuals may not be normally distributed")
  } else {
    log_info("Shapiro-Wilk test indicates residuals are normally distributed (good)")
  }
  
  # Generate forecasts
  log_info(paste("Generating", forecast_horizon, "day forecast"))
  arima_forecast <- forecast(arima_model, h = forecast_horizon, xreg = future_xreg)
  
  # Return model and forecast
  result <- list(
    model = arima_model,
    forecast = arima_forecast,
    diagnostics = list(
      ljung_box = lb_test,
      shapiro = shapiro_test
    ),
    input_data = arima_data
  )
  
  log_info("ARIMA model training and forecasting completed")
  return(result)
}

# Function to perform time series cross-validation
cross_validate_arima <- function(arima_data, k = 5, h = 7, external_regressors = TRUE) {
  log_info(paste("Performing", k, "fold time series cross-validation"))
  
  # Prepare data
  if (external_regressors) {
    ts_data_with_regressors <- add_external_regressors(arima_data)
    
    # Extract the target variable and dates
    dates <- ts_data_with_regressors$date
    ts_data <- ts_data_with_regressors$total_sales
    
    # Extract external regressors
    xreg_cols <- c(
      "is_monday", "is_tuesday", "is_wednesday", "is_thursday", "is_friday", "is_saturday", 
      "fourier_w1", "fourier_w2", "fourier_w3", "fourier_w4", 
      "fourier_m1", "fourier_m2",
      "is_holiday", "is_day_before_holiday", "is_day_after_holiday"
    )
    
    xreg <- as.matrix(ts_data_with_regressors[, xreg_cols])
  } else {
    # Convert to ts object for forecast package
    dates <- arima_data$date
    ts_data <- as.ts(arima_data$total_sales)
    xreg <- NULL
  }
  
  # Total number of observations
  n <- length(ts_data)
  
  # Size of each fold
  fold_size <- floor(n / k)
  
  # Initialize error metrics
  rmse_values <- numeric(k)
  mae_values <- numeric(k)
  mape_values <- numeric(k)
  
  # Perform k-fold cross-validation
  for (i in 1:k) {
    log_info(paste("Cross-validation fold", i, "of", k))
    
    # Define the test set for this fold
    test_start <- n - (k - i + 1) * fold_size + 1
    test_end <- min(n - (k - i) * fold_size, n)
    
    train_indices <- 1:(test_start - 1)
    test_indices <- test_start:test_end
    
    # Create training and test sets
    train_ts <- ts_data[train_indices]
    test_ts <- ts_data[test_indices]
    
    if (!is.null(xreg)) {
      train_xreg <- xreg[train_indices, ]
      test_xreg <- xreg[test_indices, ]
    } else {
      train_xreg <- NULL
      test_xreg <- NULL
    }
    
    # Train model on training set
    if (!is.null(train_xreg)) {
      model <- auto.arima(train_ts, xreg = train_xreg, seasonal = TRUE)
    } else {
      model <- auto.arima(train_ts, seasonal = TRUE)
    }
    
    # Generate forecasts for test set
    if (!is.null(test_xreg)) {
      predictions <- forecast(model, h = length(test_indices), xreg = test_xreg)$mean
    } else {
      predictions <- forecast(model, h = length(test_indices))$mean
    }
    
    # Calculate error metrics
    actuals <- test_ts
    rmse_values[i] <- sqrt(mean((actuals - predictions)^2))
    mae_values[i] <- mean(abs(actuals - predictions))
    mape_values[i] <- mean(abs((actuals - predictions) / actuals)) * 100
    
    log_info(paste("Fold", i, "RMSE:", rmse_values[i], "MAE:", mae_values[i], "MAPE:", mape_values[i]))
  }
  
  # Calculate average metrics
  avg_rmse <- mean(rmse_values)
  avg_mae <- mean(mae_values)
  avg_mape <- mean(mape_values)
  
  log_info(paste("Cross-validation results - Avg RMSE:", avg_rmse, "Avg MAE:", avg_mae, "Avg MAPE:", avg_mape))
  
  return(list(
    rmse = rmse_values,
    mae = mae_values,
    mape = mape_values,
    avg_rmse = avg_rmse,
    avg_mae = avg_mae,
    avg_mape = avg_mape
  ))
}

# Function to evaluate ARIMA model
evaluate_arima_model <- function(arima_data, test_start_date, external_regressors = TRUE) {
  log_info("Evaluating ARIMA model with holdout data")
  
  # Split data into training and test sets
  train_data <- arima_data %>%
    filter(date < test_start_date) %>%
    as_tibble()
  
  test_data <- arima_data %>%
    filter(date >= test_start_date) %>%
    as_tibble()
  
  # Train model with external regressors if specified
  if (external_regressors) {
    # Add external regressors to training data
    ts_data_with_regressors <- add_external_regressors(train_data)
    
    # Extract target variable
    ts_train <- ts_data_with_regressors$total_sales
    
    # Extract external regressors
    xreg_cols <- c(
      "is_monday", "is_tuesday", "is_wednesday", "is_thursday", "is_friday", "is_saturday", 
      "fourier_w1", "fourier_w2", "fourier_w3", "fourier_w4", 
      "fourier_m1", "fourier_m2",
      "is_holiday", "is_day_before_holiday", "is_day_after_holiday"
    )
    
    xreg_train <- as.matrix(ts_data_with_regressors[, xreg_cols])
    
    # Generate test regressors
    xreg_test <- matrix(0, nrow = nrow(test_data), ncol = length(xreg_cols))
    colnames(xreg_test) <- xreg_cols
    
    for (i in 1:nrow(test_data)) {
      date <- test_data$date[i]
      
      # Day of week
      xreg_test[i, "is_monday"] <- as.numeric(wday(date) == 2)
      xreg_test[i, "is_tuesday"] <- as.numeric(wday(date) == 3)
      xreg_test[i, "is_wednesday"] <- as.numeric(wday(date) == 4)
      xreg_test[i, "is_thursday"] <- as.numeric(wday(date) == 5)
      xreg_test[i, "is_friday"] <- as.numeric(wday(date) == 6)
      xreg_test[i, "is_saturday"] <- as.numeric(wday(date) == 7)
      
      # Fourier terms
      xreg_test[i, "fourier_w1"] <- sin(2 * pi * 1 * as.numeric(date) / 7)
      xreg_test[i, "fourier_w2"] <- cos(2 * pi * 1 * as.numeric(date) / 7)
      xreg_test[i, "fourier_w3"] <- sin(2 * pi * 2 * as.numeric(date) / 7)
      xreg_test[i, "fourier_w4"] <- cos(2 * pi * 2 * as.numeric(date) / 7)
      
      xreg_test[i, "fourier_m1"] <- sin(2 * pi * 1 * as.numeric(date) / 30.44)
      xreg_test[i, "fourier_m2"] <- cos(2 * pi * 1 * as.numeric(date) / 30.44)
      
      # Holidays (assuming same holidays as in training data)
      us_holidays_2020 <- as.Date(c(
        "2020-01-01", "2020-01-20", "2020-02-17", "2020-05-25", "2020-07-04",
        "2020-09-07", "2020-10-12", "2020-11-11", "2020-11-26", "2020-12-25"
      ))
      
      xreg_test[i, "is_holiday"] <- as.numeric(date %in% us_holidays_2020)
      xreg_test[i, "is_day_before_holiday"] <- as.numeric(date %in% (us_holidays_2020 - days(1)))
      xreg_test[i, "is_day_after_holiday"] <- as.numeric(date %in% (us_holidays_2020 + days(1)))
    }
    
    # Train model on training data with external regressors
    arima_model <- auto.arima(ts_train, xreg = xreg_train, seasonal = TRUE)
    
    # Generate forecasts for test period with external regressors
    forecast_horizon <- nrow(test_data)
    arima_forecast <- forecast(arima_model, h = forecast_horizon, xreg = xreg_test)
  } else {
    # Convert training data to ts object
    ts_train <- as.ts(train_data$total_sales)
    
    # Train model on training data
    arima_model <- auto.arima(ts_train)
    
    # Generate forecasts for test period
    forecast_horizon <- nrow(test_data)
    arima_forecast <- forecast(arima_model, h = forecast_horizon)
  }
  
  # Calculate evaluation metrics
  actual_values <- test_data$total_sales
  predicted_values <- as.numeric(arima_forecast$mean)
  
  # RMSE (Root Mean Square Error)
  rmse <- sqrt(mean((actual_values - predicted_values)^2))
  
  # MAE (Mean Absolute Error)
  mae <- mean(abs(actual_values - predicted_values))
  
  # MAPE (Mean Absolute Percentage Error)
  mape <- mean(abs((actual_values - predicted_values) / actual_values)) * 100
  
  # SMAPE (Symmetric Mean Absolute Percentage Error)
  smape <- 200 * mean(abs(actual_values - predicted_values) / (abs(actual_values) + abs(predicted_values)))
  
  # R-squared
  ss_total <- sum((actual_values - mean(actual_values))^2)
  ss_residual <- sum((actual_values - predicted_values)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  # Log evaluation metrics
  log_info(paste("ARIMA model evaluation metrics:"))
  log_info(paste("RMSE:", rmse))
  log_info(paste("MAE:", mae))
  log_info(paste("MAPE:", mape, "%"))
  log_info(paste("SMAPE:", smape, "%"))
  log_info(paste("R-squared:", r_squared))
  
  # Return evaluation results
  evaluation <- list(
    model = arima_model,
    forecast = arima_forecast,
    actual = actual_values,
    predicted = predicted_values,
    metrics = list(
      rmse = rmse,
      mae = mae,
      mape = mape,
      smape = smape,
      r_squared = r_squared
    )
  )
  
  log_info("ARIMA model evaluation completed")
  return(evaluation)
}

# Main function to run ARIMA modeling
run_arima_modeling <- function(model_ready_data, forecast_horizon = 7, evaluate = TRUE, 
                               external_regressors = TRUE, grid_search = TRUE,
                               cross_validate = TRUE) {
  log_info("Starting ARIMA modeling process")
  
  # Get ARIMA-ready data
  arima_data <- model_ready_data$arima_data
  
  # Perform cross-validation if requested
  if (cross_validate) {
    cv_results <- cross_validate_arima(arima_data, k = 5, h = 7, external_regressors = external_regressors)
    log_info(paste("Cross-validation results - Avg RMSE:", cv_results$avg_rmse, 
                  "Avg MAE:", cv_results$avg_mae, "Avg MAPE:", cv_results$avg_mape))
  }
  
  # Train model and generate forecast
  arima_results <- train_arima_model(arima_data, 
                                    external_regressors = external_regressors,
                                    forecast_horizon = forecast_horizon,
                                    grid_search = grid_search)
  
  # Evaluate model if requested
  if (evaluate) {
    # Use the last 7 days as test data
    test_start_date <- max(arima_data$date) - days(7)
    evaluation_results <- evaluate_arima_model(arima_data, test_start_date, external_regressors = external_regressors)
    arima_results$evaluation <- evaluation_results
  }
  
  # Add cross-validation results if performed
  if (cross_validate) {
    arima_results$cross_validation <- cv_results
  }
  
  log_info("ARIMA modeling process completed")
  return(arima_results)
}

# If this script is run directly (not sourced), execute the main function
if (!interactive()) {
  # Load the model-ready data
  model_ready_data <- readRDS("data/processed/model_ready_data.rds")
  
  # Run ARIMA modeling with enhanced features
  arima_results <- run_arima_modeling(
    model_ready_data,
    external_regressors = TRUE,  # Use external regressors
    grid_search = TRUE,          # Use grid search for optimal parameters
    cross_validate = TRUE        # Perform cross-validation
  )
  
  # Save the ARIMA model results
  saveRDS(arima_results, "data/processed/arima_results.rds")
} 
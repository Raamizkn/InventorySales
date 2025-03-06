# Prophet Model Script
# This script trains a Prophet model and generates forecasts

# Load required libraries
library(prophet)
library(dplyr)
library(lubridate)
library(ggplot2)
library(logger)
library(parallel)

# Initialize logging
log_appender(appender_file("logs/prophet_model.log"))
log_info("Starting Prophet model training and forecasting")

# Function to add additional regressors to Prophet model data
add_prophet_regressors <- function(prophet_data, include_holidays = TRUE) {
  log_info("Adding additional regressors to Prophet model")
  
  # Create a copy of the input data
  enhanced_data <- prophet_data
  
  # Add temperature effect (simulated in this example)
  # In a real application, you would use actual temperature data
  set.seed(123)  # For reproducibility
  enhanced_data$temperature <- rnorm(nrow(enhanced_data), mean = 20, sd = 5) +  # Base temperature around 20°C
    5 * sin(2 * pi * (as.numeric(enhanced_data$ds) - as.numeric(as.Date("2020-01-01"))) / 365)  # Seasonal variation
  
  # Add price effect (simulated in this example)
  # In a real application, you would use actual price data
  enhanced_data$price_index <- 100 + cumsum(rnorm(nrow(enhanced_data), mean = 0, sd = 0.1))
  
  # Add promotion effect (simulated in this example)
  # In a real application, you would use actual promotion data
  enhanced_data$promotion <- sample(c(0, 1), nrow(enhanced_data), replace = TRUE, prob = c(0.9, 0.1))
  
  # Add day of week effect
  enhanced_data$day_of_week <- lubridate::wday(enhanced_data$ds)
  
  # Add weekend indicator
  enhanced_data$is_weekend <- as.numeric(enhanced_data$day_of_week %in% c(1, 7))
  
  # Add month of year
  enhanced_data$month <- lubridate::month(enhanced_data$ds)
  
  # Add quarter
  enhanced_data$quarter <- lubridate::quarter(enhanced_data$ds)
  
  # Add special events (simulated)
  # In a real application, you would use actual event data
  special_events <- as.Date(c(
    "2020-01-15",  # Special sale event
    "2020-02-14",  # Valentine's Day
    "2020-04-10",  # Easter
    "2020-07-15",  # Summer sale
    "2020-11-27"   # Black Friday
  ))
  
  enhanced_data$special_event <- as.numeric(enhanced_data$ds %in% special_events)
  
  # Add growth rate (cumulative)
  enhanced_data$cumulative_day <- seq_len(nrow(enhanced_data))
  
  log_info("Additional regressors added to Prophet data")
  return(enhanced_data)
}

# Function to perform cross-validation for Prophet
cross_validate_prophet <- function(prophet_data, initial_days = 30, period_days = 7, horizon_days = 7, 
                                  cutoffs = NULL, parallel = TRUE, num_cores = NULL) {
  log_info("Performing cross-validation for Prophet model")
  
  # Setup parallel processing if requested
  if (parallel) {
    if (is.null(num_cores)) {
      num_cores <- min(parallel::detectCores() - 1, 4)  # Use at most 4 cores
    }
    log_info(paste("Using", num_cores, "cores for parallel processing"))
  } else {
    num_cores <- 1
    log_info("Using single core for cross-validation")
  }
  
  # Perform cross-validation
  log_info(paste("CV parameters: initial =", initial_days, "days, period =", period_days, "days, horizon =", horizon_days, "days"))
  
  cv_results <- prophet::cross_validation(
    model = prophet(),
    df = prophet_data,
    initial = initial_days,
    period = period_days,
    horizon = horizon_days,
    units = "days",
    cutoffs = cutoffs,
    parallel = parallel,
    n.cores = num_cores
  )
  
  # Compute performance metrics
  performance_metrics <- prophet::performance_metrics(cv_results)
  
  log_info("Cross-validation completed")
  
  # Print average metrics
  avg_rmse <- mean(performance_metrics$rmse)
  avg_mae <- mean(performance_metrics$mae)
  avg_mape <- mean(performance_metrics$mape) * 100  # Convert to percentage
  
  log_info(paste("Average RMSE:", avg_rmse))
  log_info(paste("Average MAE:", avg_mae))
  log_info(paste("Average MAPE:", avg_mape, "%"))
  
  return(list(
    cv_results = cv_results,
    performance_metrics = performance_metrics,
    avg_metrics = list(
      rmse = avg_rmse,
      mae = avg_mae,
      mape = avg_mape
    )
  ))
}

# Function to optimize hyperparameters
optimize_prophet_hyperparameters <- function(prophet_data, metric = "rmse", horizon_days = 7) {
  log_info("Optimizing Prophet hyperparameters")
  
  # Define hyperparameter grid
  changepoint_prior_scale_grid <- c(0.001, 0.01, 0.05, 0.1, 0.5)
  seasonality_prior_scale_grid <- c(0.01, 0.1, 1.0, 10.0)
  holidays_prior_scale_grid <- c(0.01, 0.1, 1.0, 10.0)
  seasonality_mode_grid <- c("additive", "multiplicative")
  
  # Initialize best parameters and metrics
  best_params <- list()
  best_metric_value <- Inf
  
  # Track all results
  all_results <- list()
  
  # Grid search
  for (changepoint_prior in changepoint_prior_scale_grid) {
    for (seasonality_prior in seasonality_prior_scale_grid) {
      for (holidays_prior in holidays_prior_scale_grid) {
        for (seasonality_mode in seasonality_mode_grid) {
          
          log_info(paste("Testing parameters: changepoint_prior =", changepoint_prior,
                        "seasonality_prior =", seasonality_prior,
                        "holidays_prior =", holidays_prior,
                        "seasonality_mode =", seasonality_mode))
          
          # Create and fit model with these parameters
          model <- prophet(
            prophet_data,
            changepoint.prior.scale = changepoint_prior,
            seasonality.prior.scale = seasonality_prior,
            holidays.prior.scale = holidays_prior,
            seasonality.mode = seasonality_mode,
            daily.seasonality = TRUE,
            weekly.seasonality = TRUE,
            yearly.seasonality = FALSE  # Not enough data for yearly seasonality
          )
          
          # Cross-validate with these parameters (using a simpler CV to save time)
          cv_results <- prophet::cross_validation(
            model = model,
            df = prophet_data,
            initial = 30,
            period = 7,
            horizon = horizon_days,
            units = "days"
          )
          
          # Calculate performance metrics
          performance <- prophet::performance_metrics(cv_results)
          
          # Get the average of the specified metric
          current_metric_value <- mean(performance[[metric]])
          
          # Store results
          result <- list(
            params = list(
              changepoint_prior_scale = changepoint_prior,
              seasonality_prior_scale = seasonality_prior,
              holidays_prior_scale = holidays_prior,
              seasonality_mode = seasonality_mode
            ),
            metrics = list(
              rmse = mean(performance$rmse),
              mae = mean(performance$mae),
              mape = mean(performance$mape) * 100  # Convert to percentage
            )
          )
          
          all_results <- c(all_results, list(result))
          
          # Update best parameters if current metric is better
          if (current_metric_value < best_metric_value) {
            best_metric_value <- current_metric_value
            best_params <- list(
              changepoint_prior_scale = changepoint_prior,
              seasonality_prior_scale = seasonality_prior,
              holidays_prior_scale = holidays_prior,
              seasonality_mode = seasonality_mode
            )
            
            log_info(paste("New best parameters found! Metric value:", current_metric_value))
          }
        }
      }
    }
  }
  
  log_info(paste("Hyperparameter optimization completed. Best", metric, "value:", best_metric_value))
  log_info(paste("Best parameters: changepoint_prior =", best_params$changepoint_prior_scale,
                "seasonality_prior =", best_params$seasonality_prior_scale,
                "holidays_prior =", best_params$holidays_prior_scale,
                "seasonality_mode =", best_params$seasonality_mode))
  
  return(list(
    best_params = best_params,
    best_metric_value = best_metric_value,
    all_results = all_results
  ))
}

# Function to train Prophet model
train_prophet_model <- function(prophet_data, forecast_horizon = 7, add_regressors = TRUE, optimize_params = TRUE) {
  log_info("Training Prophet model")
  
  # Add additional regressors if requested
  if (add_regressors) {
    enhanced_data <- add_prophet_regressors(prophet_data)
    log_info("Using enhanced Prophet data with additional regressors")
  } else {
    enhanced_data <- prophet_data
    log_info("Using basic Prophet data without additional regressors")
  }
  
  # Optimize hyperparameters if requested
  if (optimize_params) {
    log_info("Optimizing Prophet hyperparameters")
    optimization_results <- optimize_prophet_hyperparameters(enhanced_data, metric = "rmse", horizon_days = forecast_horizon)
    best_params <- optimization_results$best_params
    
    # Create a Prophet model with optimized parameters
    model_params <- best_params
    log_info(paste("Using optimized parameters: changepoint_prior =", model_params$changepoint_prior_scale,
                  "seasonality_prior =", model_params$seasonality_prior_scale,
                  "holidays_prior =", model_params$holidays_prior_scale,
                  "seasonality_mode =", model_params$seasonality_mode))
  } else {
    # Use default parameters
    model_params <- list(
      changepoint_prior_scale = 0.05,
      seasonality_prior_scale = 10.0,
      holidays_prior_scale = 10.0,
      seasonality_mode = "multiplicative"
    )
    optimization_results <- NULL
    log_info("Using default Prophet parameters")
  }
  
  # Create and customize the Prophet model
  log_info("Creating Prophet model with customized settings")
  
  # Set up holidays
  holidays <- data.frame(
    holiday = 'custom',
    ds = as.Date(c(
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
    )),
    lower_window = -1,  # Include day before holiday
    upper_window = 1    # Include day after holiday
  )
  
  # Create the Prophet model
  prophet_model <- prophet(
    enhanced_data,
    changepoint.prior.scale = model_params$changepoint_prior_scale,
    seasonality.prior.scale = model_params$seasonality_prior_scale,
    holidays.prior.scale = model_params$holidays_prior_scale,
    seasonality.mode = model_params$seasonality_mode,
    daily.seasonality = TRUE,
    weekly.seasonality = TRUE,
    yearly.seasonality = FALSE,  # Not enough data for yearly seasonality
    holidays = holidays
  )
  
  # Add custom seasonalities if needed (example for monthly pattern)
  prophet_model <- add_seasonality(
    prophet_model,
    name = 'monthly',
    period = 30.5,
    fourier.order = 5
  )
  
  # Add additional regressors if using enhanced data
  if (add_regressors) {
    prophet_model <- add_regressor(prophet_model, 'temperature')
    prophet_model <- add_regressor(prophet_model, 'price_index')
    prophet_model <- add_regressor(prophet_model, 'promotion')
    prophet_model <- add_regressor(prophet_model, 'is_weekend')
    prophet_model <- add_regressor(prophet_model, 'special_event')
  }
  
  # Fit the model
  log_info("Fitting Prophet model")
  prophet_model <- fit.prophet(prophet_model, enhanced_data)
  
  # Plot the components to inspect seasonality and trend
  log_info("Generating component plots")
  component_plot <- prophet_plot_components(prophet_model, prophet_model$history)
  
  # Create future dataframe for forecasting
  log_info(paste("Creating future dataframe for", forecast_horizon, "days"))
  
  if (add_regressors) {
    # For models with regressors, we need to specify future regressor values
    future <- make_future_dataframe(prophet_model, periods = forecast_horizon)
    
    # Add future regressor values (simple simulation - in practice, these would be estimated values)
    last_date <- max(enhanced_data$ds)
    future_dates <- seq(last_date + days(1), by = "day", length.out = forecast_horizon)
    
    # Temperature (continue seasonal pattern)
    future$temperature <- rnorm(nrow(future), mean = 20, sd = 5) +
      5 * sin(2 * pi * (as.numeric(future$ds) - as.numeric(as.Date("2020-01-01"))) / 365)
    
    # Price index (continue trend)
    last_price <- tail(enhanced_data$price_index, 1)
    future$price_index <- last_price + cumsum(rnorm(nrow(future), mean = 0, sd = 0.1))
    
    # Promotion (random future promotions)
    future$promotion <- sample(c(0, 1), nrow(future), replace = TRUE, prob = c(0.9, 0.1))
    
    # Weekend indicator
    future$day_of_week <- lubridate::wday(future$ds)
    future$is_weekend <- as.numeric(future$day_of_week %in% c(1, 7))
    
    # Special events (none in forecast horizon for this example)
    future$special_event <- 0
  } else {
    future <- make_future_dataframe(prophet_model, periods = forecast_horizon)
  }
  
  # Generate forecasts
  log_info("Generating forecasts")
  prophet_forecast <- predict(prophet_model, future)
  
  # Return model, forecast, and additional information
  result <- list(
    model = prophet_model,
    forecast = prophet_forecast,
    component_plot = component_plot,
    input_data = enhanced_data,
    optimization_results = optimization_results
  )
  
  log_info("Prophet model training and forecasting completed")
  return(result)
}

# Function to evaluate Prophet model
evaluate_prophet_model <- function(prophet_data, test_start_date, add_regressors = TRUE, optimize_params = TRUE) {
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
  
  # Train the model with training data
  if (add_regressors) {
    # Add regressors to training data
    enhanced_train_data <- add_prophet_regressors(train_data)
    
    # Optimize parameters if requested
    if (optimize_params) {
      optimization_results <- optimize_prophet_hyperparameters(enhanced_train_data, metric = "rmse", horizon_days = nrow(test_data))
      best_params <- optimization_results$best_params
    } else {
      best_params <- list(
        changepoint_prior_scale = 0.05,
        seasonality_prior_scale = 10.0,
        holidays_prior_scale = 10.0,
        seasonality_mode = "multiplicative"
      )
    }
    
    # Set up holidays
    holidays <- data.frame(
      holiday = 'custom',
      ds = as.Date(c(
        "2020-01-01", "2020-01-20", "2020-02-17", "2020-05-25", "2020-07-04",
        "2020-09-07", "2020-10-12", "2020-11-11", "2020-11-26", "2020-12-25"
      )),
      lower_window = -1,
      upper_window = 1
    )
    
    # Create and fit model
    prophet_model <- prophet(
      enhanced_train_data,
      changepoint.prior.scale = best_params$changepoint_prior_scale,
      seasonality.prior.scale = best_params$seasonality_prior_scale,
      holidays.prior.scale = best_params$holidays_prior_scale,
      seasonality.mode = best_params$seasonality_mode,
      daily.seasonality = TRUE,
      weekly.seasonality = TRUE,
      yearly.seasonality = FALSE,
      holidays = holidays
    )
    
    # Add custom seasonalities
    prophet_model <- add_seasonality(
      prophet_model,
      name = 'monthly',
      period = 30.5,
      fourier.order = 5
    )
    
    # Add regressors
    prophet_model <- add_regressor(prophet_model, 'temperature')
    prophet_model <- add_regressor(prophet_model, 'price_index')
    prophet_model <- add_regressor(prophet_model, 'promotion')
    prophet_model <- add_regressor(prophet_model, 'is_weekend')
    prophet_model <- add_regressor(prophet_model, 'special_event')
    
    # Fit the model
    prophet_model <- fit.prophet(prophet_model, enhanced_train_data)
    
    # Create future dataframe for test period
    forecast_horizon <- nrow(test_data)
    future <- make_future_dataframe(prophet_model, periods = forecast_horizon)
    
    # Add regressor values for test period (use actual values from test data)
    future_indices <- (nrow(future) - forecast_horizon + 1):nrow(future)
    
    # Map test data variables to future dataframe
    enhanced_test_data <- add_prophet_regressors(test_data)
    
    future$temperature[future_indices] <- enhanced_test_data$temperature
    future$price_index[future_indices] <- enhanced_test_data$price_index
    future$promotion[future_indices] <- enhanced_test_data$promotion
    future$is_weekend[future_indices] <- enhanced_test_data$is_weekend
    future$special_event[future_indices] <- enhanced_test_data$special_event
    
    # Generate forecasts
    prophet_forecast <- predict(prophet_model, future)
    
    # Extract forecasts for the test period
    test_forecasts <- prophet_forecast %>%
      filter(ds >= test_start_date)
    
  } else {
    # Simple model without regressors
    prophet_model <- prophet(
      train_data,
      daily.seasonality = TRUE,
      weekly.seasonality = TRUE,
      yearly.seasonality = FALSE,
      seasonality.mode = "multiplicative"
    )
    
    # Fit the model
    prophet_model <- fit.prophet(prophet_model, train_data)
    
    # Create future dataframe for the test period
    forecast_horizon <- nrow(test_data)
    future <- make_future_dataframe(prophet_model, periods = forecast_horizon)
    
    # Generate forecasts
    prophet_forecast <- predict(prophet_model, future)
    
    # Extract forecasts for the test period
    test_forecasts <- prophet_forecast %>%
      filter(ds >= test_start_date)
  }
  
  # Calculate evaluation metrics
  actual_values <- test_data$y
  predicted_values <- test_forecasts$yhat
  
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
  log_info(paste("Prophet model evaluation metrics:"))
  log_info(paste("RMSE:", rmse))
  log_info(paste("MAE:", mae))
  log_info(paste("MAPE:", mape, "%"))
  log_info(paste("SMAPE:", smape, "%"))
  log_info(paste("R-squared:", r_squared))
  
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
      mape = mape,
      smape = smape,
      r_squared = r_squared
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
  
  # Plot changepoints if available
  if (!is.null(prophet_model$changepoints)) {
    plot(prophet_model, prophet_forecast) +
      add_changepoints_to_plot(prophet_model)
  }
  
  log_info("Prophet forecast plotting completed")
  return(forecast_plot)
}

# Main function to run Prophet modeling
run_prophet_modeling <- function(model_ready_data, forecast_horizon = 7, evaluate = TRUE, 
                                add_regressors = TRUE, optimize_params = TRUE,
                                cross_validate = TRUE) {
  log_info("Starting Prophet modeling process")
  
  # Get Prophet-ready data
  prophet_data <- model_ready_data$prophet_data
  
  # Perform cross-validation if requested
  if (cross_validate) {
    log_info("Performing cross-validation")
    cv_results <- cross_validate_prophet(
      prophet_data, 
      initial_days = 20,  # Start with 20 days of data
      period_days = 5,    # Make a forecast every 5 days
      horizon_days = 7,   # Forecast 7 days ahead
      parallel = TRUE
    )
    
    log_info(paste("Cross-validation metrics - Avg RMSE:", cv_results$avg_metrics$rmse, 
                  "Avg MAE:", cv_results$avg_metrics$mae, 
                  "Avg MAPE:", cv_results$avg_metrics$mape, "%"))
  }
  
  # Train model and generate forecast
  prophet_results <- train_prophet_model(
    prophet_data, 
    forecast_horizon = forecast_horizon,
    add_regressors = add_regressors,
    optimize_params = optimize_params
  )
  
  # Evaluate model if requested
  if (evaluate) {
    # Use the last 7 days as test data
    test_start_date <- max(prophet_data$ds) - days(7)
    evaluation_results <- evaluate_prophet_model(
      prophet_data, 
      test_start_date,
      add_regressors = add_regressors,
      optimize_params = optimize_params
    )
    prophet_results$evaluation <- evaluation_results
  }
  
  # Add cross-validation results if performed
  if (cross_validate) {
    prophet_results$cross_validation <- cv_results
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
  
  # Run Prophet modeling with enhanced features
  prophet_results <- run_prophet_modeling(
    model_ready_data,
    add_regressors = TRUE,     # Add external regressors
    optimize_params = TRUE,    # Optimize hyperparameters
    cross_validate = TRUE      # Perform cross-validation
  )
  
  # Save the Prophet model results
  saveRDS(prophet_results, "data/processed/prophet_results.rds")
} 
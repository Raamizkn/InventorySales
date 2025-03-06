# Run the entire retail sales forecasting pipeline
# This script executes all the steps in sequence:
# 1. Data ingestion
# 2. Data cleaning
# 3. Feature engineering
# 4. Model training (ARIMA, Prophet, LSTM)
# 5. Inventory planning
# 6. Generate report

# Load required libraries
library(lubridate)
library(logger)

# Initialize logging
if (!dir.exists("logs")) {
  dir.create("logs", recursive = TRUE)
}
log_appender(appender_file("logs/pipeline.log"))
log_info("Starting retail sales forecasting pipeline")

# Create directories if they don't exist
if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

# Step 1: Data Ingestion
log_info("Step 1: Data Ingestion")
tryCatch({
  source("scripts/data_ingestion.R")
  log_success("Data ingestion completed successfully")
}, error = function(e) {
  log_error(paste("Error in data ingestion:", e$message))
  stop(paste("Error in data ingestion:", e$message))
})

# Step 2: Data Cleaning
log_info("Step 2: Data Cleaning")
tryCatch({
  source("scripts/data_cleaning.R")
  log_success("Data cleaning completed successfully")
}, error = function(e) {
  log_error(paste("Error in data cleaning:", e$message))
  stop(paste("Error in data cleaning:", e$message))
})

# Step 3: Feature Engineering
log_info("Step 3: Feature Engineering")
tryCatch({
  source("scripts/feature_engineering.R")
  log_success("Feature engineering completed successfully")
}, error = function(e) {
  log_error(paste("Error in feature engineering:", e$message))
  stop(paste("Error in feature engineering:", e$message))
})

# Step 4: Train ARIMA Model
log_info("Step 4: Train ARIMA Model")
tryCatch({
  source("models/arima_model.R")
  log_success("ARIMA model training completed successfully")
}, error = function(e) {
  log_error(paste("Error in ARIMA model training:", e$message))
  stop(paste("Error in ARIMA model training:", e$message))
})

# Step 5: Train Prophet Model
log_info("Step 5: Train Prophet Model")
tryCatch({
  source("models/prophet_model.R")
  log_success("Prophet model training completed successfully")
}, error = function(e) {
  log_error(paste("Error in Prophet model training:", e$message))
  stop(paste("Error in Prophet model training:", e$message))
})

# Step 6: Train LSTM Model
log_info("Step 6: Train LSTM Model")
tryCatch({
  source("models/lstm_model.R")
  log_success("LSTM model training completed successfully")
}, error = function(e) {
  log_error(paste("Error in LSTM model training:", e$message))
  stop(paste("Error in LSTM model training:", e$message))
})

# Step 7: Inventory Planning
log_info("Step 7: Inventory Planning")
tryCatch({
  source("models/inventory_planning.R")
  log_success("Inventory planning completed successfully")
}, error = function(e) {
  log_error(paste("Error in inventory planning:", e$message))
  stop(paste("Error in inventory planning:", e$message))
})

# Step 8: Generate Weekly Report
log_info("Step 8: Generate Weekly Report")
tryCatch({
  # Load required libraries for report generation
  library(rmarkdown)
  
  # Create reports directory if it doesn't exist
  if (!dir.exists("reports/output")) {
    dir.create("reports/output", recursive = TRUE)
  }
  
  # Generate report
  report_file <- "reports/weekly_forecast_report.Rmd"
  output_file <- paste0("reports/output/weekly_forecast_report_", format(Sys.Date(), "%Y-%m-%d"), ".html")
  
  rmarkdown::render(
    input = report_file,
    output_file = output_file,
    quiet = TRUE
  )
  
  log_success(paste("Report generated successfully:", output_file))
}, error = function(e) {
  log_error(paste("Error in report generation:", e$message))
  stop(paste("Error in report generation:", e$message))
})

log_info("Pipeline completed successfully")
cat("\nRetail sales forecasting pipeline completed successfully!\n")
cat(paste0("Report generated at: reports/output/weekly_forecast_report_", format(Sys.Date(), "%Y-%m-%d"), ".html\n"))
cat("You can now run the Shiny dashboard with: shiny::runApp('app')\n") 
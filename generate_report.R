# Report Generation Script
# This script generates the weekly forecast report

# Load required libraries
library(rmarkdown)
library(logger)

# Initialize logging
if (!dir.exists("logs")) {
  dir.create("logs", recursive = TRUE)
}
log_appender(appender_file("logs/report_generation.log"))
log_info("Starting report generation")

# Create reports output directory if it doesn't exist
if (!dir.exists("reports/output")) {
  dir.create("reports/output", recursive = TRUE)
}

# Generate weekly forecast report
tryCatch({
  # Define report parameters
  report_file <- "reports/weekly_forecast_report.Rmd"
  report_date <- format(Sys.Date(), "%Y-%m-%d")
  output_file <- paste0("reports/output/weekly_forecast_report_", report_date, ".html")
  
  # Check if all required data files exist
  required_files <- c(
    "data/processed/model_ready_data.rds",
    "data/processed/arima_results.rds",
    "data/processed/prophet_results.rds",
    "data/processed/lstm_results.rds",
    "data/processed/inventory_recommendations.rds"
  )
  
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0) {
    log_error(paste("Missing required data files:", paste(missing_files, collapse = ", ")))
    log_info("Running full pipeline to generate missing data files")
    
    # Run the full pipeline
    source("run_pipeline.R")
  }
  
  # Render the report
  log_info(paste("Rendering report to", output_file))
  
  rmarkdown::render(
    input = report_file,
    output_file = output_file,
    quiet = TRUE
  )
  
  # Optional: Email the report
  # This is a placeholder - in a real implementation, you would:
  # 1. Set up an email service (e.g., using the 'sendmailR' package)
  # 2. Define recipients
  # 3. Create an email with the report attached
  
  # Example (commented out):
  # library(sendmailR)
  # from <- "forecasting_system@company.com"
  # to <- c("manager@company.com", "inventory_team@company.com")
  # subject <- paste("Weekly Sales Forecast Report -", report_date)
  # body <- paste("Please find attached the weekly sales forecast report for", report_date)
  # sendmail(from, to, subject, body, control=list(smtpServer="SMTP_SERVER"), 
  #          attachments=c(output_file))
  
  log_success(paste("Report successfully generated at", output_file))
  cat(paste("Report successfully generated at", output_file, "\n"))
  
}, error = function(e) {
  log_error(paste("Error generating report:", e$message))
  cat(paste("Error generating report:", e$message, "\n"))
})

# Additional reporting functions could be added here, such as:
# - Executive summary reports
# - Inventory alerts reports
# - Model performance comparison reports 
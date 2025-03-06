# Scheduling script for the retail sales forecasting pipeline
# This script uses the cronR package to schedule regular runs of the pipeline

# Load required libraries
library(cronR)
library(logger)

# Initialize logging
if (!dir.exists("logs")) {
  dir.create("logs", recursive = TRUE)
}
log_appender(appender_file("logs/scheduler.log"))
log_info("Setting up pipeline scheduler")

# Get the current directory
current_dir <- getwd()

# Create the cron job for the pipeline
# Schedule it to run daily at 1:00 AM
pipeline_cmd <- cron_rscript(
  rscript = file.path(current_dir, "run_pipeline.R"),
  rscript_log = file.path(current_dir, "logs/cron_pipeline.log"),
  log_append = TRUE,
  workdir = current_dir
)

# Add the job to the crontab
cron_add(
  command = pipeline_cmd,
  frequency = "0 1 * * *",  # Daily at 1:00 AM
  id = "retail_sales_pipeline",
  description = "Daily run of the retail sales forecasting pipeline"
)

log_info("Pipeline scheduled to run daily at 1:00 AM")
cat("Pipeline scheduled to run daily at 1:00 AM\n")

# List all scheduled jobs
cat("\nCurrent scheduled jobs:\n")
cron_ls()

# Optional: create a weekly report job (runs every Monday at 6:00 AM)
report_cmd <- cron_rscript(
  rscript = file.path(current_dir, "generate_report.R"),
  rscript_log = file.path(current_dir, "logs/cron_report.log"),
  log_append = TRUE,
  workdir = current_dir
)

cron_add(
  command = report_cmd,
  frequency = "0 6 * * 1",  # Every Monday at 6:00 AM
  id = "weekly_forecast_report",
  description = "Weekly generation of the retail sales forecast report"
)

log_info("Weekly report scheduled to run every Monday at 6:00 AM")
cat("Weekly report scheduled to run every Monday at 6:00 AM\n")

# Instructions for removing scheduled jobs
cat("\nTo remove a scheduled job, use: cronR::cron_rm('job_id')\n")
cat("Example: cronR::cron_rm('retail_sales_pipeline')\n") 
# Main entry point for the Retail Sales and Inventory Forecasting Tool
# This script provides a simple command-line interface to run different components

# Load required libraries
library(logger)

# Initialize logging
if (!dir.exists("logs")) {
  dir.create("logs", recursive = TRUE)
}
log_appender(appender_file("logs/main.log"))
log_info("Starting main application")

# Function to display help menu
show_help <- function() {
  cat("\nRetail Sales and Inventory Forecasting Tool\n")
  cat("===========================================\n\n")
  cat("Usage: Rscript run.R [option]\n\n")
  cat("Options:\n")
  cat("  --pipeline        Run the full data pipeline (ETL, models, forecasting)\n")
  cat("  --dashboard       Launch the interactive Shiny dashboard\n")
  cat("  --report          Generate the weekly forecast report\n")
  cat("  --schedule        Set up automated scheduling of tasks\n")
  cat("  --help            Display this help message\n")
  cat("\nExamples:\n")
  cat("  Rscript run.R --pipeline    # Run the full data pipeline\n")
  cat("  Rscript run.R --dashboard   # Launch the dashboard\n\n")
}

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# If no arguments are provided, show help
if (length(args) == 0) {
  show_help()
  cat("No option specified. What would you like to do?\n")
  cat("1) Run the full pipeline\n")
  cat("2) Launch the dashboard\n")
  cat("3) Generate a report\n")
  cat("4) Schedule automated tasks\n")
  cat("5) Exit\n")
  
  choice <- readline("Enter your choice (1-5): ")
  
  if (choice == "1") {
    args <- "--pipeline"
  } else if (choice == "2") {
    args <- "--dashboard"
  } else if (choice == "3") {
    args <- "--report"
  } else if (choice == "4") {
    args <- "--schedule"
  } else {
    cat("Exiting. Goodbye!\n")
    quit(save = "no")
  }
}

# Process the selected option
if (args[1] == "--help") {
  # Show help message
  show_help()
  
} else if (args[1] == "--pipeline") {
  # Run the full pipeline
  cat("Running the full data pipeline...\n")
  log_info("Starting full pipeline")
  
  tryCatch({
    source("run_pipeline.R")
    cat("Pipeline completed successfully!\n")
  }, error = function(e) {
    log_error(paste("Error running pipeline:", e$message))
    cat(paste("Error running pipeline:", e$message, "\n"))
  })
  
} else if (args[1] == "--dashboard") {
  # Launch the dashboard
  cat("Launching the interactive dashboard...\n")
  log_info("Starting Shiny dashboard")
  
  # Check if shiny package is installed
  if (!requireNamespace("shiny", quietly = TRUE)) {
    cat("The shiny package is not installed. Installing now...\n")
    install.packages("shiny")
  }
  
  # Check if required data files exist
  required_files <- c(
    "data/processed/model_ready_data.rds",
    "data/processed/arima_results.rds",
    "data/processed/prophet_results.rds",
    "data/processed/lstm_results.rds",
    "data/processed/inventory_recommendations.rds"
  )
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0) {
    cat("Missing required data files. Running the pipeline first...\n")
    log_info("Missing data files, running pipeline")
    source("run_pipeline.R")
  }
  
  # Run the dashboard
  cat("Starting the dashboard...\n")
  shiny::runApp("app")
  
} else if (args[1] == "--report") {
  # Generate the report
  cat("Generating the weekly forecast report...\n")
  log_info("Starting report generation")
  
  tryCatch({
    source("generate_report.R")
    cat("Report generation completed successfully!\n")
  }, error = function(e) {
    log_error(paste("Error generating report:", e$message))
    cat(paste("Error generating report:", e$message, "\n"))
  })
  
} else if (args[1] == "--schedule") {
  # Set up scheduling
  cat("Setting up automated scheduling...\n")
  log_info("Setting up scheduling")
  
  # Check if cronR package is installed
  if (!requireNamespace("cronR", quietly = TRUE)) {
    cat("The cronR package is not installed. Installing now...\n")
    install.packages("cronR")
  }
  
  tryCatch({
    source("schedule_pipeline.R")
    cat("Scheduling set up successfully!\n")
  }, error = function(e) {
    log_error(paste("Error setting up scheduling:", e$message))
    cat(paste("Error setting up scheduling:", e$message, "\n"))
  })
  
} else {
  # Invalid option
  cat(paste("Invalid option:", args[1], "\n"))
  show_help()
}

log_info("Main application finished")
cat("\nGoodbye! Thank you for using the Retail Sales and Inventory Forecasting Tool.\n") 
# Install required packages for the Retail Sales and Inventory Forecasting Tool

# List of required packages
packages <- c(
  # Data manipulation and visualization
  "shiny", "shinydashboard", "dplyr", "tidyr", "ggplot2", "DT", "plotly",
  
  # Time series and date handling
  "lubridate", "tsibble", "feasts", "fable",
  
  # Forecasting models
  "forecast", "prophet", "keras", "tensorflow",
  
  # Data import/export
  "readr", "DBI", "RODBC",
  
  # Reporting
  "rmarkdown", "knitr", "flexdashboard",
  
  # Utilities
  "logger", "cronR", "future", "promises"
)

# Function to install packages if not already installed
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("Installing package: ", pkg, "\n"))
    install.packages(pkg)
  } else {
    cat(paste0("Package already installed: ", pkg, "\n"))
  }
}

# Install packages
cat("Installing required packages...\n")
for (pkg in packages) {
  install_if_missing(pkg)
}

# Special handling for tensorflow and keras
if (!requireNamespace("tensorflow", quietly = TRUE)) {
  install.packages("tensorflow")
  tensorflow::install_tensorflow()
}

if (!requireNamespace("keras", quietly = TRUE)) {
  install.packages("keras")
  keras::install_keras()
}

cat("Package installation complete!\n") 
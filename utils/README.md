# Utility Functions

This directory contains helper functions used throughout the Retail Sales and Inventory Forecasting tool.

## helpers.R

The `helpers.R` file contains various utility functions for formatting, date handling, error calculation, and other common operations used by multiple scripts in the project.

### Key Functions

- **Formatting Functions**
  - `format_number()`: Format numbers with thousands separators
  - `format_currency()`: Format currency values
  - `format_date()`: Format dates with customizable patterns

- **Date Handling**
  - `get_report_date_range()`: Calculate date ranges for reports (daily, weekly, monthly, etc.)

- **Error Metrics**
  - `calc_mape()`: Calculate Mean Absolute Percentage Error
  - `calc_rmse()`: Calculate Root Mean Square Error
  - `calc_mae()`: Calculate Mean Absolute Error

- **Data Loading**
  - `check_data_availability()`: Check if all required data files exist
  - `load_all_data()`: Load all model data and forecasts

- **Forecasting Utilities**
  - `calculate_ensemble_forecast()`: Create an ensemble forecast by combining multiple model forecasts

## Usage

To use these utility functions in your scripts, include the following code:

```R
# Source the helpers file
source("utils/helpers.R")

# Now you can use the functions
formatted_number <- format_number(1234567, 2)
formatted_date <- format_date(Sys.Date())
date_range <- get_report_date_range("monthly")
```

## Adding New Functions

When adding new utility functions to this directory:

1. Use proper documentation with roxygen-style comments
2. Include appropriate error handling
3. Add unit tests (if applicable)
4. Update this README with the new function's purpose 
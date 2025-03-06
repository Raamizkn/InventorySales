# Retail Sales and Inventory Forecasting Tool

An AI-driven forecasting tool for retail sales and inventory planning using R.

## Overview

This solution helps reduce stock shortages and optimize supply chain demand forecasting by accurately predicting retail sales. It not only forecasts future sales using multiple models (ARIMA, Prophet, and LSTM) but also translates those forecasts into actionable inventory recommendations.

## Key Components

- **Data Pipeline & ETL:** Ingest, clean, and transform raw retail sales data.
- **Modeling:** Build, validate, and compare ARIMA, Prophet, and LSTM models.
- **Forecasting & Inventory Planning:** Generate weekly sales forecasts and derive inventory recommendations.
- **Reporting & Visualization:** Automatically generate reports and host an interactive dashboard using R Shiny.
- **Automation & Deployment:** Automate the workflow and deploy on a scalable, secure server environment.

## Project Structure

```
/RetailForecastingProject
│
├── data/                  # Raw and processed retail sales data
│   ├── raw/               # Original CSV, database exports, etc.
│   └── processed/         # Cleaned and transformed data (tsibble objects)
│
├── scripts/               # ETL and data processing scripts
│   ├── data_ingestion.R
│   ├── data_cleaning.R
│   └── feature_engineering.R
│
├── models/                # Model training and evaluation scripts
│   ├── arima_model.R
│   ├── prophet_model.R
│   └── lstm_model.R
│
├── reports/               # RMarkdown files for automated weekly reports
│   └── weekly_forecast_report.Rmd
│
├── app/                   # R Shiny application code
│   ├── ui.R             # User interface definition
│   ├── server.R         # Server logic and reactive functions
│   └── modules/         # Shiny modules for dashboard components
│
├── utils/                 # Helper functions and logging utilities
│   └── helpers.R
│
└── README.md              # Project documentation and setup instructions
```

## Setup Instructions

1. Clone this repository
2. Install required R packages:
   ```R
   install.packages(c("shiny", "shinydashboard", "dplyr", "tidyr", "ggplot2", 
                     "lubridate", "forecast", "prophet", "keras", "tensorflow",
                     "tsibble", "feasts", "fable", "DT", "rmarkdown", "knitr",
                     "readr", "DBI", "logger", "cronR"))
   ```
3. Place your retail sales data in the `data/raw/` directory
4. Run the ETL scripts to process the data
5. Train the forecasting models
6. Launch the Shiny dashboard:
   ```R
   shiny::runApp("app")
   ```

## Features

- Multi-model forecasting (ARIMA, Prophet, LSTM)
- Interactive dashboard for exploring forecasts and inventory recommendations
- Automated reporting
- Inventory optimization based on sales forecasts

## License

MIT 
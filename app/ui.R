# UI for Retail Sales and Inventory Forecasting Dashboard

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

# Define UI
ui <- dashboardPage(
  # Dashboard header
  dashboardHeader(
    title = "Retail Forecasting",
    titleWidth = 300
  ),
  
  # Dashboard sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Sales Forecast", tabName = "forecast", icon = icon("chart-line")),
      menuItem("Inventory Planning", tabName = "inventory", icon = icon("boxes")),
      menuItem("Model Performance", tabName = "performance", icon = icon("chart-bar")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    ),
    
    # Filters
    br(),
    h4("Filters", style = "padding-left: 20px;"),
    
    # Date range filter
    dateRangeInput(
      "date_range",
      "Date Range:",
      start = Sys.Date() - 30,
      end = Sys.Date() + 7,
      min = "2020-01-01",
      max = Sys.Date() + 30
    ),
    
    # Product filter
    selectInput(
      "product",
      "Product:",
      choices = c("All Products" = "all"),
      selected = "all"
    ),
    
    # Store filter
    selectInput(
      "store",
      "Store:",
      choices = c("All Stores" = "all"),
      selected = "all"
    ),
    
    # Model selection
    radioButtons(
      "model",
      "Forecast Model:",
      choices = c(
        "ARIMA" = "arima",
        "Prophet" = "prophet",
        "LSTM" = "lstm",
        "Ensemble" = "ensemble"
      ),
      selected = "ensemble"
    ),
    
    # Forecast horizon
    sliderInput(
      "horizon",
      "Forecast Horizon (Days):",
      min = 1,
      max = 30,
      value = 7,
      step = 1
    ),
    
    # Action button to refresh forecasts
    actionButton(
      "refresh",
      "Refresh Forecasts",
      icon = icon("sync"),
      style = "margin: 20px; width: 80%;"
    )
  ),
  
  # Dashboard body
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
          border-radius: 5px;
        }
        .small-box {
          border-radius: 5px;
        }
      "))
    ),
    
    # Tab content
    tabItems(
      # Dashboard tab
      tabItem(
        tabName = "dashboard",
        fluidRow(
          # KPI boxes
          valueBoxOutput("total_sales_box", width = 3),
          valueBoxOutput("avg_daily_sales_box", width = 3),
          valueBoxOutput("forecast_accuracy_box", width = 3),
          valueBoxOutput("inventory_alerts_box", width = 3)
        ),
        
        fluidRow(
          # Sales trend chart
          box(
            title = "Sales Trend and Forecast",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            height = 400,
            plotlyOutput("sales_trend_plot", height = 350)
          ),
          
          # Inventory status
          box(
            title = "Inventory Status",
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            height = 400,
            DTOutput("inventory_status_table")
          )
        ),
        
        fluidRow(
          # Product performance
          box(
            title = "Product Performance",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            height = 400,
            plotlyOutput("product_performance_plot", height = 350)
          ),
          
          # Store performance
          box(
            title = "Store Performance",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            height = 400,
            plotlyOutput("store_performance_plot", height = 350)
          )
        )
      ),
      
      # Sales Forecast tab
      tabItem(
        tabName = "forecast",
        fluidRow(
          box(
            title = "Sales Forecast",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = 500,
            plotlyOutput("forecast_plot", height = 450)
          )
        ),
        
        fluidRow(
          box(
            title = "Forecast Details",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("forecast_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Forecast Decomposition",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            height = 500,
            plotlyOutput("decomposition_plot", height = 450)
          )
        )
      ),
      
      # Inventory Planning tab
      tabItem(
        tabName = "inventory",
        fluidRow(
          box(
            title = "Inventory Recommendations",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("inventory_recommendations_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Inventory Planning Chart",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = 500,
            plotlyOutput("inventory_planning_plot", height = 450)
          )
        ),
        
        fluidRow(
          box(
            title = "Safety Stock Analysis",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            height = 400,
            plotlyOutput("safety_stock_plot", height = 350)
          ),
          
          box(
            title = "Order Quantity Optimization",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            height = 400,
            plotlyOutput("order_quantity_plot", height = 350)
          )
        )
      ),
      
      # Model Performance tab
      tabItem(
        tabName = "performance",
        fluidRow(
          box(
            title = "Model Comparison",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = 500,
            plotlyOutput("model_comparison_plot", height = 450)
          )
        ),
        
        fluidRow(
          box(
            title = "Performance Metrics",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("performance_metrics_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Residual Analysis",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            height = 500,
            plotlyOutput("residual_plot", height = 450)
          )
        )
      ),
      
      # Settings tab
      tabItem(
        tabName = "settings",
        fluidRow(
          box(
            title = "Forecast Settings",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            # ARIMA settings
            conditionalPanel(
              condition = "input.model == 'arima'",
              h4("ARIMA Settings"),
              numericInput("arima_p", "AR Order (p):", 1, min = 0, max = 5),
              numericInput("arima_d", "Differencing (d):", 1, min = 0, max = 2),
              numericInput("arima_q", "MA Order (q):", 1, min = 0, max = 5),
              checkboxInput("arima_seasonal", "Include Seasonality", TRUE)
            ),
            
            # Prophet settings
            conditionalPanel(
              condition = "input.model == 'prophet'",
              h4("Prophet Settings"),
              checkboxInput("prophet_daily", "Daily Seasonality", TRUE),
              checkboxInput("prophet_weekly", "Weekly Seasonality", TRUE),
              checkboxInput("prophet_yearly", "Yearly Seasonality", FALSE),
              selectInput("prophet_seasonality", "Seasonality Mode:", 
                          choices = c("Additive" = "additive", "Multiplicative" = "multiplicative"),
                          selected = "multiplicative")
            ),
            
            # LSTM settings
            conditionalPanel(
              condition = "input.model == 'lstm'",
              h4("LSTM Settings"),
              numericInput("lstm_units", "LSTM Units:", 50, min = 10, max = 200),
              numericInput("lstm_epochs", "Training Epochs:", 50, min = 10, max = 200),
              numericInput("lstm_batch", "Batch Size:", 32, min = 8, max = 128),
              numericInput("lstm_sequence", "Sequence Length:", 7, min = 3, max = 30)
            ),
            
            # Ensemble settings
            conditionalPanel(
              condition = "input.model == 'ensemble'",
              h4("Ensemble Settings"),
              sliderInput("arima_weight", "ARIMA Weight:", min = 0, max = 1, value = 0.33, step = 0.01),
              sliderInput("prophet_weight", "Prophet Weight:", min = 0, max = 1, value = 0.33, step = 0.01),
              sliderInput("lstm_weight", "LSTM Weight:", min = 0, max = 1, value = 0.34, step = 0.01)
            ),
            
            # Apply settings button
            actionButton("apply_settings", "Apply Settings", icon = icon("check"))
          ),
          
          box(
            title = "Inventory Planning Settings",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            # Service level
            sliderInput("service_level", "Service Level:", min = 0.8, max = 0.99, value = 0.95, step = 0.01),
            
            # Holding cost percentage
            sliderInput("holding_cost", "Holding Cost (% of unit cost):", min = 0.1, max = 0.5, value = 0.25, step = 0.01),
            
            # Ordering cost
            numericInput("ordering_cost", "Ordering Cost ($):", 50, min = 10, max = 500),
            
            # Apply settings button
            actionButton("apply_inventory_settings", "Apply Settings", icon = icon("check"))
          )
        ),
        
        fluidRow(
          box(
            title = "Data Settings",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            # Data refresh frequency
            selectInput("refresh_frequency", "Data Refresh Frequency:",
                        choices = c("Daily" = "daily", "Weekly" = "weekly", "Manual" = "manual"),
                        selected = "daily"),
            
            # Data source
            selectInput("data_source", "Data Source:",
                        choices = c("CSV Files" = "csv", "Database" = "db"),
                        selected = "csv"),
            
            # Database connection settings (if applicable)
            conditionalPanel(
              condition = "input.data_source == 'db'",
              textInput("db_host", "Database Host:", "localhost"),
              textInput("db_name", "Database Name:", "retail_db"),
              textInput("db_user", "Username:", "user"),
              passwordInput("db_password", "Password:", ""),
              actionButton("test_connection", "Test Connection", icon = icon("database"))
            ),
            
            # Apply settings button
            actionButton("apply_data_settings", "Apply Settings", icon = icon("check"))
          )
        )
      )
    )
  )
) 
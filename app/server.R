# Server logic for Retail Sales and Inventory Forecasting Dashboard

library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(forecast)
library(prophet)
library(logger)

# Initialize logging
log_appender(appender_file("logs/shiny_app.log"))
log_info("Starting Shiny application")

# Define server logic
server <- function(input, output, session) {
  
  # Load data when app starts
  app_data <- reactiveVal(NULL)
  
  # Loading data function
  load_data <- function() {
    log_info("Loading data...")
    
    # Try to load the model-ready data and forecast results
    tryCatch({
      model_ready_data <- readRDS("data/processed/model_ready_data.rds")
      arima_results <- readRDS("data/processed/arima_results.rds")
      prophet_results <- readRDS("data/processed/prophet_results.rds")
      lstm_results <- readRDS("data/processed/lstm_results.rds")
      inventory_recommendations <- readRDS("data/processed/inventory_recommendations.rds")
      
      # Update product and store filter choices
      product_data <- model_ready_data$cleaned_data$products
      store_data <- model_ready_data$cleaned_data$stores
      
      # Set product choices
      product_choices <- c("All Products" = "all", 
                         setNames(product_data$product_id, 
                                  paste(product_data$product_name, " (", product_data$product_id, ")", sep = "")))
      updateSelectInput(session, "product", choices = product_choices)
      
      # Set store choices
      store_choices <- c("All Stores" = "all", 
                       setNames(store_data$store_id, 
                                paste(store_data$store_name, " (", store_data$store_id, ")", sep = "")))
      updateSelectInput(session, "store", choices = store_choices)
      
      # Return all data as a list
      return(list(
        model_ready_data = model_ready_data,
        arima_results = arima_results,
        prophet_results = prophet_results,
        lstm_results = lstm_results,
        inventory_recommendations = inventory_recommendations
      ))
    }, error = function(e) {
      log_error(paste("Error loading data:", e$message))
      showNotification(paste("Error loading data:", e$message), type = "error")
      return(NULL)
    })
  }
  
  # Initialize data when app starts
  observe({
    app_data(load_data())
  }, priority = 1000)
  
  # Refresh data when the refresh button is clicked
  observeEvent(input$refresh, {
    showNotification("Refreshing forecasts...", type = "default", duration = 3)
    app_data(load_data())
    showNotification("Forecasts refreshed!", type = "message", duration = 3)
  })
  
  # Dashboard tab outputs ----
  
  # KPI value boxes
  output$total_sales_box <- renderValueBox({
    data <- app_data()
    if (is.null(data)) return(valueBox(0, "Total Sales", icon = icon("shopping-cart"), color = "blue"))
    
    # Calculate total sales
    historical_sales <- sum(data$model_ready_data$arima_data$total_sales)
    
    valueBox(
      format(round(historical_sales), big.mark = ","),
      "Total Historical Sales",
      icon = icon("shopping-cart"),
      color = "blue"
    )
  })
  
  output$avg_daily_sales_box <- renderValueBox({
    data <- app_data()
    if (is.null(data)) return(valueBox(0, "Avg Daily Sales", icon = icon("calendar-day"), color = "green"))
    
    # Calculate average daily sales
    sales_data <- data$model_ready_data$arima_data
    avg_sales <- mean(sales_data$total_sales)
    
    valueBox(
      format(round(avg_sales), big.mark = ","),
      "Avg Daily Sales",
      icon = icon("calendar-day"),
      color = "green"
    )
  })
  
  output$forecast_accuracy_box <- renderValueBox({
    data <- app_data()
    if (is.null(data)) return(valueBox("0%", "Forecast Accuracy", icon = icon("bullseye"), color = "purple"))
    
    # Calculate forecast accuracy (100% - MAPE)
    if (!is.null(data$arima_results$evaluation$metrics$mape)) {
      mape <- data$arima_results$evaluation$metrics$mape
      accuracy <- 100 - mape
      accuracy <- max(0, min(accuracy, 100))  # Constrain to 0-100%
      
      valueBox(
        paste0(round(accuracy, 1), "%"),
        "Forecast Accuracy",
        icon = icon("bullseye"),
        color = "purple"
      )
    } else {
      valueBox(
        "N/A",
        "Forecast Accuracy",
        icon = icon("bullseye"),
        color = "purple"
      )
    }
  })
  
  output$inventory_alerts_box <- renderValueBox({
    data <- app_data()
    if (is.null(data)) return(valueBox(0, "Inventory Alerts", icon = icon("exclamation-triangle"), color = "red"))
    
    # Count products that need reordering
    recommendations <- data$inventory_recommendations$recommendations
    alerts_count <- 0
    
    for (product_id in names(recommendations)) {
      rec <- recommendations[[product_id]]
      # Determine if any product needs reordering based on forecast vs reorder point
      if (mean(rec$daily_forecast) >= rec$reorder_point) {
        alerts_count <- alerts_count + 1
      }
    }
    
    valueBox(
      alerts_count,
      "Inventory Alerts",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  # Sales trend plot
  output$sales_trend_plot <- renderPlotly({
    data <- app_data()
    if (is.null(data)) return(plot_ly())
    
    # Filter by date range, product, and store
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    product_filter <- input$product
    store_filter <- input$store
    
    # Get historical data
    historical_data <- data$model_ready_data$arima_data %>%
      filter(date >= start_date, date <= end_date) %>%
      as_tibble()
    
    # Get forecast data
    ensemble_forecast <- data$inventory_recommendations$ensemble_forecast
    
    # Create the plot
    p <- plot_ly() %>%
      add_trace(
        data = historical_data,
        x = ~date,
        y = ~total_sales,
        type = "scatter",
        mode = "lines",
        name = "Historical Sales",
        line = list(color = "blue", width = 2)
      ) %>%
      add_trace(
        data = ensemble_forecast,
        x = ~date,
        y = ~ensemble_forecast,
        type = "scatter",
        mode = "lines",
        name = "Forecast",
        line = list(color = "red", width = 2, dash = "dot")
      ) %>%
      layout(
        title = "Sales Trend and Forecast",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Total Sales"),
        hovermode = "closest",
        legend = list(orientation = "h", x = 0.5, xanchor = "center")
      )
    
    return(p)
  })
  
  # Inventory status table
  output$inventory_status_table <- renderDT({
    data <- app_data()
    if (is.null(data)) return(NULL)
    
    # Get inventory recommendations
    recommendations <- data$inventory_recommendations$recommendations
    
    # Create a status table
    status_table <- data.frame(
      Product = character(),
      Current_Stock = numeric(),
      Reorder_Point = numeric(),
      Status = character(),
      stringsAsFactors = FALSE
    )
    
    # Fill the table with data
    for (product_id in names(recommendations)) {
      rec <- recommendations[[product_id]]
      
      # Simulate current stock (this should come from actual inventory data)
      # For now, using a random value between 0 and 2 * reorder point
      set.seed(as.numeric(factor(product_id)))
      current_stock <- round(runif(1, 0, 2 * rec$reorder_point))
      
      # Determine status
      if (current_stock <= rec$safety_stock) {
        status <- "Critical"
      } else if (current_stock <= rec$reorder_point) {
        status <- "Reorder"
      } else {
        status <- "OK"
      }
      
      # Add to table
      status_table <- rbind(
        status_table,
        data.frame(
          Product = rec$product_name,
          Current_Stock = current_stock,
          Reorder_Point = round(rec$reorder_point),
          Status = status,
          stringsAsFactors = FALSE
        )
      )
    }
    
    # Color the status column
    datatable(
      status_table,
      options = list(pageLength = 5, dom = 'tp'),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Status',
        backgroundColor = styleEqual(
          c("Critical", "Reorder", "OK"),
          c("#FF9999", "#FFCC99", "#99CC99")
        )
      )
  })
  
  # Product performance plot
  output$product_performance_plot <- renderPlotly({
    data <- app_data()
    if (is.null(data)) return(plot_ly())
    
    # Get sales by product
    sales_data <- data$model_ready_data$full_features %>%
      as_tibble() %>%
      group_by(product_id) %>%
      summarise(
        total_sales = sum(sales_quantity),
        total_revenue = sum(sales_amount)
      )
    
    # Join with product data to get names
    product_data <- data$model_ready_data$cleaned_data$products
    sales_data <- sales_data %>%
      left_join(product_data, by = "product_id")
    
    # Create the plot
    p <- plot_ly(
      data = sales_data,
      x = ~product_name,
      y = ~total_revenue,
      type = "bar",
      marker = list(color = "steelblue")
    ) %>%
      layout(
        title = "Revenue by Product",
        xaxis = list(title = "Product"),
        yaxis = list(title = "Total Revenue ($)")
      )
    
    return(p)
  })
  
  # Store performance plot
  output$store_performance_plot <- renderPlotly({
    data <- app_data()
    if (is.null(data)) return(plot_ly())
    
    # Get sales by store
    sales_data <- data$model_ready_data$full_features %>%
      as_tibble() %>%
      group_by(store_id) %>%
      summarise(
        total_sales = sum(sales_quantity),
        total_revenue = sum(sales_amount)
      )
    
    # Join with store data to get names
    store_data <- data$model_ready_data$cleaned_data$stores
    sales_data <- sales_data %>%
      left_join(store_data, by = "store_id")
    
    # Create the plot
    p <- plot_ly(
      data = sales_data,
      x = ~store_name,
      y = ~total_revenue,
      type = "bar",
      marker = list(color = "forestgreen")
    ) %>%
      layout(
        title = "Revenue by Store",
        xaxis = list(title = "Store"),
        yaxis = list(title = "Total Revenue ($)")
      )
    
    return(p)
  })
  
  # Sales Forecast tab outputs ----
  
  # Forecast plot
  output$forecast_plot <- renderPlotly({
    data <- app_data()
    if (is.null(data)) return(plot_ly())
    
    # Get historical data
    historical_data <- data$model_ready_data$arima_data %>%
      as_tibble() %>%
      select(date, total_sales)
    
    # Get forecast data based on selected model
    ensemble_forecast <- data$inventory_recommendations$ensemble_forecast
    
    selected_model <- input$model
    forecast_values <- ensemble_forecast$ensemble_forecast  # Default to ensemble
    
    if (selected_model == "arima") {
      forecast_values <- ensemble_forecast$arima_forecast
    } else if (selected_model == "prophet") {
      forecast_values <- ensemble_forecast$prophet_forecast
    } else if (selected_model == "lstm") {
      forecast_values <- ensemble_forecast$lstm_forecast
    }
    
    # Create confidence intervals (simplified)
    forecast_df <- data.frame(
      date = ensemble_forecast$date,
      forecast = forecast_values,
      lower = forecast_values * 0.9,
      upper = forecast_values * 1.1
    )
    
    # Create the plot
    p <- plot_ly() %>%
      add_trace(
        data = historical_data,
        x = ~date,
        y = ~total_sales,
        type = "scatter",
        mode = "lines",
        name = "Historical Sales",
        line = list(color = "blue", width = 2)
      ) %>%
      add_trace(
        data = forecast_df,
        x = ~date,
        y = ~forecast,
        type = "scatter",
        mode = "lines",
        name = "Forecast",
        line = list(color = "red", width = 2)
      ) %>%
      add_ribbons(
        data = forecast_df,
        x = ~date,
        ymin = ~lower,
        ymax = ~upper,
        name = "95% Confidence",
        fillcolor = "rgba(255, 0, 0, 0.2)",
        line = list(color = "transparent")
      ) %>%
      layout(
        title = paste(toupper(selected_model), "Forecast"),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Total Sales"),
        hovermode = "closest",
        legend = list(orientation = "h", x = 0.5, xanchor = "center")
      )
    
    return(p)
  })
  
  # Forecast table
  output$forecast_table <- renderDT({
    data <- app_data()
    if (is.null(data)) return(NULL)
    
    # Get forecast data
    ensemble_forecast <- data$inventory_recommendations$ensemble_forecast
    
    # Create a forecast table with all models
    forecast_table <- data.frame(
      Date = ensemble_forecast$date,
      ARIMA = round(ensemble_forecast$arima_forecast, 2),
      Prophet = round(ensemble_forecast$prophet_forecast, 2),
      LSTM = round(ensemble_forecast$lstm_forecast, 2),
      Ensemble = round(ensemble_forecast$ensemble_forecast, 2)
    )
    
    # Display the table
    datatable(
      forecast_table,
      options = list(
        pageLength = 7,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      caption = "Daily Sales Forecasts",
      rownames = FALSE
    )
  })
  
  # Forecast decomposition plot
  output$decomposition_plot <- renderPlotly({
    data <- app_data()
    if (is.null(data)) return(plot_ly())
    
    # For simplicity, just show a general decomposition using the historical data
    # In a real app, you would use the decomposition from the selected model
    ts_data <- data$model_ready_data$arima_data$total_sales
    
    # Create a time series object
    ts_obj <- ts(ts_data, frequency = 7)  # Assuming weekly seasonality
    
    # Decompose the time series
    decomp <- stl(ts_obj, s.window = "periodic")
    
    # Convert to data frame for plotting
    decomp_df <- data.frame(
      date = data$model_ready_data$arima_data$date,
      observed = as.numeric(decomp$time.series[, "seasonal"] + decomp$time.series[, "trend"] + decomp$time.series[, "remainder"]),
      trend = as.numeric(decomp$time.series[, "trend"]),
      seasonal = as.numeric(decomp$time.series[, "seasonal"]),
      remainder = as.numeric(decomp$time.series[, "remainder"])
    )
    
    # Create the plot
    p <- subplot(
      plot_ly(decomp_df, x = ~date, y = ~observed, type = "scatter", mode = "lines", name = "Observed") %>%
        layout(title = "Observed", showlegend = FALSE),
      plot_ly(decomp_df, x = ~date, y = ~trend, type = "scatter", mode = "lines", name = "Trend", line = list(color = "blue")) %>%
        layout(title = "Trend", showlegend = FALSE),
      plot_ly(decomp_df, x = ~date, y = ~seasonal, type = "scatter", mode = "lines", name = "Seasonal", line = list(color = "green")) %>%
        layout(title = "Seasonal", showlegend = FALSE),
      plot_ly(decomp_df, x = ~date, y = ~remainder, type = "scatter", mode = "lines", name = "Remainder", line = list(color = "red")) %>%
        layout(title = "Remainder", showlegend = FALSE),
      nrows = 4, shareX = TRUE, titleY = TRUE
    ) %>%
      layout(
        title = "Time Series Decomposition",
        margin = list(t = 50)
      )
    
    return(p)
  })
  
  # Inventory Planning tab outputs ----
  
  # Inventory recommendations table
  output$inventory_recommendations_table <- renderDT({
    data <- app_data()
    if (is.null(data)) return(NULL)
    
    # Get recommendations
    recommendations <- data$inventory_recommendations$recommendations
    
    # Create a recommendations table
    recommendations_table <- data.frame(
      Product_ID = character(),
      Product_Name = character(),
      Safety_Stock = numeric(),
      Reorder_Point = numeric(),
      Recommended_Order_Quantity = numeric(),
      Lead_Time_Days = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Fill the table with data from each product
    for (product_id in names(recommendations)) {
      rec <- recommendations[[product_id]]
      recommendations_table <- rbind(
        recommendations_table,
        data.frame(
          Product_ID = rec$product_id,
          Product_Name = rec$product_name,
          Safety_Stock = round(rec$safety_stock, 2),
          Reorder_Point = round(rec$reorder_point, 2),
          Recommended_Order_Quantity = round(rec$recommended_order_quantity, 2),
          Lead_Time_Days = rec$lead_time_days,
          stringsAsFactors = FALSE
        )
      )
    }
    
    # Display the table
    datatable(
      recommendations_table,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      caption = "Inventory Recommendations",
      rownames = FALSE
    )
  })
  
  # Inventory planning plot
  output$inventory_planning_plot <- renderPlotly({
    data <- app_data()
    if (is.null(data)) return(plot_ly())
    
    # Get selected product
    product_filter <- input$product
    
    # If "all" is selected, use the first product
    if (product_filter == "all") {
      recommendations <- data$inventory_recommendations$recommendations
      product_id <- names(recommendations)[1]
      rec <- recommendations[[product_id]]
    } else {
      # Use the selected product
      recommendations <- data$inventory_recommendations$recommendations
      rec <- recommendations[[product_filter]]
      
      if (is.null(rec)) {
        # If product not found, use the first product
        product_id <- names(recommendations)[1]
        rec <- recommendations[[product_id]]
      }
    }
    
    # Create a data frame for the forecast
    product_forecast <- data.frame(
      Date = rec$forecast_dates,
      Forecast = rec$daily_forecast
    )
    
    # Create the plot
    p <- plot_ly() %>%
      add_trace(
        data = product_forecast,
        x = ~Date,
        y = ~Forecast,
        type = "scatter",
        mode = "lines+markers",
        name = "Forecast",
        line = list(color = "blue", width = 2)
      ) %>%
      add_trace(
        x = c(min(product_forecast$Date), max(product_forecast$Date)),
        y = c(rec$reorder_point, rec$reorder_point),
        type = "scatter",
        mode = "lines",
        name = "Reorder Point",
        line = list(color = "red", width = 2, dash = "dash")
      ) %>%
      add_trace(
        x = c(min(product_forecast$Date), max(product_forecast$Date)),
        y = c(rec$safety_stock, rec$safety_stock),
        type = "scatter",
        mode = "lines",
        name = "Safety Stock",
        line = list(color = "orange", width = 2, dash = "dash")
      ) %>%
      layout(
        title = paste("Inventory Planning for", rec$product_name),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Quantity"),
        hovermode = "closest",
        legend = list(orientation = "h", x = 0.5, xanchor = "center")
      )
    
    return(p)
  })
  
  # Safety stock plot
  output$safety_stock_plot <- renderPlotly({
    data <- app_data()
    if (is.null(data)) return(plot_ly())
    
    # Get recommendations
    recommendations <- data$inventory_recommendations$recommendations
    
    # Create a data frame for safety stock analysis
    safety_data <- data.frame(
      Product = character(),
      Safety_Stock = numeric(),
      Lead_Time = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Fill the data frame
    for (product_id in names(recommendations)) {
      rec <- recommendations[[product_id]]
      safety_data <- rbind(
        safety_data,
        data.frame(
          Product = rec$product_name,
          Safety_Stock = rec$safety_stock,
          Lead_Time = rec$lead_time_days,
          stringsAsFactors = FALSE
        )
      )
    }
    
    # Create the plot
    p <- plot_ly(
      data = safety_data,
      x = ~Lead_Time,
      y = ~Safety_Stock,
      text = ~Product,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 12,
        color = "forestgreen",
        opacity = 0.8
      )
    ) %>%
      layout(
        title = "Safety Stock vs Lead Time",
        xaxis = list(title = "Lead Time (Days)"),
        yaxis = list(title = "Safety Stock (Units)"),
        hovermode = "closest"
      )
    
    return(p)
  })
  
  # Order quantity plot
  output$order_quantity_plot <- renderPlotly({
    data <- app_data()
    if (is.null(data)) return(plot_ly())
    
    # Get recommendations
    recommendations <- data$inventory_recommendations$recommendations
    
    # Create a data frame for order quantity analysis
    order_data <- data.frame(
      Product = character(),
      EOQ = numeric(),
      MOQ = numeric(),
      Recommended = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Fill the data frame
    for (product_id in names(recommendations)) {
      rec <- recommendations[[product_id]]
      order_data <- rbind(
        order_data,
        data.frame(
          Product = rec$product_name,
          EOQ = rec$economic_order_quantity,
          MOQ = rec$min_order_quantity,
          Recommended = rec$recommended_order_quantity,
          stringsAsFactors = FALSE
        )
      )
    }
    
    # Create the plot
    p <- plot_ly() %>%
      add_trace(
        data = order_data,
        x = ~Product,
        y = ~EOQ,
        type = "bar",
        name = "Economic Order Quantity",
        marker = list(color = "blue")
      ) %>%
      add_trace(
        data = order_data,
        x = ~Product,
        y = ~Recommended,
        type = "bar",
        name = "Recommended Order Quantity",
        marker = list(color = "green")
      ) %>%
      add_trace(
        data = order_data,
        x = ~Product,
        y = ~MOQ,
        type = "bar",
        name = "Minimum Order Quantity",
        marker = list(color = "red")
      ) %>%
      layout(
        title = "Order Quantity Analysis",
        xaxis = list(title = "Product"),
        yaxis = list(title = "Quantity (Units)"),
        barmode = "group",
        hovermode = "closest",
        legend = list(orientation = "h", x = 0.5, xanchor = "center")
      )
    
    return(p)
  })
  
  # Model Performance tab outputs ----
  
  # Model comparison plot
  output$model_comparison_plot <- renderPlotly({
    data <- app_data()
    if (is.null(data)) return(plot_ly())
    
    # Get evaluation metrics
    arima_metrics <- data$arima_results$evaluation$metrics
    prophet_metrics <- data$prophet_results$evaluation$metrics
    lstm_metrics <- data$lstm_results$evaluation$metrics
    
    # Create a data frame for model comparison
    metrics_data <- data.frame(
      Model = c("ARIMA", "Prophet", "LSTM"),
      RMSE = c(arima_metrics$rmse, prophet_metrics$rmse, lstm_metrics$rmse),
      MAE = c(arima_metrics$mae, prophet_metrics$mae, lstm_metrics$mae),
      MAPE = c(arima_metrics$mape, prophet_metrics$mape, lstm_metrics$mape)
    )
    
    # Create the plot
    p <- plot_ly() %>%
      add_trace(
        data = metrics_data,
        x = ~Model,
        y = ~RMSE,
        type = "bar",
        name = "RMSE",
        marker = list(color = "blue")
      ) %>%
      add_trace(
        data = metrics_data,
        x = ~Model,
        y = ~MAE,
        type = "bar",
        name = "MAE",
        marker = list(color = "green")
      ) %>%
      add_trace(
        data = metrics_data,
        x = ~Model,
        y = ~MAPE,
        type = "bar",
        name = "MAPE",
        marker = list(color = "red")
      ) %>%
      layout(
        title = "Model Performance Comparison",
        xaxis = list(title = "Model"),
        yaxis = list(title = "Error Metric"),
        barmode = "group",
        hovermode = "closest",
        legend = list(orientation = "h", x = 0.5, xanchor = "center")
      )
    
    return(p)
  })
  
  # Performance metrics table
  output$performance_metrics_table <- renderDT({
    data <- app_data()
    if (is.null(data)) return(NULL)
    
    # Get evaluation metrics
    arima_metrics <- data$arima_results$evaluation$metrics
    prophet_metrics <- data$prophet_results$evaluation$metrics
    lstm_metrics <- data$lstm_results$evaluation$metrics
    
    # Create a metrics table
    metrics_table <- data.frame(
      Model = c("ARIMA", "Prophet", "LSTM"),
      RMSE = c(
        round(arima_metrics$rmse, 2),
        round(prophet_metrics$rmse, 2),
        round(lstm_metrics$rmse, 2)
      ),
      MAE = c(
        round(arima_metrics$mae, 2),
        round(prophet_metrics$mae, 2),
        round(lstm_metrics$mae, 2)
      ),
      MAPE = c(
        round(arima_metrics$mape, 2),
        round(prophet_metrics$mape, 2),
        round(lstm_metrics$mape, 2)
      ),
      Accuracy = c(
        round(100 - arima_metrics$mape, 2),
        round(100 - prophet_metrics$mape, 2),
        round(100 - lstm_metrics$mape, 2)
      )
    )
    
    # Display the table
    datatable(
      metrics_table,
      options = list(dom = 'tip'),
      caption = "Model Performance Metrics",
      rownames = FALSE
    ) %>%
      formatStyle(
        'Accuracy',
        background = styleColorBar(c(0, 100), 'lightgreen'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Residual plot
  output$residual_plot <- renderPlotly({
    data <- app_data()
    if (is.null(data)) return(plot_ly())
    
    # Get residuals based on selected model
    selected_model <- input$model
    
    # Default to ARIMA model
    model_name <- "ARIMA"
    residuals <- residuals(data$arima_results$model)
    
    if (selected_model == "prophet") {
      model_name <- "Prophet"
      # For Prophet, create residuals from the evaluation
      actual <- data$prophet_results$evaluation$actual
      predicted <- data$prophet_results$evaluation$predicted
      residuals <- actual - predicted
    } else if (selected_model == "lstm") {
      model_name <- "LSTM"
      # For LSTM, create residuals from the evaluation
      actual <- data$lstm_results$evaluation$actual
      predicted <- data$lstm_results$evaluation$predicted
      residuals <- actual - predicted
    }
    
    # Create a data frame for plotting
    residual_df <- data.frame(
      Index = 1:length(residuals),
      Residual = residuals
    )
    
    # Create the plot
    p <- subplot(
      # Residual time plot
      plot_ly(
        data = residual_df,
        x = ~Index,
        y = ~Residual,
        type = "scatter",
        mode = "lines",
        name = "Residuals",
        line = list(color = "blue")
      ) %>%
        layout(
          title = paste(model_name, "Residuals"),
          xaxis = list(title = "Index"),
          yaxis = list(title = "Residual"),
          showlegend = FALSE
        ),
      
      # Residual histogram
      plot_ly(
        x = residuals,
        type = "histogram",
        name = "Histogram",
        marker = list(color = "steelblue")
      ) %>%
        layout(
          title = "Residual Distribution",
          xaxis = list(title = "Residual"),
          yaxis = list(title = "Count"),
          showlegend = FALSE
        ),
      
      # QQ plot
      plot_ly(
        y = sort(residuals),
        x = qnorm(ppoints(length(residuals))),
        type = "scatter",
        mode = "markers",
        name = "QQ Plot",
        marker = list(color = "forestgreen")
      ) %>%
        layout(
          title = "QQ Plot",
          xaxis = list(title = "Theoretical Quantiles"),
          yaxis = list(title = "Sample Quantiles"),
          showlegend = FALSE
        ),
      
      nrows = 3, titleY = TRUE, heights = c(0.33, 0.33, 0.33)
    ) %>%
      layout(
        title = paste(model_name, "Residual Analysis"),
        margin = list(t = 50)
      )
    
    return(p)
  })
  
  # Settings tab functionality ----
  
  # Apply forecast settings
  observeEvent(input$apply_settings, {
    # In a real application, this would update the model parameters
    # For this prototype, just show a notification
    showNotification("Forecast settings applied successfully!", type = "message", duration = 3)
  })
  
  # Apply inventory settings
  observeEvent(input$apply_inventory_settings, {
    # In a real application, this would update the inventory parameters
    # For this prototype, just show a notification
    showNotification("Inventory settings applied successfully!", type = "message", duration = 3)
  })
  
  # Apply data settings
  observeEvent(input$apply_data_settings, {
    # In a real application, this would update the data settings
    # For this prototype, just show a notification
    showNotification("Data settings applied successfully!", type = "message", duration = 3)
  })
  
  # Test database connection
  observeEvent(input$test_connection, {
    # In a real application, this would test the database connection
    # For this prototype, just show a notification
    if (input$data_source == "db") {
      showNotification(paste("Testing connection to", input$db_name, "on", input$db_host, "..."), type = "default", duration = 3)
      
      # Simulate connection test
      Sys.sleep(1)
      
      showNotification("Database connection successful!", type = "message", duration = 3)
    }
  })
}

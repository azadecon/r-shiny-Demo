# app.R
library(shiny)
library(httr)
library(rvest)
library(dplyr)
library(DT)
library(ggplot2)
library(tidyr)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("PDS Bihar API Data Fetcher"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("start_month", "Start Month (MM)", min = 1, max = 12, value = 3),
      numericInput("end_month", "End Month (MM)", min = 1, max = 12, value = 7),
      actionButton("fetch", "Fetch Data"),
      br(), br(),
      selectInput("district_select", "Select District:",
                  choices = NULL, selected = NULL)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table",
                 h4("Fetched Data"),
                 DTOutput("table")
        ),
        
        tabPanel("Summary Stats",
                 h4("Summary Table"),
                 tableOutput("summary_stats")
        ),
        
        tabPanel("District Time Series",
                 h4("Total Cards (A), Transactions (B), and Portability (C) Over Time"),
                 plotOutput("time_series_plot", height = "400px"),
                 br(),
                 h4("Portability as Share of Transactions (C / B)"),
                 plotOutput("portability_share_plot", height = "300px")
        )
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  final_data <- reactiveVal(NULL)
  
  observeEvent(input$fetch, {
    start_month <- input$start_month
    end_month <- input$end_month
    
    if (start_month > end_month) {
      showNotification("Start month cannot be after end month.", type = "error")
      return(NULL)
    }
    
    if (end_month > 8) {
      showNotification("Please wait for the data to be uploaded.", type = "error")
      return(NULL)
    }
    
    if (start_month < 3) {
      showNotification("This is just a demo app. Chill!", type = "error")
      return(NULL)
    }
    
    all_data <- list()
    
    for (month_no in start_month:end_month) {
      month_str <- sprintf("%02d", month_no)
      body <- paste0("trans_date=29-", month_str, "-2025")
      
      headers <- c(
        'Accept' = '*/*',
        'Accept-Language' = 'en-US,en;q=0.9',
        'Connection' = 'keep-alive',
        'Content-Type' = 'application/x-www-form-urlencoded; charset=UTF-8',
        'Origin' = 'https://epos.bihar.gov.in',
        'Referer' = 'https://epos.bihar.gov.in/AbstractTransReport.jsp',
        'Sec-Fetch-Dest' = 'empty',
        'Sec-Fetch-Mode' = 'cors',
        'Sec-Fetch-Site' = 'same-origin',
        'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/135.0.0.0 Safari/537.36 OPR/120.0.0.0',
        'X-Requested-With' = 'XMLHttpRequest',
        'sec-ch-ua' = '"Opera";v="120", "Not-A.Brand";v="8", "Chromium";v="135"',
        'sec-ch-ua-mobile' = '?0',
        'sec-ch-ua-platform' = '"Windows"',
        'Cookie' = 'JSESSIONID=glB2vrbjhwPRAumGuSkHWjSG'
      )
      
      res <- tryCatch({
        VERB("POST", url = "https://epos.bihar.gov.in/abstract_Transaction_report.action", 
             body = body, add_headers(headers))
      }, error = function(e) {
        showNotification(paste("Error fetching data for month", month_str), type = "error")
        return(NULL)
      })
      
      if (!is.null(res)) {
        df_list <- read_html(res) %>% html_table(fill = TRUE)
        
        if (length(df_list) > 0) {
          df <- df_list[[1]]
          colnames(df) <- df[4, ]
          df <- df[-c(1:4), ]
          
          clean_df <- df[, -c(1, 3:5, 7:11, 13, 15:23)]
          colnames(clean_df) <- c("District", "Total Cards (A)", "No of Transactions (B)", "Portability (C)", "Month")
          
          clean_df$Month <- month_str
          
          all_data[[month_str]] <- clean_df
        }
      }
    }
    
    combined_df <- bind_rows(all_data)
    combined_df$Month <- as.numeric(combined_df$Month)
    
    # Convert to numeric
    combined_df$`Total Cards (A)` <- as.numeric(gsub(",", "", combined_df$`Total Cards (A)`))
    combined_df$`No of Transactions (B)` <- as.numeric(gsub(",", "", combined_df$`No of Transactions (B)`))
    combined_df$`Portability (C)` <- as.numeric(gsub(",", "", combined_df$`Portability (C)`))
    
    final_data(combined_df)
    
    # Update district dropdown
    updateSelectInput(session, "district_select",
                      choices = unique(combined_df$District),
                      selected = unique(combined_df$District)[1])
  })
  
  # -----------------------------
  # Outputs
  # -----------------------------
  
  # 1. Data Table
  output$table <- renderDT({
    req(final_data())
    datatable(final_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # 2. Summary Stats Table
  # 2. Summary Stats Table (only selected district + Total)
  output$summary_stats <- renderTable({
    req(final_data(), input$district_select)
    
    df <- final_data()
    
    df %>%
      filter(District %in% c(input$district_select, "Total")) %>%
      group_by(District) %>%
      summarise(
        Avg_Cards = round(mean(`Total Cards (A)`, na.rm = TRUE), 0),
        Avg_Transactions = round(mean(`No of Transactions (B)`, na.rm = TRUE), 0),
        Avg_Portability = round(mean(`Portability (C)`, na.rm = TRUE), 0)
      ) %>%
      arrange(match(District, c(input$district_select, "Total")))
  })
  
  
  # 3. Time Series Plot for Selected District (A, B, C)
  output$time_series_plot <- renderPlot({
    df <- final_data()
    req(df, input$district_select)
    
    df_filtered <- df %>%
      filter(District == input$district_select) %>%
      mutate(Month = as.numeric(Month)) %>%
      pivot_longer(
        cols = c(`Total Cards (A)`, `No of Transactions (B)`, `Portability (C)`),
        names_to = "Metric",
        values_to = "Value"
      )
    
    ggplot(df_filtered, aes(x = Month, y = Value/100000, color = Metric)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = sort(unique(df_filtered$Month))) +
      labs(
        title = paste("Time Series for", input$district_select),
        x = "Month",
        y = "In lakhs",
        color = "Metric"
      ) +
      theme_minimal()
  })
  
  # 4. Portability Share Plot (C / B)
  output$portability_share_plot <- renderPlot({
    df <- final_data()
    req(df, input$district_select)
    
    df_filtered <- df %>%
      filter(District == input$district_select) %>%
      mutate(
        Month = as.numeric(Month),
        Portability_Share = `Portability (C)` / `No of Transactions (B)`
      )
    
    ggplot(df_filtered, aes(x = Month, y = Portability_Share)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(color = "darkblue", size = 2) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_x_continuous(breaks = sort(unique(df_filtered$Month))) +
      labs(
        title = paste("Portability as Share of Transactions in", input$district_select),
        x = "Month",
        y = "Portability Share"
      ) +
      theme_minimal()
  })
}

# -----------------------------
# Run App
# -----------------------------
shinyApp(ui = ui, server = server)

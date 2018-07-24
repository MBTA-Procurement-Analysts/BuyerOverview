# server.r
# created by: Mickey Guo
# Shiny Server Function for Buyer Overview Shiny App


# Server Function, make sure this is at bottom ----------------------------

function (input, output) {
  
  # Reactive Data Definitions ---------------------------------------------
  
  # Raw data, used for calculating sidebar numbers
  pg1_reactive_raw_buyer_req <- reactive({
    filter(raw_buyer_req, Buyer == input$buyer)
  })
  
  # Raw data with selected rows for plotly and Datatable, contains outliers
  pg1_reactive_raw_buyer_plot_dt_req <- reactive({
    filter(raw_buyer_req, Buyer == input$buyer) %>%
      select(`Req ID`, Status, Requester, `Req Date`, `Approval_Date`, `PO Date`, Duration, `PO No.`, `Amount`)
  })
  
  # Raw data, with selected rows and no outliers
  # Outliers are defined by anything above 2x StdDev for Amount or Duration.
  pg1_reactive_99pct_buyer_plot_dt_req <- reactive({
    filter(raw_buyer_req, Buyer == input$buyer) %>%
      select(`Req ID`, Status, Requester, `Req Date`, `Approval_Date`, `PO Date`, Duration, `PO No.`, `Amount`) %>%
      filter(Amount < mean(Amount) + 2*sd(Amount) & Duration < mean(Duration) + 2*sd(Duration))
  })
  
  # Just the outliers
  pg1_reactive_outlier_buyer_plot_dt_req <- reactive({
    filter(raw_buyer_req, Buyer == input$buyer) %>%
      select(`Req ID`, Status, Requester, `Req Date`, `Approval_Date`, `PO Date`, Duration, `PO No.`, `Amount`) %>%
      filter(Amount >= mean(Amount) + 2*sd(Amount) | Duration >= mean(Duration) + 2*sd(Duration))
  })
  
  # Crosstalk -------------------------------------------------------------
  
  # Data shared between plotly and DT
  pg1_ct_shared_data = SharedData$new(pg1_reactive_99pct_buyer_plot_dt_req)
  
  # Plotly scatter that takes in DT input of selected rows
  output$pg1_main_plot_by_buyer <- renderPlotly({
    selected = input$pg1_main_dt_by_buyer_rows_selected
    
    # Not selected anything
    if (!length(selected)) {
      plot_default <- pg1_ct_shared_data %>%
        plot_ly(x = ~Duration,
                y = ~Amount,
                type = 'scatter',
                mode = 'markers',
                color = I('blue')) %>%
        layout(xaxis = list(title = "Req Duration (Days)"),
               showlegend = FALSE)
    } else if (length(selected)) {
      # If selected something, render everything first
      plot_selected <- pg1_reactive_99pct_buyer_plot_dt_req() %>%
        plot_ly() %>%
        add_trace(x = ~Duration,
                  y = ~Amount,
                  type = 'scatter',
                  mode = 'markers',
                  color = I('blue')) %>%
        layout(xaxis = list(title = "Req Duration (Days)"),
               showlegend = FALSE)
      
      # Then render the selected rows
      plot_selected <- add_trace(plot_selected, data = pg1_reactive_99pct_buyer_plot_dt_req()[selected,],
                                 x = ~Duration,
                                 y = ~Amount,
                                 type = 'scatter',
                                 mode = 'markers',
                                 color = I('red'), name = "Selected")
    }
  })
  
  # Backup plotly for non-crosstalk use
  output$pg1_main_plot_by_buyer_backup <- renderPlotly(plot_ly(data = pg1_reactive_99pct_buyer_plot_dt_req(),
                                                               x = ~Duration,
                                                               y = ~Amount,
                                                               type = 'scatter',
                                                               mode = 'markers') %>%
                                                         layout(xaxis = list(title = "Req Duration (Days)")))
  
  # Sidebar Data Widgets --------------------------------------------------
  
  # Uses reactive raw data to calculate metrics on the fly, slightly hack-ish
  
  output$pg1_viewing_buyer_count <- renderText({paste("Buyer Total Lines Count:",
                                                      summarise(pg1_reactive_raw_buyer_req(),
                                                                Count = n())[[1]])})
  
  output$pg1_viewing_buyer_sum <- renderText({paste("Buyer Dollar Amount YTD:",
                                                    currency(
                                                      summarise(
                                                        pg1_reactive_raw_buyer_req(), Sum = sum(Amount))[[1]]),
                                                    sep = '\n')})
  
  output$pg1_viewing_buyer_2moago <- renderText({paste("Buyer Dollar Amount in", prevmos[1], ": ",
                                                       currency(
                                                         summarise(
                                                           filter(pg1_reactive_raw_buyer_req(), Month == prevmos[1]), Sum = sum(Amount))[[1]]),
                                                       sep = '\n')})
  
  output$pg1_viewing_buyer_1moago <- renderText({paste("Buyer Dollar Amount in", prevmos[2], ": ",
                                                       currency(
                                                         summarise(
                                                           filter(pg1_reactive_raw_buyer_req(), Month == prevmos[2]), Sum = sum(Amount))[[1]]),
                                                       sep = '\n')})
  
  # Datatable Definitions -------------------------------------------------
  
  output$pg1_main_dt_by_buyer <- DT::renderDataTable(DT::datatable(pg1_reactive_99pct_buyer_plot_dt_req(),
                                                                   rownames = FALSE,
                                                                   colnames = c("Duration (Days)" = "Duration")) %>%
                                                       formatCurrency(c("Amount")) %>%
                                                       formatDate(c("Approval_Date", "Req Date", "PO Date"), "toLocaleDateString"))
  
  output$pg1_main_dt_by_buyer_outliers <- DT::renderDataTable(DT::datatable(pg1_reactive_outlier_buyer_plot_dt_req(),
                                                                            rownames = FALSE,
                                                                            colnames = c("Duration (Days)" = "Duration")) %>%
                                                                formatCurrency(c("Amount")) %>%
                                                                formatDate(c("Approval_Date", "Req Date", "PO Date"), "toLocaleDateString"))
}

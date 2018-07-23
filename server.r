# server.r
# created by: Mickey Guo
# Shiny Server Function for Buyer Overview Shiny App


# Server Function, make sure this is at bottom ----------------------------

function (input, output) {
  
  reactive_raw_buyer_req <- reactive({
    filter(raw_buyer_req, Buyer == input$buyer)
  })
  
  reactive_raw_buyer_dt_req <- reactive({
    filter(raw_buyer_req, Buyer == input$buyer) %>% select(Duration, `Req ID`, `Req Date`, `Approval_Date`, `PO No.`, `Amount`)
  })
  
  # pg1_plot_buyer_req <- plot_ly(data = reactive_raw_buyer_req(), 
  #                               x = ~Duration,
  #                               y = ~Amount, 
  #                               type = 'scatter', 
  #                               mode = 'markers')
  
  output$pg1_main_plot_by_buyer <- renderPlotly(plot_ly(data = reactive_raw_buyer_req(), 
                                                        x = ~Duration,
                                                        y = ~Amount, 
                                                        color = ~Business_Unit,
                                                        type = 'scatter', 
                                                        mode = 'markers') %>% 
                                                  layout(xaxis = list(title = "Req Duration (Days)")))
  
  output$pg1_viewing_buyer_count <- renderText({paste("Buyer Total Lines Count:",
                                                      summarise(reactive_raw_buyer_req(), 
                                                                Count = n())[[1]])})
  
  output$pg1_viewing_buyer_sum <- renderText({paste("Buyer Dollar Amount YTD:", 
                                                    currency(summarise(reactive_raw_buyer_req(), Sum = sum(Amount))[[1]]), sep = '\n')})
  
  output$pg1_viewing_buyer_2moago <- renderText({paste("Buyer Dollar Amount in", prevmos[1], ": ",
                                                    currency(
                                                      summarise(
                                                        filter(reactive_raw_buyer_req(), Month == prevmos[1]), Sum = sum(Amount))[[1]]),
                                                    sep = '\n')})
  
  output$pg1_viewing_buyer_1moago <- renderText({paste("Buyer Dollar Amount in", prevmos[2], ": ",
                                                       currency(
                                                         summarise(
                                                           filter(reactive_raw_buyer_req(), Month == prevmos[2]), Sum = sum(Amount))[[1]]),
                                                       sep = '\n')})
  
  output$pg1_main_dt_by_buyer <- DT::renderDataTable(DT::datatable(reactive_raw_buyer_dt_req(), 
                                                                   rownames = FALSE) %>% 
                                                       formatCurrency(c("Amount")) %>% 
                                                       formatDate(c("Approval_Date", "Req Date"), "toLocaleDateString"))
}

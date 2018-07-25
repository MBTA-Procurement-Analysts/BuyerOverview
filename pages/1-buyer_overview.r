# File: 1-buyer_overview.r
# Created by: Mickey Guo
# Overview of Buyer Req information, per buyer


# Setup Constants, Variables, Functions -----------------------------------

# FY Month Factors, July first
fy_factors <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                "Jan", "Feb", "Mar", "Apr", "May", "Jun")

# Two previous months, in chronological order

# prevmos <- c("May", "Jun")

# or use this function that takes date of your local machine.
prevmos <- as.character(month(month(today()) - c(2,1), label = TRUE, abbr = TRUE))

# Data import -------------------------------------------------------------

raw_req <- readxl::read_excel("data/NG_SPILT_REQ_SIDE_1458.xlsx", skip = 1, guess_max = 2^16) %>%
  filter(!is.na(Buyer))


# Data Wrangling ----------------------------------------------------------

buyers <- raw_req %>% distinct(Buyer)


# Adds days between Approval and Requisition Date
# Line level Reqs
(raw_buyer_req_line <- raw_req  %>%
    mutate(Month = month(`Req Date`, label = TRUE, abbr = TRUE)) %>%
    mutate_at("Month", ~parse_factor(., levels = fy_factors)) %>%
    mutate(`Duration` = as.numeric(`PO Date` - `Req Date`, units="days")) %>%
    filter(Duration >= 0) %>%
    arrange(desc(Duration)) %>%
    select(Month, Duration, everything()))

# Req ID level Reqs
(raw_buyer_req_id <- raw_buyer_req_line %>%
    group_by(`Req ID`) %>%
    mutate(Req_Sum = sum(Amount)) %>%
    mutate(Avg_Duration = mean(Duration)) %>%
    select(-`Req_Line`, -`Req Qty`, -`Item`, -`UOM`, -`More Info`,
           -`Description`, -`Product`, -`PO_Line`, -`Amount`, -Duration, 
           -`PO No.`, -`PO Date`, -Fund, -`Dept/Loc`) %>%
    ungroup(`Req ID`) %>%
    distinct() %>% 
    rename(`Duration` = Avg_Duration, Amount = Req_Sum))

# Choose which Req Table for Plotly and DT
# Req ID Level
raw_buyer_req <- raw_buyer_req_id

# Req Line Level, uniting Req ID and Line Number for displaying purposes 
# (raw_buyer_req <- raw_buyer_req_line  %>%
#     unite("Req ID", c("Req ID", "Req_Line"), sep = "_"))

# Plot Data ---------------------------------------------------------------

raw_buyer_req %>% plot_ly(x = ~(Approval_Date - `Req Date`), y = ~Amount, type = 'scatter', mode = 'markers', color = ~Buyer)


# Shiny UI Definition for this Page ---------------------------------------

uiPage1 <- tabPanel("Req by Buyers",
                    verticalLayout(
                      fluidRow(column(2,
                                      selectInput("buyer", "Buyer", choices = buyers),
                                      textOutput("pg1_viewing_buyer_count"),
                                      textOutput("pg1_viewing_buyer_sum"),
                                      textOutput("pg1_viewing_buyer_1moago"),
                                      textOutput("pg1_viewing_buyer_2moago")),
                               column(10, plotlyOutput("pg1_main_plot_by_buyer"),
                                      DT::dataTableOutput("pg1_main_dt_by_buyer"),
                                      h3("Outliers"),
                                      DT::dataTableOutput("pg1_main_dt_by_buyer_outliers")))))

# 3-buyer_po_overview.r
# Created by: Mickey Guo
# Overview of Buyer PO information, per buyer


# Setup Constants, Variables, Functions -----------------------------------

# FY Month Factors, July first
fy_factors <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                "Jan", "Feb", "Mar", "Apr", "May", "Jun")

# Two previous months, in chronological order

# prevmos <- c("May", "Jun")

# or use this function that takes date of your local machine.
prevmos <- as.character(month(month(today()) - c(2,1), label = TRUE, abbr = TRUE))


# Data import -------------------------------------------------------------

pg3_raw_req <- readxl::read_excel("data/NG_SPILT_REQ_SIDE_1458.xlsx", skip = 1, guess_max = 2^16) %>%
  filter(!is.na(Buyer))


# Data Wrangling ----------------------------------------------------------

buyers <- pg3_raw_req %>% distinct(Buyer)

(pg3_raw_buyer_req_line <- raw_req  %>%
    mutate(Month = month(`Req Date`, label = TRUE, abbr = TRUE)) %>%
    mutate_at("Month", ~parse_factor(., levels = fy_factors)) %>%
    mutate(`Duration` = as.numeric(`PO Date` - `Req Date`, units="days")) %>%
    filter(Duration >= 0) %>%
    arrange(desc(Duration)) %>%
    select(Month, Duration, everything()))

(pg3_raw_buyer_req_id <- raw_buyer_req_line %>%
    group_by(`Req ID`) %>%
    mutate(Req_Sum = sum(Amount)) %>%
    mutate(Avg_Duration = mean(Duration)) %>%
    select(-`Req_Line`, -`Req Qty`, -`Item`, -`UOM`, -`More Info`,
           -`Description`, -`Product`, -`PO_Line`, -`Amount`, -Duration,
           -`PO No.`, -`PO Date`, -Fund, -`Dept/Loc`) %>%
    ungroup(`Req ID`) %>%
    distinct())


# Shiny UI Definition for this Page ---------------------------------------

uiPage3 <- tabPanel("Req by Buyers",
                    verticalLayout(
                      fluidRow(column(2,
                                      selectInput("pg3_buyer", "Buyer", choices = buyers),
                                      textOutput("pg3_viewing_buyer_count"),
                                      textOutput("pg3_viewing_buyer_sum"),
                                      textOutput("pg3_viewing_buyer_1moago"),
                                      textOutput("pg3_viewing_buyer_2moago")),
                               column(10, plotlyOutput("pg3_main_plot_by_buyer"),
                                      DT::dataTableOutput("pg3_main_dt_by_buyer"),
                                      h3("Outliers"),
                                      DT::dataTableOutput("pg3_main_dt_by_buyer_outliers")))))

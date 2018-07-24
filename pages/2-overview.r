# 2-overview.r
# Created by: Mickey Guo
# General Overview of Req Information

# Setup Constants, Variables, Functions ----------------------------------------

# FY Month Factors, July first
fy_factors <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                "Jan", "Feb", "Mar", "Apr", "May", "Jun")

# 2 Previous months, in chronological order
prevmos <- as.character(month(month(today()) - c(2,1), label = TRUE, abbr = TRUE))

# Data Import ------------------------------------------------------------------

# NAs are removed in this step so the Buyer to factor feature is functional.
raw_req <- readxl::read_excel("data/NG_SPILT_REQ_SIDE_7750.xlsx", skip = 1, guess_max = 2^16) %>%
  filter(!is.na(Buyer))

# Data Wrangling --------------------------------------------------------------

buyers <- raw_req %>% distinct(Buyer)


# Adds days between Approval and Requisition Date
raw_buyer_req <- raw_req  %>%
  mutate(Month = month(`Req Date`, label = TRUE, abbr = TRUE)) %>%
  mutate_at("Month", ~parse_factor(., levels = fy_factors)) %>%
  mutate(`Duration` = as.numeric(`Approval_Date` - `Req Date`, units="days")) %>%
  arrange(desc(Duration)) %>%
  select(Month, Duration, everything())


# Plot Data ---------------------------------------------------------------

raw_buyer_req %>% ggplot() + geom_point(mapping = aes(x = Duration, y = Amount, color = `Business_Unit`)) + xlim(0, 100) + ylim(0,10000000)

raw_buyer_req %>% plot_ly(x = ~(Approval_Date - `Req Date`), y = ~Amount, type = 'scatter', mode = 'markers', color = ~Business_Unit)


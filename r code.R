# Load required packages using pacman
pacman::p_load(
  rio,           # Import/export data
  here,          # Manage file paths
  tidyverse,     # Data manipulation and visualisation
  lubridate,     # Date and time handling
  plotly,        # Interactive visualisation
  gganimate,     # Animated plots
  ggExtra,       # Additional ggplot2 components
  ggalt,         # Alternatives for ggplot2
  ggcorrplot,    # Correlation matrix plots
  ggpubr,        # Publication-ready plots
  ggplot2,       # Main visualisation package (including ggarrange())
  zoo,           # Time series manipulation
  data.table,    # Efficient data handling
  formattable,   # Formatting tables
  tidyr,         # Data tidying
  ggpmisc,       # Add stat_valey or annotations
  ggalluvial,    # Sankey charts and alluvial diagrams
  RColorBrewer,  # Colour palettes
  CGPfunctions,  # Slope graphs and other functions
  ggrepel,       # Avoid overlapping labels in plots
  ggthemes,      # Additional themes for ggplot2
  viridis        # Colour palettes for better perceptual design
)

# Import data
mydata <- import(here("data", "table_511_and_521.xlsx"), which = "5.1.1 (excl tax)", skip = 7)  

# Data cleaning
## Price excl tax 
data <- mydata %>% janitor::clean_names()   # clean cols name 
colnames(data)[18] <- 'UK'                  # rename 

data <- data[complete.cases(data), ].       # remove NA 

data <- data %>%                            # Remove unwanted columns 
  select(-month, 
         -day_in_month_of_price_snapshot, 
         -uk_rank_in_eu14_plus_uk, 
         -uk_rank_in_eu27_plus_uk)

data$year <- as.factor(data$year)           # change data class 

data <- data %>%                            # calculate annual avg price 
  group_by(year) %>% 
  summarise(across(2:28, ~ mean(.x, na.rm = TRUE)))

dtslop <- data %>%                          # long to wide format
  pivot_longer(
    cols = belgium:slovenia,
    names_to = "country",
    values_to = "price_excl_tax"
  )

dtslop$price_excl_tax <- round(dtslop$price_excl_tax, 1)

str(dtslop)

## Price incl tax
mydata2 <- import(here("data", "table_511_and_521.xlsx"), which = "5.1.1 (incl tax)", skip = 7)

data2 <- mydata2 %>%
  janitor::clean_names() %>%              
  rename(UK = colnames(.)[18])

data2 <- data2[complete.cases(data2), ]

data2 <- data2 %>%                          
  select(-month, 
         -day_in_month_of_price_snapshot, 
         -uk_rank_in_eu14_plus_uk, 
         -uk_rank_in_eu27_plus_uk)

data2 <- data2 %>%
  mutate(year = as.factor(year))

data2 <- data2 %>%                         
  group_by(year) %>% 
  summarise(across(2:28, ~ mean(.x, na.rm = TRUE)))

dtslop2 <- data2 %>% 
  pivot_longer(
    cols = belgium:slovenia,
    names_to = "country",
    values_to = "price_incl_tax"
  )

dtslop2$price_incl_tax <- round(dtslop2$price_incl_tax, 0)

# Other data cleaning 
dtam <- cbind(dtslop, dtslop2$price_incl_tax) 
colnames(dtam)[4] <- 'price_incl_tax'
dtam <- dtam %>%
  mutate(price_incl_tax = round(price_incl_tax, 1))

# MoM, YoY changes
## data preparation 
mydata <- import(here("data", "Weekly_Fuel_Prices_240423.xlsx"), which = "All years", skip = 7)

data <- mydata %>% janitor::clean_names()
data <- data %>% filter(date > "2004-06-1") # omit NA value 

str(data) # please note: date has POSIXct, format, not as.Date format 

## Calculation for 1 variable only 
fuelprice <- data %>%
  mutate(YearMonth = format(date, "%Y-%m")) 

monthly_price <- fuelprice %>%
  group_by(YearMonth) %>%
  summarize(MonthlyPrice = mean(ulsp_pump_price_p_litre)) %>%
  arrange(YearMonth)

monthly_price <- monthly_price[monthly_price$YearMonth >= "2005-01", ] # Have a full year of 12 months. 

monthly_report <- monthly_price %>%
  mutate(
    mom = (MonthlyPrice - lag(MonthlyPrice)) / lag(MonthlyPrice),
    yoy = (MonthlyPrice - lag(MonthlyPrice, 12)) / lag(MonthlyPrice, 12)
  )

monthly_report <- monthly_report %>%
  mutate(
    mom = round(mom * 100, 1),
    yoy = round(yoy * 100, 1)
  )

## Calculate for multiple variables
fuelprice <- data %>%
  select(c(1:2,7)) %>% 
  mutate(YearMonth = format(date, "%Y-%m")) # create new column 

fuelprice <- fuelprice[, c(1,4,2,3)] # reorder column

str(fuelprice)

monthly_price <- fuelprice %>%
  group_by(YearMonth) %>%
  summarise(across(c(ulsp_pump_price_p_litre, ulsd_pump_price_p_litre), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}")) %>%
  arrange(YearMonth)

monthly_price <- monthly_price[monthly_price$YearMonth >= "2005-01", ] # Have a full year of 12 months. 

monthly_report <- 
  monthly_price %>%
  mutate(across(c("mean_ulsp_pump_price_p_litre", "mean_ulsd_pump_price_p_litre"),list(mom = ~(. - lag(., 1))/lag(., 1)))*100) %>%
  mutate(across(c("mean_ulsp_pump_price_p_litre", "mean_ulsd_pump_price_p_litre"), list(yoy = ~(. - lag(., 12))/lag(., 12)))*100)

monthly_report_2 <- 
  monthly_price %>%  # option 2
  mutate(across(c("mean_ulsp_pump_price_p_litre", "mean_ulsd_pump_price_p_litre"), ~(. - lag(., 1))/lag(., 1), .names = "mom_{.col}")) %>%
  mutate(across(c("mean_ulsp_pump_price_p_litre", "mean_ulsd_pump_price_p_litre"), ~(. - lag(., 12))/lag(., 12), .names = "yoy_{.col}"))

monthly_report <- 
  monthly_report %>%
  mutate(across(where(is.numeric), ~round(., 2))) # rounding 

monthly_report <- monthly_report[, c(1,2,4,6,3,5,7)]  # reorder cols 

col_names <- list("year_month",    # rename cols 
                  "ulsp_m_price", 
                  "ulsp_mom", 
                  "ulsp_yoy",
                  "ulsd_m_price", 
                  "ulsd_mom", 
                  "ulsd_yoy")

colnames(monthly_report) <- c(col_names)

## Viz with table 
monthly_report_kable <- monthly_report. 

kbl(monthly_report_kable) %>%
  kable_classic() %>%
  add_header_above(c(" " = 1, "ULSP" = 3, "ULSD" = 3)) %>%  # Add header rows to group columns
  footnote(general = "Data is retrieved from GOV.UK on 30 Apr 2023. ",
           footnote_as_chunk = T, title_format = c("italic", "underline")) %>% 
  scroll_box(width = "100%", height = "200px")

monthly_report_kable$ulsp_mom <- cell_spec(monthly_report_kable$ulsp_mom,   # highlight in text 
                                           color = ifelse(is.na(monthly_report_kable$ulsp_mom), "lightgrey",
                                                          ifelse(monthly_report_kable$ulsp_mom < 0, "red", "blue"))
)

kbl(monthly_report_kable, escape = F) %>% # display table 
  kable_paper("striped", full_width = F) %>% 
  scroll_box(width = "100%", height = "200px")

formattable(  # format 
  head(monthly_report, 15),
  align = c("l", rep("r", NCOL(monthly_report) - 1)), # format column to left / right aligment 
  list(`year_month` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")))
)

diff_formatter <- 
  formatter("span", style = x ~ style(color = ifelse(x > 0, "#71CA97", ifelse(x < 0, "red", "black")),"font.size" = "12px"),
            x ~ icontext(ifelse(x>0, "arrow-up", ifelse(x<0, "arrow-down", "")), x))

formattable(
  head(monthly_report, 15), # if not using head, the table will be long
  align = c("l", rep("r", NCOL(monthly_report) - 1)), 
  list(
    `year_month` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
    `ulsp_m_price` = color_tile("#DeF7E9", "#71CA97"),
    `ulsd_m_price` = color_tile("#DeF7E9", "#71CA97"),
    `ulsp_mom` = diff_formatter,
    `ulsd_mom` = diff_formatter,
    `ulsp_yoy` = diff_formatter,
    `ulsd_yoy` = diff_formatter)
)


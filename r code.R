#--- Load required packages using pacman
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

#--- Import data
mydata <- import(here("data", "table_511_and_521.xlsx"), which = "5.1.1 (excl tax)", skip = 7)  

#--- Data cleaning
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

#--- Other data cleaning 
dtam <- cbind(dtslop, dtslop2$price_incl_tax) 
colnames(dtam)[4] <- 'price_incl_tax'
dtam <- dtam %>%
  mutate(price_incl_tax = round(price_incl_tax, 1))

#--- MoM, YoY changes
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

#--- Time Series Plot for USLP: Pump price (p/litre)

mydata <- import(here("data", "Weekly_Fuel_Prices_240423.xlsx"), 
                 which = "All years", 
                 skip = 7)

data <- mydata %>% janitor::clean_names()
data <- data %>% filter(date > "2004-06-1")

str(data)

## by year 
g1 <- ggplot(data, aes(x = date, y = ulsp_pump_price_p_litre)) +
  geom_line() +
  labs(title = "ULSP:  Pump price (p/litre)",
       caption = "Source: GOV.UK 2023",
       x = "Year",
       y = "p/litre") +
  theme_classic() +
  annotate(geom = "text",
           x = as.POSIXct("2008-08-25", origin="1970-01-01"), y = 70,
           label = "2008\nEconomic Crisis?",
           family = "Arial Narrow",
           colour = "red",
           size = 3, fontface = "bold") +
  annotate(geom = "rect",
           xmin = as.POSIXct("2008-08-25", origin="1970-01-01"), xmax = as.POSIXct("2009-08-25", origin="1970-01-01"),
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = .4)
g1

ggplot(data, aes(x = date, y = ulsp_pump_price_p_litre)) +
  geom_line(color="#69b3a2") +
  labs(title = "ULSP:  Pump price (p/litre)",
       caption = "Source: GOV.UK 2023",
       x = "Year",
       y = "p/litre") +
  theme_classic() +
  annotate(geom = "text",
           x = as.POSIXct("2008-08-25", origin="1970-01-01"), y = 70,
           label = "2008\nEconomic Crisis?",
           family = "Arial Narrow",
           colour = "red",
           size = 3, fontface = "bold") +
  annotate(geom = "rect",
           xmin = as.POSIXct("2008-08-25", origin="1970-01-01"), xmax = as.POSIXct("2009-08-25", origin="1970-01-01"),
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = .4) +
  annotate(geom="text", x=as.POSIXct("2022-07-04"), y=191.5466, label="Price reached 191$ at mid of 2022", color = "blue", angle = 0,
           hjust = 1.0, vjust = 0.1) +
  annotate(geom="point", x=as.POSIXct("2022-07-04"), y=191.5466, size=6, shape=21, fill="transparent") 

# by day, week, month, year
data$year <- year(data$date)
# data$date has POSIXct format, so we need to change it into date format for scale_x_date()
data$date <- as.Date(data$date, format = "%Y-%m-%d") 
ggplot(data, aes(x = date, y = ulsp_pump_price_p_litre)) +
  geom_line(color="#69b3a2") +
  facet_wrap(~year, scales = "free") +
  scale_x_date(date_labels = "%b")

x <- data   # select recent year 
x <- x[x$date >= as.Date("2013-01-01"), ]
x$date <- as.Date(x$date, format = "%Y-%m-%d")

ggplot(x, aes(x = date, y = ulsp_pump_price_p_litre)) +
  geom_line(color="#69b3a2") +
  facet_wrap(~year, scales = "free") +
  scale_x_date(date_labels = "%b")

g2 <- ggplot(data, aes(date, ulsp_pump_price_p_litre, color = ulsp_pump_price_p_litre)) +  
  geom_line(show.legend = T) +
  scale_colour_gradient(low = "blue", high = "orange") +  # add color to line 
  labs(title = "ULSP:  Pump price (p/litre)",
       subtitle = "Weekly Prices time series (from 2003)",
       caption = "Source: GOV.UK 2023",
       x = "Year",
       y = "p/litre")+
  theme_classic()
g2

g2 <- ggplot(data, aes(x = date, y = ulsp_diff_on_previous_week_p_litre)) +
  geom_line() +
  labs(title = "ULSP:  Pump price (p/litre)",
       subtitle="Diff on previous WEEK (p/litre)",
       caption = "Source: GOV.UK",
       x = "Year",
       y = "p/litre") +
  theme_classic()

g2

g3 <- ggplot(data, aes(date, ulsp_diff_on_previous_year_p_litre)) +
  geom_line() +
  labs(title = "ULSP:  Pump price (p/litre)",
       subtitle = "Diff on previous  YEAR (p/litre)",
       caption = "Source: GOV.UK 2023",
       x = "Year",
       y = "p/litre") +
  theme_classic()

g3

## combination time series data & diff
g1 <- ggplot(data, aes(x = date, y = ulsp_pump_price_p_litre)) +
  geom_line() +
  labs(title    = "ULSP:  Pump price (p/litre)",
       subtitle = "Weekly Prices time series (from 2003)",
       y        = "p/litre") + 
  theme_classic() + 
  theme_update(axis.title.x = element_blank()) 

g2 <- ggplot(data, aes(x = date, y = ulsp_diff_on_previous_week_p_litre)) +
  geom_line() +
  labs(subtitle  ="Diff on previous WEEK (p/litre)",
       y         = "p/litre") + 
  theme_classic() + 
  theme_update(axis.title.x = element_blank()) 

g3 <- ggplot(data, aes(date, ulsp_diff_on_previous_year_p_litre)) +
  geom_line() +
  labs(subtitle  = "Diff on previous  YEAR (p/litre)",
       y         = "p/litre") + 
  theme_classic() + 
  theme_update(axis.title.x = element_blank())

ggpubr::ggarrange(g1,  # First row with line plot
                  ggarrange(g2, g3, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
                  nrow   = 2,
                  labels = "A"
) 

#-- Time series plot for ULSD: Pump price (p/litre)
g1 <- ggplot(data, aes(x = date, y = ulsd_pump_price_p_litre)) +
  geom_line() +
  labs(title = "ULSD: Pump price (p/litre)",
       caption = "Source: GOV.UK 2023",
       x = "Year",
       y = "p/litre") +
  theme_classic() +
  annotate(geom = "text",
           x = as.Date("2008-08-25", format = "%Y-%m-%d"), y = 70, 
           label = "2008\nEconomic Crisis?",
           family = "Arial Narrow",
           colour = "red",
           size = 3, fontface = "bold") +
  annotate(geom = "rect",
           xmin = as.Date("2008-08-25", format = "%Y-%m-%d"), xmax = as.Date("2009-08-25", format = "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = .4)
g1

g2 <- ggplot(data, aes(x = date, y = ulsd_diff_on_previous_week_p_litre)) +
  geom_line() +
  labs(title = "ULSD:  Pump price (p/litre)",
       subtitle="Diff on previous WEEK (p/litre)",
       caption = "Source: GOV.UK",
       x = "Year",
       y = "p/litre") +
  theme_classic()
g2

g3 <- ggplot(data, aes(date, ulsd_diff_on_previous_year_p_litre)) +
  geom_line() +
  labs(title = "ULSD:  Pump price (p/litre)",
       subtitle = "Diff on previous  YEAR (p/litre)",
       caption = "Source: GOV.UK 2023",
       x = "Year",
       y = "p/litre") +
  theme_classic()

g3

## combine time series and diff 
g1 <- ggplot(data, aes(x = date, y = ulsd_pump_price_p_litre)) +
  geom_line() +
  labs(title    = "ULSD:  Pump price (p/litre)",
       subtitle = "Weekly Prices time series (from 2003)",
       y        = "p/litre") + 
  theme_classic() + 
  theme_update(axis.title.x = element_blank()) 


g2 <- ggplot(data, aes(x = date, y = ulsd_diff_on_previous_week_p_litre)) +
  geom_line() +
  labs(subtitle  ="Diff on previous WEEK (p/litre)",
       y         = "p/litre") + 
  theme_classic() + 
  theme_update(axis.title.x = element_blank()) 


g3 <- ggplot(data, aes(date, ulsd_diff_on_previous_year_p_litre)) +
  geom_line() +
  labs(subtitle  = "Diff on previous  YEAR (p/litre)",
       y         = "p/litre") + 
  theme_classic() + 
  theme_update(axis.title.x = element_blank()) 
ggpubr::ggarrange(g1,  # First row with line plot
                  ggarrange(g2, g3, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
                  nrow   = 2,
                  labels = "A"
) 

#--- Comparison of ULSP & ULSD
dt <- data
dt1 <- dt %>% select(c(1:6))
str(dt1)

dt1 <- dt1 %>%
  add_column(fuel_type = "ULSP") %>% rename("pump_price"   = "ulsp_pump_price_p_litre",   # new = old 
                                            "diff_week"    = "ulsp_diff_on_previous_week_p_litre",
                                            "diff_year"    = "ulsp_diff_on_previous_year_p_litre",
                                            "duty_rate"    = "duty_rate_ulsp_p_litre",
                                            "vat_per_rate" = "vat_percent_rate_ulsp")

dt2 <- dt %>% select(1, 7:11)
dt2 <- dt2 %>%
  add_column(fuel_type = "ULSD") %>% rename("pump_price"   = "ulsd_pump_price_p_litre",
                                            "diff_week"    = "ulsd_diff_on_previous_week_p_litre",
                                            "diff_year"    = "ulsd_diff_on_previous_year_p_litre",
                                            "duty_rate"    = "duty_rate_ulsd_p_litre",
                                            "vat_per_rate" = "vat_percent_rate_ulsd")

dt <- bind_rows(dt1, dt2)

dt$fuel_type <- as.factor(dt$fuel_type)
str(dt)


#--- Compare ULSP vs ULSD 
g1 <- ggplot(dt, aes(date, pump_price, color = fuel_type)) +  # using linetype = 
  geom_line(show.legend = FALSE) +
  labs(title    = "Pump price",
       subtitle = "Weekly Prices time series (from 2023): ULSP vs ULSD",
       y        = "p/litre",
       x        = "date") + 
  theme_classic() +
  theme_update(axis.title.x = element_blank())

g2 <- ggplot(data, aes(x = date, y = ulsp_pump_price_p_litre)) +
  geom_line(color = "lightblue") +
  labs(title    = "ULSP:  Pump price",
       y        = "p/litre",
       x        = "date") + 
  theme_classic() +
  theme_update(axis.title.x = element_blank())

g3 <- ggplot(data, aes(x = date, y = ulsd_pump_price_p_litre)) +
  geom_line(color = "red") +
  labs(title = "ULSD: Pump price",
       y        = "p/litre",
       x        = "date",
       caption = "https://www.gov.uk/government/statistics/weekly-road-fuel-prices") + 
  theme_classic() +
  theme_update(axis.title.x = element_blank())

gridExtra::grid.arrange(g1, g2, g3, ncol = 1, nrow = 3) # arrange plot in 1 same page

## Trend by month per year from 2003 - 2023 
data <- mydata %>% janitor::clean_names()
df <- data %>% filter(date >= "2004-01-05" & date <= "2023-01-05")
df <- df %>% select(c(1:2))

df <- df %>% dplyr::mutate(year = lubridate::year(date), 
                           month = lubridate::month(date), 
                           day = lubridate::day(date)
)

df$date <- as.Date(df$date, format = "%Y-%m-%d") 

df$week <- isoweek(ymd(df$date)) # calculate number of week 

df <- df %>% 
  group_by(week) %>% 
  mutate(year_week = first(date))  # week of month 

df %>%  # viz
  ggplot(aes(year_week, ulsp_pump_price_p_litre, color = factor(year))) +
  geom_line() +
  scale_x_date(date_breaks="1 month", date_labels="%b") + 
  geom_label(aes(label = factor(year)),
             data = df %>% group_by(year) %>% filter(date == max(date)),
             nudge_x = 0.35,
             size = 4)+
  labs(title = "ULSP:  Pump price (p/litre)",
       caption = "Source: GOV.UK 2023",
       x = " ",
       y = "p/litre") +
  theme_classic() +
  theme(legend.position="none")

## data table (tibble)
df_q <- data %>% select(c(1:2))

df_q <- df_q %>%
  group_by(quarter = zoo::as.yearqtr(date)) %>% # date to quater 
  summarise(mp = median(ulsp_pump_price_p_litre), .groups = 'drop') %>%   
  mutate(diff= lead(mp) - mp)

df_q$quarter <- as.character(df_q$quarter) 
a <- stringr::str_split_fixed(df_q$quarter, " ", 2) 
df_q <- cbind(a, df_q)

names(df_q)[1] <- "year"
names(df_q)[2] <- "quater"
df_q$quarter <- NULL

df_w <- df_q %>% 
  pivot_wider(names_from = quater, 
              values_from = c(mp, diff),
              values_fill = 0)

df_w <- df_w %>% mutate(across(where(is.numeric), ~round(., 1)))

head(df_w, 10)

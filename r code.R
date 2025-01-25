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
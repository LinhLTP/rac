# Load required libraries using pacman
pacman::p_load(
  # Data Import and Management
  rio,           # Import/export data
  here,          # Manage file paths
  tidyverse,     # Data manipulation and visualisation
  data.table,    # Efficient data handling
  zoo,           # Time series manipulation
  tidyr,         # Data tidying
  lubridate,     # Date and time handling
  
  # Data Visualisation
  ggplot2,       # Core visualisation package (includes ggarrange())
  plotly,        # Interactive visualisation
  gganimate,     # Animated visualisations
  ggExtra,       # Additional ggplot2 components
  ggalt,         # Alternatives for ggplot2
  ggthemes,      # Additional themes for ggplot2
  ggpubr,        # Publication-ready plots
  ggcorrplot,    # Correlation matrix visualisation
  ggpmisc,       # Add stat_valey or annotations
  ggalluvial,    # Sankey charts and alluvial diagrams
  ggrepel,       # Avoid overlapping points in plots
  RColorBrewer,  # Colour palettes
  viridis,       # Colour palettes for better perceptual design
  
  # Specific Visualisation Features
  CGPfunctions,  # Slope graphs and other functions
  formattable    # Formatting tables
)

#-- Data viz with Map 

# Load world map data
world_map <- map_data("world")  
colnames(dtam)[2] <- "region"

dtam <- dtam %>%
  mutate(region = R.utils::capitalize(region))  # Ensure proper capitalisation

# Join 'dtam' with 'world_map' to merge region data
dt_map <- left_join(
  dtam,
  world_map,
  by = "region",
  relationship = "many-to-many"  # Specify relationship to avoid warnings
)

# Visualisation 
## One year 
dtest <- dt.map %>% filter(year %in% c("2020"))

excl <- ggplot(dtest, aes(long, lat, group = group))+
  geom_polygon(aes(fill = price_excl_tax ), color = "white")+
  scale_fill_viridis_c(option = "D") +
  labs(title = " ",
       subtitle = "Price excl tax - Pence (p/litre)",
       x = "", y = "") +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  coord_fixed() + 
  theme_void() +
  theme(
    legend.justification = c(0, 1),
    legend.position = c(1, .50)) 

incl <- ggplot(dtest, aes(long, lat, group = group))+
  geom_polygon(aes(fill = price_incl_tax ), color = "white")+
  scale_fill_viridis_c(option = "D") +
  labs(title = " ",
       subtitle = "Price incl tax - Pence (p/litre)",
       x = "", y = "") +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  coord_fixed() + 
  theme_void() +
  theme(
    legend.justification = c(0, 1),
    legend.position = c(1, .50)) 

incl <- ggplot(dtest, aes(long, lat, group = group))+
  geom_polygon(aes(fill = price_incl_tax ), color = "white")+
  scale_fill_viridis_c(option = "D") +
  labs(title = " ",
       subtitle = "Price incl tax - Pence (p/litre)",
       x = "", y = "") +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  coord_fixed() + 
  theme_void()  +
  theme(
    legend.justification = c(0, 1),
    legend.position = c(1, .50)) 

figure <- ggarrange(excl, incl, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

annotate_figure(figure,
                top = text_grob("2020 EU Nation Fuel Price", color = "black", face = "bold", size = 14),
                bottom = text_grob("Data source: GOV.UK 2023", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Figure arranged using ggpubr", color = "grey", rot = 90, size = 8),
                right = " ",
                fig.lab = "Figure 1", fig.lab.face = "bold"
)

## One more years 
### data preparation 
years <- c("2017", "2018", "2019", "2020", "2021", "2022")  # Filter data by year using a loop

dt_by_year <- lapply(years, function(y) {                   # create list of df for each year
  dt.map %>% filter(year == y)
})

names(dt_by_year) <- years                                  # Assign filtered df to individual variables

list2env(dt_by_year, envir = .GlobalEnv)

### data viz 
g17 <- ggplot(dt2017, aes(long, lat, group = group))+
  geom_polygon(aes(fill = price_excl_tax ), color = "white")+
  scale_fill_viridis_c(option = "C") +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  coord_fixed() + 
  theme_void() +
  theme(legend.position = "none") +
  labs(subtitle = "2017",
       x = "", y = "")

g18 <- ggplot(dt2018, aes(long, lat, group = group))+
  geom_polygon(aes(fill = price_excl_tax ), color = "white")+
  scale_fill_viridis_c(option = "C") +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  coord_fixed() + 
  theme_void() +
  theme(legend.position = "none") +
  labs(subtitle = "2018",
       x = "", y = "")

g19 <- ggplot(dt2019, aes(long, lat, group = group))+
  geom_polygon(aes(fill = price_excl_tax ), color = "white")+
  scale_fill_viridis_c(option = "D") +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  coord_fixed() + 
  theme_void() +
  theme(legend.position = "none") +
  labs(subtitle = "2019",
       x = "", y = "")

g20 <- ggplot(dt2020, aes(long, lat, group = group))+
  geom_polygon(aes(fill = price_excl_tax ), color = "white")+
  scale_fill_viridis_c(option = "D") +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  coord_fixed() + 
  theme_void() +
  theme(legend.position = "none") +
  labs(subtitle = "2020",
       x = "", y = "")

g21 <- ggplot(dt2021, aes(long, lat, group = group))+
  geom_polygon(aes(fill = price_excl_tax ), color = "white")+
  scale_fill_viridis_c(option = "D") +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  coord_fixed() + 
  theme_void() +
  theme(legend.position = "none") +
  labs(subtitle = "2021",
       x = "", y = "")

g22 <- ggplot(dt2022, aes(long, lat, group = group))+
  geom_polygon(aes(fill = price_excl_tax ), color = "white")+
  scale_fill_viridis_c(option = "D") +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  coord_fixed() + 
  theme_void() +
  theme(legend.position = "none") +
  labs(subtitle = "2022",
       x = "", y = "")
ggarrange(g19, g20, g21, g22, ncol=2, nrow=2, common.legend = TRUE, legend="right")

figure.2 <- ggarrange(g19, g20, g21, g22, ncol=2, nrow=2, common.legend = TRUE, legend="right")

annotate_figure(figure.2,
                top = text_grob("2020 EU Nation Fuel Price", color = "black", face = "bold", size = 14),
                bottom = text_grob("Data source: GOV.UK 2023", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Figure for (mean) price excluding tax & duty", color = "purple", rot = 90, size = 8),
                right = " "                
)


#--- Data viz with slope graph & sankey chart 

# Slope graph
dtslop <- data %>%              # wide to long format 
  pivot_longer(
    cols = belgium:slovenia,
    names_to = "country",
    values_to = "price_excl_tax"
  )

dtslop$price_excl_tax <- round(dtslop$price_excl_tax, 1)

str(dtslop)

## "econ" theme 
t <- dtslop %>% filter(year %in% c("2019", "2020", "2021", "2022"))

CGPfunctions::newggslopegraph(t, year, price_excl_tax, country,
                              Title = "Premium unleaded petrol prices excluding tax and duty",
                              SubTitle = "2019-2023",
                              Caption = "By R CHARTS",
                              CaptionJustify = "left",
                              ThemeChoice = "econ",
                              DataTextSize = 2.5,
                              DataLabelPadding = .05)

## “wsj” theme
t <- t %>% filter(country %in% c("UK", "sweden", "france", "netherlands", "belgium", "luxembourg", "germany", "greece", "malta", "italy", "findland")) 

CGPfunctions::newggslopegraph(t, year, price_excl_tax, country,
                              Title = "Premium unleaded petrol prices excluding tax and duty",
                              SubTitle = "2019-2023",
                              Caption = "By R CHARTS",
                              CaptionJustify = "left",
                              ThemeChoice = "wsj",
                              DataTextSize = 2.5,
                              DataLabelPadding = .05)


# Compare price excl - incl tax & duty
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

data2 <- data2 %>%         # avg price 
  group_by(year) %>% 
  summarise(across(2:28, ~ mean(.x, na.rm = TRUE)))

dtslop2 <- data2 %>%       # long to wide 
  pivot_longer(
    cols = belgium:slovenia,
    names_to = "country",
    values_to = "price_incl_tax"
  )

dtslop2$price_incl_tax <- round(dtslop2$price_incl_tax, 0)

dtam <- cbind(dtslop, dtslop2$price_incl_tax) 
colnames(dtam)[4] <- 'price_incl_tax'
dtam$price_incl_tax <- round(dtam$price_incl_tax, 1)

b <- dtam %>% filter(year %in% c("2022")) 

c <- b %>% 
  pivot_longer(
    cols = price_excl_tax:price_incl_tax,
    names_to = "type",
    values_to = "price"
  ) %>% 
  select(-c(year))

ggplot(c, aes(x = reorder(country,-price), y = price, fill = type)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer() +
  guides(fill = guide_legend(title = "Title")) +
  labs(title = "Pump Price of Unleaded Petrol as in 2022",
       x = " ",
       y = "Pence (p/litre)") +
  theme_classic() +
  coord_flip()


# Sankey chart for ranking changes
## data preparation 
data <- mydata %>% janitor::clean_names()   # clean cols name
data <- data[complete.cases(data), ]

data <- mydata %>%
  janitor::clean_names() %>%   # Clean column names
  drop_na()

data <- data %>%
  select(
    -month, 
    -day_in_month_of_price_snapshot, 
    -uk_rank_in_eu14_plus_uk, 
    -uk_rank_in_eu27_plus_uk
  )

data <- data %>%
  mutate(year = as.factor(year))

data <- data %>%
  group_by(year) %>%
  summarise(across(2:28, ~ mean(.x, na.rm = TRUE)))

## Ranking the data 
dt_rank <- cbind(data[1], 
                 t(apply(-data[-1], 1, rank, ties.method='min', na.last='keep')))  

colnames(dt_rank)[15] <- 'UK' # rename 

dt <- dt_rank %>% 
  pivot_longer(
    cols = belgium:slovenia,
    names_to = "country",
    values_to = "rank"
  )

## Change variable classes (required for Sankey chart)
dt <- dt %>%
  mutate(
    year = as.character(year),              
    country = as.character(country),        
    rank = as.factor(rank)                  
  )

str(dt)

## viz
ggplot(dt, aes(x = year, stratum = rank, alluvium = country, fill = country, label = country)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft", color = "darkgray") +
  geom_stratum() +
  theme(legend.position = "bottom") +
  ggtitle("Fuel price rank changes") +
  scale_fill_viridis_d(direction = -1) +
  theme_classic()

hint:   git config pull.rebase true   # rebase
hint:   git config pull.ff only       # fast-forward only
#### Setup ####
library(tidyverse)
library(maps)
library(rstudioapi)

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

# Assumes data is in same directory
data <- read_csv("air.csv")
airlinenames <- read_csv("airlinenames.csv")
source('airports.R')


#### Data Wrangling ####
# Subset airports data to only include columns of interest
airports <- airports %>%
  select(iata_code, latitude_deg, longitude_deg)

# List of airpots not in continental US to be removed
remov_airport <- c('HNL', 'OGG', 'LIH', 'KOA', 'ANC', 'STT', 'STX', 'SJU')

# Subset data to only include columns we need and add in origin/destination coordinates
flightdata <- data %>%
  select(Dest, Origin, Carrier = UniqueCarrier) %>%
  left_join(airports, by = c('Dest' = 'iata_code')) %>%
  rename(lat.dest = latitude_deg, long.dest = longitude_deg) %>%
  left_join(airports, by = c('Origin' = 'iata_code')) %>%
  rename(lat.org = latitude_deg, long.org = longitude_deg) %>%
  unique() %>%
  filter(!Dest %in% remov_airport & !Origin %in% remov_airport)

# Create map of US on which to graph results
usamap <- borders("usa", colour="grey100", fill="grey100")


#### Southwest Airlines ####
# Subset flightdata to only include WN
WN_data <- flightdata %>%
  filter(Carrier == 'WN')

# Plot data
ggplot() + usamap + 
  geom_curve(data=WN_data, aes(x = long.org, y = lat.org, xend = long.dest, yend = lat.dest), alpha = 0.2, size = 0.3, color = "red3") + 
  geom_point(data=WN_data, aes(x = long.dest, y = lat.dest), alpha = 0.8, size = 0.1, colour = "red3") + 
  theme(panel.background = element_rect(fill="skyblue1"), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
    annotate("text", x = -130, y = 26, hjust = 0, size = 14,
              label = paste("Southwest Airlines"), color = "white", family = "Helvetica Black")

# Export
ggsave('WN_flights.png', width = 12, height = 8, units = 'in')


#### United Airline ####
# Subset flightdata to only include UA
UA_data <- flightdata %>%
  filter(Carrier == 'UA')

# Plot
ggplot() + usamap + 
  geom_curve(data=UA_data, aes(x = long.org, y = lat.org, xend = long.dest, yend = lat.dest), alpha = 0.2, size = 0.3, color = "red3") + 
  geom_point(data=UA_data, aes(x = long.dest, y = lat.dest), alpha = 0.8, size = 0.1, colour = "red3") + 
  theme(panel.background = element_rect(fill="skyblue1"), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
    annotate("text", x = -130, y = 26, hjust = 0, size = 14,
              label = paste("United Airlines"), color = "white", family = "Helvetica Black")

# Export
ggsave('UA_flights.png', width = 12, height = 8, units = 'in')


#### Thanksgiving Data ####
# Generate data for Thanksgiving 2008
flights_TG <- data %>% 
  select(UniqueCarrier, ArrDelay, Month, DayofMonth) %>%
  left_join(airlinenames, by = c('UniqueCarrier' = 'iata_code')) %>%
  filter(Month == 11) %>%
  filter(DayofMonth >= 24 & DayofMonth <= 30) %>%
  mutate(delay = ifelse(ArrDelay > 5, 1, 0)) %>%
  group_by(name) %>%
  summarise(flightdelay_percent = mean(delay, na.rm = T)*100)

# Plot
ggplot(flights_TG, aes(x = reorder(name, +flightdelay_percent ), y = flightdelay_percent, 
             fill = ifelse(name=='Southwest', T, F))) + 
  scale_fill_manual(values = c('grey60', 'blue2')) +
  geom_col() +
  geom_text(aes(label=name),position = position_stack(0.45), angle = 90, col='white') +
  labs(title = 'Percentage of Late Arrivals by Airline', 
       subtitle = 'Thanksgiving 2008') +
  theme(panel.background = element_rect(fill="white"),
        legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# Export
ggsave("delay_graph.png", width = 12, height = 8, units = 'in')

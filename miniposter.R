#### Setup ####

library(tidyverse)
library(ggrepel)
library(maps)

setwd("C:/Users/tarnd/Google Drive/School Stuff/DATA 550/miniposter")
data <- read_csv("air.csv")
source('airports.R')

#### Delay Plots ####
data %>%
  mutate(hours = DepTime%/%100) %>%
  group_by(hours) %>%
  filter(!is.na(ArrDelay)) %>%
  summarise(delay = mean(ArrDelay >= 0, na.rm = T)) %>%
  ggplot(aes(hours, delay, fill = delay)) + geom_col()

data %>%
  mutate(hours = DepTime%/%100) %>%
  group_by(hours) %>%
  filter(!is.na(ArrDelay)) %>%
  summarise(delay = mean(ArrDelay, na.rm = T)) %>%
  ggplot(aes(hours, delay, fill = delay)) + geom_col()

#### Data Wrangling ####

airports <- airports %>%
  select(iata_code, latitude_deg, longitude_deg)

flightdata <- data %>%
  select(Dest, Origin, Carrier = UniqueCarrier) %>%
  left_join(airports, by = c('Dest' = 'iata_code')) %>%
  rename(lat.dest = latitude_deg, long.dest = longitude_deg) %>%
  left_join(airports, by = c('Origin' = 'iata_code')) %>%
  rename(lat.org = latitude_deg, long.org = longitude_deg) %>%
  unique()

#### Southwest Airlines ####

WN_data <- flightdata %>%
  filter(Carrier == 'WN')

airport_plot <- WN_data %>%
  select(Dest, lat.dest, long.dest) %>%
  unique()

ggplot() + usamap + 
  geom_curve(data=WN_data, aes(x = long.org, y = lat.org, xend = long.dest, yend = lat.dest), alpha = 0.2, size = 0.3, color = "#f9ba00") + 
  geom_point(data=WN_data, aes(x = long.dest, y = lat.dest), alpha = 0.8, size = 0.1, colour = "#f9ba00") + 
  geom_text_repel(data=airport_plot, aes(x = long.dest, y = lat.dest, label = airport_plot$Dest), col = "white", size = 2, segment.color = NA) +  
  theme(panel.background = element_rect(fill="#101046"), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

ggsave('WN_flights.png', width = 10, height = 8, units = 'in')

#### American Airlines ####

remov_airport <- c('HNL', 'OGG', 'LIH', 'KOA', 'ANC', 'STT', 'STX', 'SJU')

AA_data <- flightdata %>%
  filter(Carrier == 'AA') %>%
  filter(!Dest %in% remov_airport & !Origin %in% remov_airport)

airport_plot <- AA_data %>%
  select(Dest, lat.dest, long.dest) %>%
  unique()
  

ggplot() + usamap +
  geom_curve(data=AA_data, aes(x = long.org, y = lat.org, xend = long.dest, yend = lat.dest), alpha = 0.2, size = 0.3, color = "#f9ba00") + 
  geom_point(data=AA_data, aes(x = long.dest, y = lat.dest), alpha = 0.8, size = 0.1, colour = "#f9ba00") + 
  geom_text_repel(data=airport_plot, aes(x = long.dest, y = lat.dest, label = airport_plot$Dest), col = "white", size = 2, segment.color = NA) + 
  theme(panel.background = element_rect(fill="#101046"), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

ggsave("AA_flights.png", width = 10, height = 8, units = 'in')


#### United Airline ####
UA_data <- flightdata %>%
  filter(Carrier == 'UA')

remov_airport <- c('HNL', 'OGG', 'LIH', 'KOA', 'ANC', 'STT', 'STX', 'SJU')
UA_data <- flightdata %>%
  filter(Carrier == 'UA') %>%
  filter(!Dest %in% remov_airport & !Origin %in% remov_airport)

airport_plot <- UA_data %>%
  select(Dest, lat.dest, long.dest) %>%
  unique()

ggplot() + usamap + 
  geom_curve(data=UA_data, aes(x = long.org, y = lat.org, xend = long.dest, yend = lat.dest), alpha = 0.2, size = 0.3, color = "#f9ba00") + 
  geom_point(data=UA_data, aes(x = long.dest, y = lat.dest), alpha = 0.8, size = 0.1, colour = "#f9ba00") + 
  geom_text_repel(data=airport_plot, aes(x = long.dest, y = lat.dest, label = airport_plot$Dest), col = "white", size = 2, segment.color = NA) +  
  theme(panel.background = element_rect(fill="#101046"), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

ggsave('UA_flights.png', width = 10, height = 8, units = 'in')

#### Thanksgiving Data ####
data %>% 
  select(UniqueCarrier, ArrDelay, Month, DayofMonth) %>%
  filter(Month == 11) %>%
  filter(DayofMonth >= 24 & DayofMonth <= 30) %>%
  group_by(UniqueCarrier) %>%
  summarise(delay = mean(ArrDelay > 0, na.rm = T)) %>%
  ggplot(aes(x = reorder(UniqueCarrier, +delay), y=delay, fill = delay)) + geom_col()


flights_TG <- data %>% 
  select(UniqueCarrier, ArrDelay, Month, DayofMonth) %>%
  filter(Month == 11) %>%
  filter(DayofMonth >= 24 & DayofMonth <= 30) %>%
  mutate(delay = ifelse(ArrDelay > 0, 1, 0)) %>%
  group_by(UniqueCarrier) %>%
  summarise(flightdelay_percent = mean(delay, na.rm = T)) %>%
  ggplot(aes(x = reorder(UniqueCarrier, +flightdelay_percent ), y=flightdelay_percent, fill = flightdelay_percent)) + 
  geom_col() +labs(x = 'Carrier', y = 'Percentage of Flights Delayed')
  
ggsave("delay_graph.png", width = 12, height = 8, units = 'in')


#### Graphing all WN, AA, UA on one plot ####

ggplot() + usamap + 
  geom_curve(data=WN_data, aes(x = long.org, y = lat.org, xend = long.dest, yend = lat.dest), alpha = 0.2, size = 0.3, color = "#f9ba00") +
  geom_curve(data=AA_data, aes(x = long.org, y = lat.org, xend = long.dest, yend = lat.dest), alpha = 0.2, size = 0.3, color = "#ff0000") +
  geom_curve(data=UA_data, aes(x = long.org, y = lat.org, xend = long.dest, yend = lat.dest), alpha = 0.2, size = 0.3, color = "#ff0000") + 
  geom_point(data=UA_data, aes(x = long.dest, y = lat.dest), alpha = 0.8, size = 0.1, colour = "#f9ba00") + 
  geom_text_repel(data=airport_plot, aes(x = long.dest, y = lat.dest, label = airport_plot$Dest), col = "white", size = 2, segment.color = NA) +  
  theme(panel.background = element_rect(fill="#101046"), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

ggsave("ALL_flights.png", width = 10, height = 8, units = 'in')

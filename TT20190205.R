library(tidyverse)
library(janitor)
library(readr)
library(tidylog)

state_hpi <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/state_hpi.csv")
mortgage_rates <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/mortgage.csv")
recession_dates <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/recessions.csv")


# Initial quick and dirty look
mortgage_rates %>% 
  ggplot(aes(x = date, y = fixed_rate_30_yr)) +
  geom_point() +
  theme_bw()

state_hpi %>% 
  ggplot(aes(x = year, y = price_index, color = state)) +
  geom_point() +
  theme_bw()

# Look at state_hpi
ssy_statehpi<- state_hpi %>% 
  arrange(state,year,month) %>% 
  group_by(state,year) %>% 
  summarise(point_change = first(price_index) - last(price_index)) 

ssy_statehpi %>% 
  ggplot(aes(x = year, y = point_change, color = state)) +
  geom_line() +
  theme_bw() +
  labs(title = "State HPI")

# Ok maybe not the most useful metric, change the calculation

ssy_statehpi<- state_hpi %>% 
  arrange(state,year,month) %>% 
  group_by(state,year) %>% 
  summarise(
    percent_change = (first(price_index/us_avg) - 
                           last(price_index/us_avg))*100) 

ssy_statehpi %>% 
  ggplot(aes(x = year, y = percent_change, color = state)) +
  geom_line() +
  theme_bw() +
  labs(title = "State HPI")

# Trying a different way of thinking

state_hpiPercent<- state_hpi %>% 
  arrange(state,year,month) %>% 
  mutate(percent_of_us = (price_index/us_avg)*100)

state_hpiPercent %>% 
  ggplot(aes(x = year, y = percent_of_us, color = state)) +
  geom_line() +
  theme_bw() +
  labs(title = "State HPI")

# Something definitely happened in 2000?

state_hpiPercent %>% 
  ggplot(aes(x = year, y = percent_of_us)) +
  geom_line() +
  facet_wrap(~state, scales = "free") +
  theme_bw() +
  labs(title = "State HPI")

# ok this seems more useful. move from monthly to yearly

state_hpiPerYear<- state_hpiPercent %>% 
  arrange(state,year,month) %>% 
  group_by(state,year) %>% 
  summarise(Jan = first(percent_of_us), 
            Dec = last(percent_of_us),
            Lowest = min(percent_of_us),
            Highest = max(percent_of_us)) %>% 
  mutate(percent_change = Jan - Dec)

state_hpiPerYear %>% 
  ggplot(aes(x = year, y = percent_change, color = state)) +
  geom_line() +
  geom_pointrange(aes(ymin = Lowest,ymax = Highest)) +
  theme_bw() +
  labs(title = "State HPI")


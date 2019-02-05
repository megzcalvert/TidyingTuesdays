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
  summarise(percentage_change = first(price_index) - last(price_index))

ssy_statehpi %>% 
  ggplot(aes(x = year, y = percentage_change, color = state)) +
  geom_line() +
  theme_bw()

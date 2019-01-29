library(tidyverse)
library(RCurl)
library(usmap)
library(maps)
library(gganimate)

###############################################################################
# Tidy Tuesday data about all things dairy
###############################################################################

# Read in all data available
clean_cheese<- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/clean_cheese.csv")

fmilk_sales<- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/fluid_milk_sales.csv")

milk_products<- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/milk_products_facts.csv")

cow_facts<- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/milkcow_facts.csv")

state_milkprod<- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/state_milk_production.csv")


clean_cheese %>% str()
fmilk_sales %>% str()
milk_products %>% str()
cow_facts %>% str()
state_milkprod %>% str()

###############################################################################
####  The great state of Wisconsin will not apologise for it's cheese!    ####
###############################################################################
unique(state_milkprod$region)

# Easier to judge based on color or facet
state_milkprod %>% 
  ggplot(aes(year,milk_produced,color=region)) +
  geom_point() +
  scale_color_manual(values = c('#7f3b08','#b35806','#e08214','#fdb863',
                                '#fee0b6','#d8daeb','#b2abd2','#8073ac',
                                '#542788','#2d004b')) +
  theme_bw()

state_milkprod %>% 
  ggplot(aes(year,milk_produced,color=region)) +
  geom_point() +
  facet_wrap(~region, scales = "free", ncol = 3) +
  scale_color_manual(values = c('#7f3b08','#b35806','#e08214','#fdb863',
                                '#fee0b6','#d8daeb','#b2abd2','#8073ac',
                                '#542788','#2d004b')) +
  theme_bw()

# I'm keeping both
# which region produces the most milk
regionalProduction<- state_milkprod %>% 
  group_by(region,year) %>% 
  summarise(milk_prod = sum(milk_produced))

us_states <- map_data("state")
head(us_states)

mapData<- state_milkprod %>% 
  select(region,state,year) %>%
  left_join(regionalProduction) 
  mapData$state<- tolower(mapData$state)
mapData<- left_join(us_states,mapData, by = c("region" = "state"))

 mapData %>%
  filter(year == "1970") %>% 
  ggplot(aes(x = long, y = lat,
             group = group,
             fill = milk_prod,
             frame = year)) +
  geom_polygon(color = "gray70", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_gradient(low = 'white', 
                     #  mid = '#f7f7f7',
                       high = '#542788') +
  labs(fill = "Milk produced") +
  theme(legend.position = "bottom")

 
 




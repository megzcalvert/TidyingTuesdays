library(tidyverse)
library(RCurl)

x <- getURL("https://raw.githubusercontent.com/vera-institute/incarceration_trends/master/incarceration_trends.csv")
rawData<- read.csv(text = x)
colnames(rawData)

str(rawData)

unique(rawData$state)
unique(rawData$year)

##### Examine individuals who are in prison pre-trial
preTrial<- rawData %>% 
  select(year,state,total_pop,urbanicity,region,division,county_name,
         ends_with("crime"),ends_with("pretrial")) %>%
  filter(total_jail_pretrial > 0)

# what is the pre-trial population doing in the different regions by year
ggplot(preTrial, aes(year,total_jail_pretrial)) +
  geom_point() +
  facet_wrap("region", scales = "free") +
  theme_bw()

# I live in the midwest so lets look at the midwest and how much difference
# the urban area makes
preTrial %>% 
  filter(region == "Midwest") %>%
  ggplot(aes(year,total_jail_pretrial, color=urbanicity)) + 
  geom_point(aes(shape=division)) +
  facet_wrap("state", scales = "free") +
  theme_bw()

# Deeper into Kansas we go...
preTrial %>% 
  filter(state == "KS") %>%
  ggplot(aes(year,total_jail_pretrial, color=urbanicity)) + 
  geom_point(aes(shape=division)) +
  facet_wrap("county_name", scales = "free") +
  theme_bw()
#not really helpful

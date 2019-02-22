library(tidyverse)
library(janitor)
library(readr)
library(tidylog)

phd_field<- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")
glimpse(phd_field)
unique(phd_field$broad_field)
unique(phd_field$major_field)
unique(phd_field$field)
unique(phd_field$year)

phd_field %>% 
  ggplot(aes(x = year, y = n_phds, colour = broad_field)) +
  geom_point() +
  facet_wrap(~broad_field, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")

phd_field$major_field<- as.factor(phd_field$major_field)
phd_field$field<- as.factor(phd_field$field)

fit<- phd_field %>% 
  group_by(broad_field) %>% 
  nest %>% 
  mutate(reg = map(data, ~lm(n_phds ~ major_field + field,
                             data = .x)),
         tidied = map(reg,tidy)) %>% 
  unnest()

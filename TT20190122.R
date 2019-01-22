library(tidyverse)
library(RCurl)

x <- getURL("https://raw.githubusercontent.com/vera-institute/incarceration_trends/master/incarceration_trends.csv")
rawData<- read.csv(text = x)
colnames(rawData)

str(rawData)

unique(rawData$state)
unique(rawData$year)

# load packages
library(tidyverse)
library(tidytuesdayR)

# get the dah-ta
tuesdata <- tidytuesdayR::tt_load(2021, week = 32)
View(tuesdata)

athletes <- tuesdata$athletes

glimpse(athletes)

# load packages
library(tidytuesdayR)
library(tidyverse)
library(here)

# load in different way to use na = "NULL"
scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv', na = "NULL")
glimpse(scoobydoo)

# EDA

scoobydoo %>%
  count(monster_type)

# str_trim removes leading space
# recode can't do because leading blank wouldn't read
tidyscooby <- scoobydoo %>%
  separate_rows(monster_type, sep = ",", convert = TRUE) %>%
  separate_rows(culprit_gender, sep = ",", convert = TRUE) %>%
  drop_na(monster_type, motive) %>%
  filter(monster_type != "",
         culprit_gender != "",
         culprit_gender != "None") %>%
  mutate(monster_type = str_trim(monster_type),
         culprit_gender = str_trim(culprit_gender)) %>% 
  mutate(monster_type = recode(monster_type,
                               Disguise = "Disguised",
                               Disugised = "Disguised",
                               Possessed = "Possessed Object"),
         across(where(is.character), factor)) 

# visualize
tidyscooby %>%
  group_by(motive, culprit_gender) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(5) %>%
  View()


# visualize
tidyscooby %>%
  group_by(motive, culprit_gender) %>%
  summarise(count = n()) %>%
  ggplot(aes(reorder(motive, count), fill = culprit_gender, count)) +
  geom_col(position = "dodge") +
  coord_flip() +
  # facet_wrap(~ culprit_gender, dir = "v") +
  theme_minimal() 


# hot mess

# load in data
# tuesdata <- tidytuesdayR::tt_load(2021, week = 29)
# scooby <- tuesdata$scoobydoo




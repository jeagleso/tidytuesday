# set up
library(tidytuesdayR)
library(tidyverse)
library(readr)
library(here)

# load data
water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')

# quick eda
glimpse(water)

# look at the unique values of water_source
water %>%
  select(water_source) %>%
  unique()

# how many N/As are there?
sum(is.na(water$water_source)) 

# 62714 sounds like a lot but there are ~470k entries so I guess we can remove

# create a bar chart
water %>%
  ggplot(aes(x = water_source)) +
  geom_bar() +
  coord_flip()

# I want to sort by order - following what @kierisi did this creates another 
# dataframe thing with a row for each source and count
water_source <- water %>%
  group_by(water_source) %>%
  summarize(count = n())

water_source %>%
  ggplot(aes(x = reorder(water_source, count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip()

# remove N/As
water_source %>%
  drop_na(water_source) %>%
  ggplot(aes(x = reorder(water_source, count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip()

# IT WORKED! 
# You don't have to specify column for drop_na but that would remove all NA's from all cols

# now just going to add formatting
water_source %>%
  mutate(highlight_borehole = ifelse(water_source == 'Borehole', T, F)) %>%
  drop_na(water_source) %>%
  ggplot(aes(x = reorder(water_source, count), y = count, fill = highlight_borehole)) +
  scale_fill_manual(values = c('#D9D9D9', '#38a3a5')) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Boreholes are the most common type of water source",
       caption =  "Design: Jenna Eagleson | Source: wpdx | #TidyTuesday 2021W19") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0, color = '#38a3a5'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") 

# save plot
ggsave(here("images", "2021-05-05_tt_water_sources.pdf"), width = 10, height = 7, last_plot())

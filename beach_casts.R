# beach_casts.R
# Read in and analyse Carlin's beach cast data
# AJ Smit
# 11 May 2018

# NOTES: I had to edit "project_data.csv" because there were unnecessary spaces in there.
# Please pay attention to detail during data entry!
# Getting the data into the right format that's conducive to analysis was not straight forward (see below);
# to endure forward compatibility with this script, please retain the same data format when the data are
# recorded in the field, and when data are entered (and pay attention to detail and avoid extra spaces!).

library(tidyverse)

casts.in <- read.csv2("project_data.csv") # `read_csv2` gives unexpected results

# Parse the data into the right format
casts <- casts.in %>%
  mutate(date = as.Date(date, "%d-%b-%y")) %>% # because the dates were added in a weird way
  mutate(holdfast_diameter = as.numeric(holdfast_diameter)) %>% # numeric expected, not factor
  separate(number, into = c("parent", "child"), fill = "right") %>% # two new columns after split on "."
  mutate(stipe_length = as.character(stipe_length)) %>% # necessary prior to replacing "<10" with numeric 5
  mutate(stipe_length = as.numeric(replace(stipe_length, stipe_length %in% c("<10"), 5))) %>%
  as_tibble() # because I like tibbles
casts

# A quick, very basic data summary
casts.summary <- casts %>%
  group_by(date) %>%
  summarise(n_casts = n(),
            mean_diam = mean(holdfast_diameter, na.rm = TRUE),
            sd_diam = sd(holdfast_diameter, na.rm = TRUE),
            mean_st_length = mean(stipe_length, na.rm = TRUE),
            sd_st_length = sd(stipe_length, na.rm = TRUE),
            mean_fr_length = mean(frond_length, na.rm = TRUE),
            sd_fr_length = sd(frond_length, na.rm = TRUE))



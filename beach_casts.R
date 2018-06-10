# beach_casts.R
# Graphs for beach cast project
# Carlin Landsberg
# 10 June 2018

# Load libraries ----------------------------------------------------------

library(tidyverse)

# Load data ---------------------------------------------------------------

casts.in <- read.csv2("project_data_working_copy.csv")

# Set data up correctly for analyses --------------------------------------

# Parse the data into the right format
casts <- casts.in %>%
  mutate(date = as.Date(date, "%d-%b-%y")) %>% # because the dates were added in a weird way
  mutate(holdfast_diameter = as.numeric(holdfast_diameter)) %>% # numeric expected, not factor
  separate(number, into = c("parent", "child"), fill = "right") %>% # two new columns after split on "."
  mutate(stipe_length = as.character(stipe_length)) %>% # necessary prior to replacing "<10" with numeric 5
  mutate(stipe_length = as.numeric(replace(stipe_length, stipe_length %in% c("<10"), 5))) %>%
  as_tibble() # because I like tibbles
casts

# Summarise data ----------------------------------------------------------

# basic data summary
casts.summary <- casts %>%
  group_by(date) %>%
  summarise(n_casts = n(),
            mean_diam = mean(holdfast_diameter, na.rm = TRUE),
            sd_diam = sd(holdfast_diameter, na.rm = TRUE),
            mean_st_length = mean(stipe_length, na.rm = TRUE),
            sd_st_length = sd(stipe_length, na.rm = TRUE),
            mean_fr_length = mean(frond_length, na.rm = TRUE),
            sd_fr_length = sd(frond_length, na.rm = TRUE))
casts.summary

# Graphical visulaisations ------------------------------------------------

plot1 <- ggplot(casts.summary, aes(x = as.factor(date), y = n_casts)) +
  geom_bar(stat = "identity")
plot1
# A lot of variation seen. Total number of kelp fluctuates each week

mean_stipe <- ggplot(casts.summary, aes(x = as.factor(date), y = mean_st_length)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_st_length - sd_st_length,
                    ymax = mean_st_length + sd_st_length, width = 0.2))
mean_stipe
# mean stipe lengths vary weekly, larger kelps dislodged due to strong wave action?
# OR very small kelps dislodged due to stronger wave action? -- refer to de Bettignies 2015

mean_frond <- ggplot(casts.summary, aes(x = as.factor(date), y = mean_fr_length)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_fr_length - sd_fr_length,
                    ymax = mean_fr_length + sd_fr_length, width = 0.2))
mean_frond
# the same pattern applies to frond length. can we find ratio for stipe length:frond
# possibly compare to Jesse's data to determine where population comes from 

mean_diam <- ggplot(casts.summary, aes(x = as.factor(date), y = mean_diam)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_diam - sd_diam,
                    ymax = mean_diam + sd_diam, width = 0.2))
mean_diam
# larger diameter of holdfast dislodged by stronger wave action
# larger diameters usually associated with aggregates
# therefore, assume that when aggregates (large holdfast diameters) wash up, wave action must've been strong


# for all these plots there is an NA at the end??? is this because of the spaces?

# Still to do or try out --------------------------------------------------

# How to plot y/n (binary) for number of holdfasts present? 
# How to plot number of 'children' at each date OR 
# how many parents have children (and how many) at each date

# statistics
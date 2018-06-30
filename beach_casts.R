# beach_casts.R
# Graphs for beach cast project
# Carlin Landsberg
# 10 June 2018

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(pgirmess)

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

# Graphical visualisations (1) ------------------------------------------------

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

# Graphical visualisations (2) --------------------------------------------

# holdfast presence or absence
ggplot(casts, aes(x = holdfast)) +
  geom_histogram(stat = "count")
# this is just holdfast presence or absence in total.
# so all aggregates sharing a holdfast are included

# Number of aggregates per week
ggplot(casts, aes(x = as.factor(date), y = child)) +
  geom_bar(stat = "identity")
# Total number of aggregates per week

# stipe vs frond
ggplot(casts, aes(x = stipe_length, y = frond_length)) +
  geom_point() +
  geom_smooth(method = "lm")

# plot for parents with children
# average number of aggregates per holdfast
# proportion of total number per week with holdfasts and then...
  # what proportion of these have aggregates?

# boxplots
# check normality 
# so either anova or kruskall-wallis

# Boxplots ----------------------------------------------------------------

# Holdfast diameter
ggplot(data = casts, aes(x = as.factor(date), y = holdfast_diameter)) +
  geom_boxplot(aes(fill = as.factor(date)))

# Stipe length
ggplot(data = casts, aes(x = as.factor(date), y = stipe_length)) +
  geom_boxplot(aes(fill = as.factor(date)))
  
# Frond length
ggplot(data = casts, aes(x = as.factor(date), y = frond_length)) +
  geom_boxplot(aes(fill = as.factor(date)))

# Check normality ---------------------------------------------------------

shapiro.test(casts$holdfast_diameter)
  # not normal
shapiro.test(casts$stipe_length)
  # not normal
shapiro.test(casts$frond_length)
  # not normal

# casts %>% 
#   group_by(date) %>% 
#   summarise(hf_norm = as.numeric(shapiro.test(holdfast_diameter)[2]),
#             hf_var = var(holdfast_diameter)) 
  # Error in summarise_impl(.data, dots) : 
  #   Evaluation error: sample size must be between 3 and 5000.

# Kruskall-wallis ---------------------------------------------------------

# holdfast
hf_kw <- kruskal.test(holdfast_diameter ~ as.factor(date), data = casts)
  # Kruskal-Wallis rank sum test
  # 
  # data:  holdfast_diameter by as.factor(date)
  # Kruskal-Wallis chi-squared = 8.5487, df = 6, p-value = 0.2006
    # p > 0.05, variance is not significant

# stipe length
st_kw <- kruskal.test(stipe_length ~ as.factor(date), data = casts)
  # Kruskal-Wallis rank sum test
  # 
  # data:  stipe_length by as.factor(date)
  # Kruskal-Wallis chi-squared = 24.437, df = 9, p-value = 0.003661
    # p < 0.05, significant variance

# frond length
fr_kw <- kruskal.test(frond_length ~ as.factor(date), data = casts)
  # Kruskal-Wallis rank sum test
  # 
  # data:  frond_length by as.factor(date)
  # Kruskal-Wallis chi-squared = 22.355, df = 9, p-value = 0.007819
    # p > 0.05, significant variance

# holdfast diameter post-hoc
kruskalmc(holdfast_diameter ~ as.factor(date), data = casts)

# stipe length post-hoc
kruskalmc(stipe_length ~ as.factor(date), data = casts)

# frond length post-hoc
kruskalmc(frond_length ~ as.factor(date), data = casts)

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
  geom_bar(stat = "identity") +
  labs(x = "Date", y = "Number of casts", title = "Number of casts per week") +
  theme_classic()
plot1
# A lot of variation seen. Total number of kelp fluctuates each week

mean_stipe <- ggplot(casts.summary, aes(x = as.factor(date), y = mean_st_length)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_st_length - sd_st_length,
                    ymax = mean_st_length + sd_st_length, width = 0.2)) +
  labs(x = "Date", y = "Stipe length", title = "Mean stipe length per week") +
  theme_classic()
  mean_stipe
# mean stipe lengths vary weekly, larger kelps dislodged due to strong wave action?
# OR very small kelps dislodged due to stronger wave action? -- refer to de Bettignies 2015

mean_frond <- ggplot(casts.summary, aes(x = as.factor(date), y = mean_fr_length)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_fr_length - sd_fr_length,
                    ymax = mean_fr_length + sd_fr_length, width = 0.2)) +
  labs(x = "Date", y = "Frond length", title = "Mean frond length per week") +
  theme_classic()
mean_frond
# the same pattern applies to frond length. can we find ratio for stipe length:frond
# possibly compare to Jesse's data to determine where population comes from 

mean_diam <- ggplot(casts.summary, aes(x = as.factor(date), y = mean_diam)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_diam - sd_diam,
                    ymax = mean_diam + sd_diam, width = 0.2)) +
  labs(x = "Date", y = "Holdfast diameter", title = "Mean holdfast diameter per week") +
  theme_classic()
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
child_plot <- ggplot(casts, aes(x = as.factor(date), y = child)) +
  geom_bar(stat = "identity") +
  labs(x = "Date", y = "Number of aggregates", title = "Number of aggregates per week") +
  theme_classic()
child_plot
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
# OR to add notches
  #geom_boxplot(aes(fill = as.factor(date)), notch = TRUE)

# Stipe length
ggplot(data = casts, aes(x = as.factor(date), y = stipe_length)) +
  geom_boxplot(aes(fill = as.factor(date)))
# OR to add notches
  #geom_boxplot(aes(fill = as.factor(date)), notch = TRUE)
  
# Frond length
ggplot(data = casts, aes(x = as.factor(date), y = frond_length)) +
  geom_boxplot(aes(fill = as.factor(date)))
# OR to add notches
  #geom_boxplot(aes(fill = as.factor(date)), notch = TRUE)

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

# wave data ---------------------------------------------------------------

# 2005 - 2018 data
wave_all_1 <- read_table2("ChristoRautenbach_NCEP_180904/FB3_all_wav.txt", 
                           skip = 6)
wave_all <- wave_all_1[-c(1, 2),]

# basic summary for wave data

wave_all_summary_1 <- wave_all %>%
  group_by(Date) %>%
  summarise(mean_h1f = mean(as.numeric(H1F, na.rm = TRUE)),
            sd_h1f = sd(as.numeric(H1F, na.rm = TRUE)),
            mean_tp = mean(as.numeric(TP, na.rm = TRUE)),
            sd_tp = sd(as.numeric(TP, na.rm = TRUE)),
            mean_tz = mean(as.numeric(TZ, na.rm = TRUE)),
            sd_tz = sd(as.numeric(TZ, na.rm = TRUE)),
            mean_tcf = mean(as.numeric(TCF, na.rm = TRUE)),
            sd_tcf = sd(as.numeric(TCF, na.rm = TRUE)),
            mean_tbf = mean(as.numeric(TBF, na.rm = TRUE)),
            sd_tbf = sd(as.numeric(TBF, na.rm = TRUE)),
            mean_dir = mean(as.numeric(DIR, na.rm = TRUE)),
            sd_dir = sd(as.numeric(DIR, na.rm = TRUE)),
            mean_spr = mean(as.numeric(SPR, na.rm = TRUE)),
            sd_spr = sd(as.numeric(SPR, na.rm = TRUE)),
            mean_hs = mean(as.numeric(HS, na.rm = TRUE)),
            sd_hs = sd(as.numeric(HS, na.rm = TRUE)),
            mean_hmax = mean(as.numeric(HMAX, na.rm = TRUE)),
            sd_hmax = sd(as.numeric(HMAX, na.rm = TRUE)))

# remove last row
wave_all_summary <- wave_all_summary_1[-c(15),]

# plots for wave data from 2005 - 2018 (mean of each parameter)
plot_h1f <- ggplot(wave_all_summary, aes(x = as.factor(Date), y = mean_h1f)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_h1f - sd_h1f,
                    ymax = mean_h1f + sd_h1f, width = 0.2)) +
  labs(x = "Year", y = "Mean H1F", title = "Mean H1F 2005 - 2018") +
  theme_classic()
plot_h1f

plot_tp <- ggplot(wave_all_summary, aes(x = as.factor(Date), y = mean_tp)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_tp - sd_tp,
                    ymax = mean_tp + sd_tp, width = 0.2)) +
  labs(x = "Year", y = "Mean TP", title = "Mean TP 2005 - 2018") +
  theme_classic()
plot_tp

plot_tz <- ggplot(wave_all_summary, aes(x = as.factor(Date), y = mean_tz)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_tz - sd_tz,
                    ymax = mean_tz + sd_tz, width = 0.2)) +
  labs(x = "Year", y = "Mean TZ", title = "Mean TZ 2005 - 2018") +
  theme_classic()
plot_tz

plot_tcf <- ggplot(wave_all_summary, aes(x = as.factor(Date), y = mean_tcf)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_tcf - sd_tcf,
                    ymax = mean_tcf + sd_tcf, width = 0.2)) +
  labs(x = "Year", y = "Mean TCF", title = "Mean TCF 2005 - 2018") +
  theme_classic()
plot_tcf

plot_hs <- ggplot(wave_all_summary, aes(x = as.factor(Date), y = mean_hs)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_hs - sd_hs,
                    ymax = mean_hs + sd_hs, width = 0.2)) +
  labs(x = "Year", y = "Mean HS", title = "Mean HS 2005 - 2018") +
  theme_classic()
plot_hs

  # note, some columns have identical values throughout. not plotted

# Now filtering out only 2018 (sampling took place from March 2018-September 2018)
wave_2018 <- wave_all %>% 
  filter(Date == "2018") %>% 
  group_by(Time) %>% 
  summarise(mean_h1f = mean(as.numeric(H1F, na.rm = TRUE)),
            sd_h1f = sd(as.numeric(H1F, na.rm = TRUE)),
            mean_tp = mean(as.numeric(TP, na.rm = TRUE)),
            sd_tp = sd(as.numeric(TP, na.rm = TRUE)),
            mean_tz = mean(as.numeric(TZ, na.rm = TRUE)),
            sd_tz = sd(as.numeric(TZ, na.rm = TRUE)),
            mean_tcf = mean(as.numeric(TCF, na.rm = TRUE)),
            sd_tcf = sd(as.numeric(TCF, na.rm = TRUE)),
            mean_tbf = mean(as.numeric(TBF, na.rm = TRUE)),
            sd_tbf = sd(as.numeric(TBF, na.rm = TRUE)),
            mean_dir = mean(as.numeric(DIR, na.rm = TRUE)),
            sd_dir = sd(as.numeric(DIR, na.rm = TRUE)),
            mean_spr = mean(as.numeric(SPR, na.rm = TRUE)),
            sd_spr = sd(as.numeric(SPR, na.rm = TRUE)),
            mean_hs = mean(as.numeric(HS, na.rm = TRUE)),
            sd_hs = sd(as.numeric(HS, na.rm = TRUE)),
            mean_hmax = mean(as.numeric(HMAX, na.rm = TRUE)),
            sd_hmax = sd(as.numeric(HMAX, na.rm = TRUE)))
wave_2018

# Plots for parameters only 2018
h1f_2018 <- ggplot(wave_2018, aes(x = as.factor(Time), y = mean_h1f)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_h1f - sd_h1f,
                    ymax = mean_h1f + sd_h1f, width = 0.2)) +
  labs(x = "Month", y = "Mean H1F", title = "Mean H1F 2018") +
  theme_classic()
h1f_2018

tp_2018 <- ggplot(wave_2018, aes(x = as.factor(Time), y = mean_tp)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_tp - sd_tp,
                    ymax = mean_tp + sd_tp, width = 0.2)) +
  labs(x = "Month", y = "Mean TP", title = "Mean TP 2018") +
  theme_classic()
tp_2018

tz_2018 <- ggplot(wave_2018, aes(x = as.factor(Time), y = mean_tz)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_tz - sd_tz,
                    ymax = mean_tz + sd_tz, width = 0.2)) +
  labs(x = "Month", y = "Mean TZ", title = "Mean TZ 2018") +
  theme_classic()
tz_2018

tcf_2018 <- ggplot(wave_2018, aes(x = as.factor(Time), y = mean_tcf)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_tcf - sd_tcf,
                    ymax = mean_tcf + sd_tcf, width = 0.2)) +
  labs(x = "Month", y = "Mean TCF", title = "Mean TCF 2018") +
  theme_classic()
tcf_2018

hs_2018 <- ggplot(wave_2018, aes(x = as.factor(Time), y = mean_hs)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_hs - sd_hs,
                    ymax = mean_hs + sd_hs, width = 0.2)) +
  labs(x = "Month", y = "Mean HS", title = "Mean HS 2018") +
  theme_classic()
hs_2018

# 1979 - 2010
old_wave_1 <- read_table2("ChristoRautenbach_NCEP_180904/FB3_rean_all_wav.txt", 
                                skip = 7)
old_wave <- old_wave_1[-c(1,2),]

# basic summary for older wave data

wave_old_summary_1 <- old_wave %>%
  group_by(Date) %>%
  summarise(mean_h1f = mean(as.numeric(H1F, na.rm = TRUE)),
            sd_h1f = sd(as.numeric(H1F, na.rm = TRUE)),
            mean_tp = mean(as.numeric(TP, na.rm = TRUE)),
            sd_tp = sd(as.numeric(TP, na.rm = TRUE)),
            mean_tz = mean(as.numeric(TZ, na.rm = TRUE)),
            sd_tz = sd(as.numeric(TZ, na.rm = TRUE)),
            mean_tcf = mean(as.numeric(TCF, na.rm = TRUE)),
            sd_tcf = sd(as.numeric(TCF, na.rm = TRUE)),
            mean_tbf = mean(as.numeric(TBF, na.rm = TRUE)),
            sd_tbf = sd(as.numeric(TBF, na.rm = TRUE)),
            mean_dir = mean(as.numeric(DIR, na.rm = TRUE)),
            sd_dir = sd(as.numeric(DIR, na.rm = TRUE)),
            mean_spr = mean(as.numeric(SPR, na.rm = TRUE)),
            sd_spr = sd(as.numeric(SPR, na.rm = TRUE)),
            mean_hs = mean(as.numeric(HS, na.rm = TRUE)),
            sd_hs = sd(as.numeric(HS, na.rm = TRUE)),
            mean_hmax = mean(as.numeric(HMAX, na.rm = TRUE)),
            sd_hmax = sd(as.numeric(HMAX, na.rm = TRUE)))

# remove last row
wave_old_summary <- wave_old_summary_1[-c(33),]

# plots for wave data from 1979 - 2010 (mean of each parameter)
old_h1f <- ggplot(wave_old_summary, aes(x = as.factor(Date), y = mean_h1f)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_h1f - sd_h1f,
                    ymax = mean_h1f + sd_h1f, width = 0.2)) +
  labs(x = "Year", y = "Mean H1F", title = "Mean H1F 1979 - 2010") +
  theme_classic()
old_h1f

old_tp <- ggplot(wave_old_summary, aes(x = as.factor(Date), y = mean_tp)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_tp - sd_tp,
                    ymax = mean_tp + sd_tp, width = 0.2)) +
  labs(x = "Year", y = "Mean TP", title = "Mean TP 1979 - 2010") +
  theme_classic()
old_tp

old_tz <- ggplot(wave_old_summary, aes(x = as.factor(Date), y = mean_tz)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_tz - sd_tz,
                    ymax = mean_tz + sd_tz, width = 0.2)) +
  labs(x = "Year", y = "Mean TZ", title = "Mean TZ 1979 - 2010") +
  theme_classic()
old_tz

old_tcf <- ggplot(wave_old_summary, aes(x = as.factor(Date), y = mean_tcf)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_tcf - sd_tcf,
                    ymax = mean_tcf + sd_tcf, width = 0.2)) +
  labs(x = "Year", y = "Mean TCF", title = "Mean TCF 1979 - 2010") +
  theme_classic()
old_tcf

old_hs <- ggplot(wave_old_summary, aes(x = as.factor(Date), y = mean_hs)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_hs - sd_hs,
                    ymax = mean_hs + sd_hs, width = 0.2)) +
  labs(x = "Year", y = "Mean HS", title = "Mean HS 1979 - 2010") +
  theme_classic()
old_hs
  
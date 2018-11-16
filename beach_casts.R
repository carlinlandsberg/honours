# beach_casts.R
# Graphs for beach cast project
# Carlin Landsberg
# 10 June 2018

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(pgirmess)
library(circular)
library(zoo)
library(dplyr)

# Load data ---------------------------------------------------------------

casts.in <- read.csv2("data/project_data_working_copy.csv")

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
  geom_bar(stat = "identity", na.rm = TRUE) +
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
diam_box <- ggplot(data = casts, aes(x = as.factor(date), y = holdfast_diameter)) +
  geom_boxplot(aes(fill = as.factor(date)))
# OR to add notches
  #geom_boxplot(aes(fill = as.factor(date)), notch = TRUE)
diam_box

# Stipe length
stipe_box <- ggplot(data = casts, aes(x = as.factor(date), y = stipe_length)) +
  geom_boxplot(aes(fill = as.factor(date)))
# OR to add notches
  #geom_boxplot(aes(fill = as.factor(date)), notch = TRUE)
stipe_box
  
# Frond length
frond_box <- ggplot(data = casts, aes(x = as.factor(date), y = frond_length)) +
  geom_boxplot(aes(fill = as.factor(date)))
# OR to add notches
  #geom_boxplot(aes(fill = as.factor(date)), notch = TRUE)
frond_box

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
# Kruskal-Wallis chi-squared = 46.195, df = 9, p-value = 5.54e-07
    # p < 0.05, variance is significant

# stipe length
st_kw <- kruskal.test(stipe_length ~ as.factor(date), data = casts)
# Kruskal-Wallis rank sum test
# 
# data:  stipe_length by as.factor(date)
# Kruskal-Wallis chi-squared = 39.109, df = 16, p-value = 0.001049
    # p < 0.05, significant variance

# frond length
fr_kw <- kruskal.test(frond_length ~ as.factor(date), data = casts)
# Kruskal-Wallis rank sum test
# 
# data:  frond_length by as.factor(date)
# Kruskal-Wallis chi-squared = 44.278, df = 16, p-value = 0.0001789
    # p > 0.05, significant variance

# holdfast diameter post-hoc
kruskalmc(holdfast_diameter ~ as.factor(date), data = casts)

# stipe length post-hoc
kruskalmc(stipe_length ~ as.factor(date), data = casts)

# frond length post-hoc
kruskalmc(frond_length ~ as.factor(date), data = casts)  

# load wave data ---------------------------------------------------------------

# 2005 - 2018 data
# wave_all_1 <- read_csv("NCEP/NCEP_dat.csv", 
#                        col_types = cols(dp = col_character(), 
#                                         hs = col_character(), tp = col_character(), 
#                                         u = col_character(), v = col_character()))

wave_all_1 <- read_csv("NCEP/NCEP_dat.csv")

wave_all <- separate(wave_all_1, time, into = c("date", "time"), sep = " ")

# wave_all_1$date <- sapply(strsplit(wave_all_1$time, " "), "[", 1)
# wave_all_1$time <- sapply(strsplit(as.numeric(wave_all_1$time), " "), "[", 2)
# 
# wave_date <- as.data.frame(as.Date(wave_all_1$time)) 
# colnames(wave_date) <- c("date")
# 
# wave_time <- as.tibble(format(as.POSIXct(wave_all_1$time)))
# colnames(wave_time) <- c("time")

# eg <- cbind(wave_all_1, wave_date, wave_time)
# 
# wave_all <- eg[, -1]

sep_wave_all <- wave_all %>% 
  separate(date, c("year", "month", "day"), "-")

# basic summary for wave data

wave_all_summary <- sep_wave_all %>%
  group_by(year) %>% 
  summarise(mean_hs = mean(as.numeric(hs, na.rm = TRUE)),
            sd_hs = sd(as.numeric(hs, na.rm = TRUE)),
            mean_tp = mean(as.numeric(tp, na.rm = TRUE)),
            sd_tp = sd(as.numeric(tp, na.rm = TRUE)),
            mean_dp = mean(as.numeric(dp, na.rm = TRUE)),
            sd_dp = sd(as.numeric(dp, na.rm = TRUE)),
            mean_u = mean(as.numeric(u, na.rm = TRUE)),
            sd_u = sd(as.numeric(u, na.rm = TRUE)),
            mean_v = mean(as.numeric(v, na.rm = TRUE)),
            sd_v = sd(as.numeric(v, na.rm = TRUE)))


# Wave data summary plots (2012 - 2018) -------------------------------------------------

# COME BACK TO THESE ------------------------------------------------------


# plots for wave data from 2005 - 2018 (mean of each parameter)
plot_hs <- ggplot(wave_all_summary, aes(x = as.factor(date), y = mean_hs)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_hs - sd_hs,
                    ymax = mean_hs + sd_hs, width = 0.2)) +
  labs(x = "Year", y = "Mean hs", title = "Mean hs 2012 - 2018") +
  theme_classic()
plot_hs

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

# Wave data visualisations 2018 -------------------------------------------

# Now filtering out only 2018 (sampling took place from March 2018-September 2018)
sep_wave_all <- wave_all %>% 
  separate(date, c("year", "month", "day"), "-")

wave_2018_1 <- sep_wave_all %>% 
  filter(year == "2018") %>% 
  group_by(month)

wave_2018 <- sep_wave_all %>%
  filter(year == "2018") %>% 
  group_by(month) %>% 
  summarise(mean_hs = mean(as.numeric(hs, na.rm = TRUE)),
            sd_hs = sd(as.numeric(hs, na.rm = TRUE)),
            mean_tp = mean(as.numeric(tp, na.rm = TRUE)),
            sd_tp = sd(as.numeric(tp, na.rm = TRUE)),
            mean_dp = mean(as.numeric(dp, na.rm = TRUE)),
            sd_dp = sd(as.numeric(dp, na.rm = TRUE)),
            mean_u = mean(as.numeric(u, na.rm = TRUE)),
            sd_u = sd(as.numeric(u, na.rm = TRUE)),
            mean_v = mean(as.numeric(v, na.rm = TRUE)),
            sd_v = sd(as.numeric(v, na.rm = TRUE)))
wave_2018

# Plots for parameters only 2018
hs_2018 <- ggplot(wave_2018, aes(x = as.factor(month), y = mean_hs)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_hs - sd_hs,
                    ymax = mean_hs + sd_hs, width = 0.2)) +
  labs(x = "Month", y = "Mean hs", title = "Mean hs 2018") +
  theme_classic()
hs_2018

tp_2018 <- ggplot(wave_2018, aes(x = as.factor(month), y = mean_tp)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_tp - sd_tp,
                    ymax = mean_tp + sd_tp, width = 0.2)) +
  labs(x = "Month", y = "Mean TP", title = "Mean TP 2018") +
  theme_classic()
tp_2018

dp_2018 <- ggplot(wave_2018, aes(x = as.factor(month), y = mean_dp)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_dp - sd_dp,
                    ymax = mean_dp + sd_dp, width = 0.2)) +
  labs(x = "Month", y = "Mean dp", title = "Mean dp 2018") +
  theme_classic()
dp_2018

u_2018 <- ggplot(wave_2018, aes(x = as.factor(month), y = mean_u)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_u - sd_u,
                    ymax = mean_u + sd_u, width = 0.2)) +
  labs(x = "Month", y = "Mean u", title = "Mean u 2018") +
  theme_classic()
u_2018

v_2018 <- ggplot(wave_2018, aes(x = as.factor(month), y = mean_v)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_v - sd_v,
                    ymax = mean_v + sd_v, width = 0.2)) +
  labs(x = "Month", y = "Mean v", title = "Mean v 2018") +
  theme_classic()
v_2018

# Wave data 1979-2010 -----------------------------------------------------

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

# wave data summary plots 1979-2010 ---------------------------------------

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

# Time series 2018 -------------------------------------------------------------

daily_2018 <- wave_2018_1 %>% 
  group_by(month, day) %>% 
  summarise(hs_circ = mean.circular(circular(hs)),
            tp_circ = mean.circular(circular(tp)),
            dp_circ = mean.circular(circular(dp)),
            u_circ = mean.circular(circular(u)),
            v_circ = mean.circular(circular(v))) %>% 
  mutate(year = "2018")
daily_2018

rm <- daily_2018[, -1:-2]
final_rm <- rm[, -6]

try <- as.tibble(paste(daily_2018$year, daily_2018$month, daily_2018$day, sep = "-"))
try

time_2018 <- cbind(try, final_rm)
time_2018

sep_2018 <- time_2018 %>% 
  separate(value, c("year", "month", "day"), "-")

# time_2018 <- combined_2018[-c(1:82),]

# circular function: smaller gap between degrees (wave data is in degrees) large gap in numerical gap.
# eg large numerical gap between 365 and 2, but the gap is small between 365 degrees and 2 degrees

hs_time <- ggplot(time_2018, aes(x = as.Date(value), y = hs_circ), group = 1) +
#  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "HS (m)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
#  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
hs_time

# ncasts_tp <- ggplot(time_2018, aes(x = as.Date(value), y = tp_circ, group = 1)) +
#   geom_line() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
#   scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Total number of casts")) +
#   geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
#   geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), 
#                                          y = 0, yend = n_casts)) +
#   labs(x = "Date", y = "Peak period (s)", title = "") +
#   theme_bw()

tp_time <- ggplot(time_2018, aes(x = as.Date(value), y = tp_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "TP (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
tp_time

dp_time <- ggplot(time_2018, aes(x = as.Date(value), y = dp_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "DP", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
dp_time

u_time <- ggplot(time_2018, aes(x = as.Date(value), y = u_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "U", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
u_time

v_time <- ggplot(time_2018, aes(x = as.Date(value), y = v_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "V", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
v_time

# total number of casts over time series
ncasts_hs <- ggplot(time_2018, aes(x = as.Date(value), y = hs_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Total number of casts")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = n_casts)) +
#  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "hs (m)", title = "") +
  theme_bw()
ncasts_hs

ncasts_tp <- ggplot(time_2018, aes(x = as.Date(value), y = tp_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Total number of casts")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = n_casts)) +
#  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "TP (s)", title = "") +
  theme_bw()
ncasts_tp

ncasts_dp <- ggplot(time_2018, aes(x = as.Date(value), y = dp_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Total number of casts")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = n_casts)) +
#  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "dp", title = "") +
  theme_bw()
ncasts_dp

ncasts_u <- ggplot(time_2018, aes(x = as.Date(value), y = u_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Total number of casts")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = n_casts)) +
  #  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "u", title = "") +
  theme_bw()
ncasts_u

ncasts_v <- ggplot(time_2018, aes(x = as.Date(value), y = v_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Total number of casts")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = n_casts)) +
  #  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "v", title = "") +
  theme_bw()
ncasts_v

# mean diameter of holdfast over time series
diam_hs <- ggplot(time_2018, aes(x = as.Date(value), y = hs_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Holdfast diameter")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = mean_diam), na.rm = TRUE) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = mean_diam), na.rm = TRUE) +
#  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "hs (m)", title = "") +
  theme_bw()
diam_hs

diam_tp <- ggplot(time_2018, aes(x = as.Date(value), y = tp_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Holdfast diameter")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = mean_diam), na.rm = TRUE) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = mean_diam), na.rm = TRUE) +
  labs(x = "Date", y = "Tp (s)", title = "") +
  theme_bw()
diam_tp

diam_dp <- ggplot(time_2018, aes(x = as.Date(value), y = dp_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Holdfast diameter")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = mean_diam), na.rm = TRUE) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = mean_diam), na.rm = TRUE) +
#  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "dp (m)", title = "") +
  theme_bw()
diam_dp

diam_u <- ggplot(time_2018, aes(x = as.Date(value), y = u_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Holdfast diameter")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = mean_diam), na.rm = TRUE) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = mean_diam), na.rm = TRUE) +
  #  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "u", title = "") +
  theme_bw()
diam_u

diam_v <- ggplot(time_2018, aes(x = as.Date(value), y = v_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Holdfast diameter")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = mean_diam), na.rm = TRUE) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = mean_diam), na.rm = TRUE) +
  #  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "v", title = "") +
  theme_bw()
diam_v

# mean stipe length over time series
stipe_hs <- ggplot(time_2018, aes(x = as.Date(value), y = hs_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Stipe length")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = mean_st_length), na.rm = TRUE) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = mean_st_length), na.rm = TRUE) +
#  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "hs", title = "") +
  theme_bw()
stipe_hs

stipe_tp <- ggplot(time_2018, aes(x = as.Date(value), y = tp_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Stipe length")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = mean_st_length), na.rm = TRUE) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = mean_st_length), na.rm = TRUE) +
#  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "Tp (s)", title = "") +
  theme_bw()
stipe_tp

stipe_dp <- ggplot(time_2018, aes(x = as.Date(value), y = dp_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Stipe length")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = mean_st_length), na.rm = TRUE) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = mean_st_length), na.rm = TRUE) +
#  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "dp", title = "") +
  theme_bw()
stipe_dp

stipe_u <- ggplot(time_2018, aes(x = as.Date(value), y = u_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Stipe length")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = mean_st_length), na.rm = TRUE) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = mean_st_length), na.rm = TRUE) +
  #  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "u", title = "") +
  theme_bw()
stipe_u

stipe_v <- ggplot(time_2018, aes(x = as.Date(value), y = v_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Stipe length")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = mean_st_length), na.rm = TRUE) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = mean_st_length), na.rm = TRUE) +
  #  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "v", title = "") +
  theme_bw()
stipe_v

# mean frond length over time series
frond_hs <- ggplot(time_2018, aes(x = as.Date(value), y = hs_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Frond length")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = mean_fr_length), na.rm = TRUE) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = mean_fr_length), na.rm = TRUE) +
#  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "hs", title = "") +
  theme_bw()
frond_hs

frond_tp <- ggplot(time_2018, aes(x = as.Date(value), y = tp_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Frond length")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = mean_fr_length), na.rm = TRUE) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = mean_fr_length), na.rm = TRUE) +
#  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "Tp (s)", title = "") +
  theme_bw()
frond_tp

frond_dp <- ggplot(time_2018, aes(x = as.Date(value), y = dp_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Frond length")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = mean_fr_length), na.rm = TRUE) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = mean_fr_length), na.rm = TRUE) +
#  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "dp", title = "") +
  theme_bw()
frond_dp

frond_u <- ggplot(time_2018, aes(x = as.Date(value), y = u_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Frond length")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = mean_fr_length), na.rm = TRUE) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = mean_fr_length), na.rm = TRUE) +
  #  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "u", title = "") +
  theme_bw()
frond_u

frond_v <- ggplot(time_2018, aes(x = as.Date(value), y = v_circ, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Frond length")) +
  geom_point(data = casts.summary, aes(x = as.Date(date), y = mean_fr_length), na.rm = TRUE) +
  geom_segment(data = casts.summary, aes(x = as.Date(date), xend = as.Date(date), y = 0, yend = mean_fr_length), na.rm = TRUE) +
  #  geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
  labs(x = "Date", y = "v", title = "") +
  theme_bw()
frond_v

# ncasts_tp <- ggplot(time_2018, aes(x = as.Date(value), y = tp_circ, group = 1)) +
#   geom_line() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
#   scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Total number of casts")) +
#   geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
#   geom_vline(xintercept = casts.summary$date, na.rm = TRUE, linetype = "dotted") +
#   labs(x = "Date", y = "Peak period (s)", title = "") +
#   theme_classic()
# ncasts_tp

# some extra stuff... -----------------------------------------------------

sep_all <- wave_all %>% 
  separate(date, c("year", "month", "day"), "-")

wave_16_17_18 <- sep_all %>% 
  filter(year %in% c("2016", "2017", "2018")) %>% 
  group_by(month)
wave_16_17_18

daily_16_17_18 <- wave_16_17_18 %>% 
  group_by(year, month, day) %>% 
  summarise(hs_circ = mean.circular(circular(hs)),
            tp_circ = mean.circular(circular(tp)),
            dp_circ = mean.circular(circular(dp)),
            u_circ = mean.circular(circular(u)),
            v_circ = mean.circular(circular(v))) %>% 
  mutate(year1 = "2018")
daily_16_17_18

rm_1 <- daily_16_17_18[, -1:-3]
final_rm_1 <- rm_1[, -6]

try_1 <- as.tibble(paste(daily_16_17_18$year, daily_16_17_18$month, daily_16_17_18$day, sep = "-"))
try_1

combined_16_17_18 <- cbind(final_rm_1, try_1)
combined_16_17_18

sep_16_17_18 <- combined_16_17_18 %>% 
  separate(value, c("year", "month", "day"), "-")
sep_16_17_18

tp_16_17_18 <- ggplot(combined_16_17_18, aes(x = as.Date(value), y = tp_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line(aes(group = as.Date(value))) +
  labs(x = "Date", y = "TP (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
tp_16_17_18

tcf_time <- ggplot(time_2018, aes(x = as.Date(value), y = tcf_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "TCF (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
tcf_time

hs_time <- ggplot(time_2018, aes(x = as.Date(value), y = hs_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "HS", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
hs_time

# Looking at the past 2 years ---------------------------------------------

wave_16 <- wave_16_17_18 %>% 
  filter(year == "2016") %>% 
  group_by(month)
wave_16

daily_16 <- wave_16 %>% 
  group_by(year, month, day) %>% 
  summarise(hs_circ = mean.circular(circular(hs)),
            tp_circ = mean.circular(circular(tp)),
            dp_circ = mean.circular(circular(dp)),
            u_circ = mean.circular(circular(u)),
            v_circ = mean.circular(circular(v))) %>% 
  mutate(year1 = "2016")
daily_16

rm_2 <- daily_16[, -1:-3]
# final_rm_2 <- rm_2[, -4]

try_2 <- as.tibble(paste(daily_16$year, daily_16$month, daily_16$day, sep = "-"))
try_2

combined_16 <- cbind(rm_2, try_2)
combined_16

sep_16 <- combined_16 %>% 
  separate(value, c("year", "month", "day"), "-")
sep_16

# time_16 <- combined_16[-c(1:83),]
# time_16_1 <- time_16[-c(246:366),]
# time_16_2 <- time_16_1[-c(246:328),]

ttime_16 <- combined_16[-c(275:366),]
ttime_16_1 <- ttime_16[-c(1:76),]

wave_17 <- wave_16_17_18 %>% 
  filter(year == "2017") %>% 
  group_by(month)
wave_17

daily_17 <- wave_17 %>% 
  group_by(year, month, day) %>% 
  summarise(hs_circ = mean.circular(circular(hs)),
            tp_circ = mean.circular(circular(tp)),
            dp_circ = mean.circular(circular(dp)),
            u_circ = mean.circular(circular(u)),
            v_circ = mean.circular(circular(v))) %>% 
  mutate(year2 = "2017")
daily_17

rm_3 <- daily_17[, -1:-3]
# final_rm_3 <- rm_3[, -4]

try_3 <- as.tibble(paste(daily_17$year, daily_17$month, daily_17$day, sep = "-"))
try_3

combined_17 <- cbind(rm_3, try_3)
combined_17

sep_17 <- combined_17 %>% 
  separate(value, c("year", "month", "day"), "-")
sep_17

time_17 <- combined_17[-c(274:365),]
time_17_1 <- time_17[-c(1:75),]

daily_18 <- wave_2018_1 %>% 
  group_by(month, day) %>% 
  summarise(hs_circ = mean.circular(circular(hs)),
            tp_circ = mean.circular(circular(tp)),
            dp_circ = mean.circular(circular(dp)),
            u_circ = mean.circular(circular(u)),
            v_circ = mean.circular(circular(v))) %>% 
  mutate(year3 = "2018")
daily_18

rm_4 <- daily_18[, -1:-2]
# final_rm <- rm[, -4]

try_4 <- as.tibble(paste(daily_18$year3, daily_18$month, daily_18$day, sep = "-"))
try_4

combined_18 <- cbind(rm_4, try_4)
combined_18

time_18_1 <- combined_18[-c(274),]
time_18 <- time_18_1[-c(1:75),]

merge_try <- rbind(combined_16, combined_17, combined_18)

tp_16_17 <- ggplot(merge_try, aes(x = as.Date(value), y = hs_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line(aes(colour = year)) +
  labs(x = "Date", y = "TP (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
tp_16_17

hs_2016 <- ggplot(ttime_16_1, aes(x = as.Date(value), y = hs_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "HS (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
hs_2016

hs_2017 <- ggplot(time_17_1, aes(x = as.Date(value), y = hs_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "HS (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
hs_2017

hs_2018 <- ggplot(time_18, aes(x = as.Date(value), y = hs_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "HS (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
hs_2018

ggarrange(hs_2016, hs_2017, hs_2018, nrow = 3, ncol = 1)

tp_2016 <- ggplot(ttime_16_1, aes(x = as.Date(value), y = tp_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "TP (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
tp_2016

tp_2017 <- ggplot(time_17_1, aes(x = as.Date(value), y = tp_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "TP (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
tp_2017

tp_2018 <- ggplot(time_18, aes(x = as.Date(value), y = tp_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "TP (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
tp_2018

ggarrange(tp_2016, tp_2017, tp_2018, nrow = 3, ncol = 1)

tcf_2016 <- ggplot(ttime_16_1, aes(x = as.Date(value), y = tcf_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "TCF (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
tcf_2016

tcf_2017 <- ggplot(time_17_1, aes(x = as.Date(value), y = tcf_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "TCF (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
tcf_2017

tcf_2018 <- ggplot(time_18, aes(x = as.Date(value), y = tcf_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  labs(x = "Date", y = "TCF (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_classic()
tcf_2018

ggarrange(tcf_2016, tcf_2017, tcf_2018, nrow = 3, ncol = 1)

# Still to look at --------------------------------------------------------

# cut data into 4 seasons dec-fec etc
# fr each season do anova for seasons
# Boxplots for summary stats for each seasons 
# add vert lines for where seasons satrt and end

# Dates with more than usual (total) kelp 
mean(casts.summary$n_casts)
  # 2018-03-24 - 52
  # 2018-04-14 - 42
  # 2018-05-19 - 14
  # 2018-05-26 - 14
  # 2018-08-04 - 22

# Dates with large number of aggregates
  # 2018-03-24 - 13
  # 2018-04-14 - 18
  # 2018-08-04 - 19

# Dates with more holdfasts
  # 2018-03-24 - 12
  # 2018-04-14 - 12

# Dates with largest total size
add_len <- as.tibble(casts$stipe_length + casts$frond_length)

total_casts <- cbind(casts, add_len)

add_len_1 <- as.tibble(casts.summary$mean_st_length + casts.summary$mean_fr_length)
total_casts_1 <- cbind(casts.summary, add_len_1)

  # 2018-03-24 - 343.96520  - 52
  # 2018-05-05 - 483.11111  - 9
  # 2018-04-28 - 325.00000  - 1
  # 2018-05-26 - 302.50758  - 14

# Separating dates into seasons -------------------------------------------

seasons_16_17_18 <- sep_16_17_18 %>% 
  mutate(season = ifelse(month %in% c("12", "01", "02"), "Summer",        
                       ifelse(month %in% c("03", "04", "05"), "Autumn",
                              ifelse(month %in% c("06", "07", "08"), "Winter",
                                     ifelse(month %in% c("09", "10", "11"), "Spring","Error")))))

seasons_16 <- sep_16 %>% 
  mutate(season = ifelse(month %in% c("12", "01", "02"), "Summer",        
                         ifelse(month %in% c("03", "04", "05"), "Autumn",
                                ifelse(month %in% c("06", "07", "08"), "Winter",
                                       ifelse(month %in% c("09", "10", "11"), "Spring","Error")))))

season_16_rm <- as.data.frame(seasons_16[, -1:-9])

sea_16_rm <- season_16_rm %>% 
  select(season = `seasons_16[, -1:-9]`)

sea_16 <- as.data.frame(sea_16_rm[-c(275:366),])
sea_16_1 <- sea_16[-c(1:76),]

season_16_1 <- cbind(ttime_16_1, sea_16_1)
season_16_1

mean_16 <- season_16_1 %>% 
  group_by(sea_16_1) %>% 
  summarise(mean_tp_16 = mean(tp_circ),
            mean_hs_16 = mean(hs_circ),
            mean_dp_16 = mean(dp_circ),
            mean_u_16 = mean(u_circ),
            mean_v_16 = mean(v_circ))

seasons_17 <- sep_17 %>%
  mutate(season = ifelse(month %in% c("12", "01", "02"), "Summer",        
                         ifelse(month %in% c("03", "04", "05"), "Autumn",
                                ifelse(month %in% c("06", "07", "08"), "Winter",
                                       ifelse(month %in% c("09", "10", "11"), "Spring","Error")))))

season_17_rm <- as.data.frame(seasons_17[, -1:-9])

sea_17_rm <- season_17_rm %>% 
  select(season = `seasons_17[, -1:-9]`)

sea_17 <- as.data.frame(sea_17_rm[-c(274:365),])
sea_17_1 <- sea_17[-c(1:75),]

season_17_1 <- cbind(time_17_1, sea_17_1)
season_17_1

mean_17 <- season_17_1 %>% 
  group_by(sea_17_1) %>% 
  summarise(mean_tp_17 = mean(tp_circ),
            mean_hs_17 = mean(hs_circ),
            mean_dp_17 = mean(dp_circ),
            mean_u_17 = mean(u_circ),
            mean_v_17 = mean(v_circ))

seasons_18 <- sep_2018 %>% 
  mutate(season = ifelse(month %in% c("12", "01", "02"), "Summer",        
                         ifelse(month %in% c("03", "04", "05"), "Autumn",
                                ifelse(month %in% c("06", "07", "08"), "Winter",
                                       ifelse(month %in% c("09", "10", "11"), "Spring","Error")))))

# season_18_rm <- as.data.frame(seasons_18[, -1:-8])
# 
# sea_18_rm <- season_18_rm %>% 
#   select(season = `seasons_18[, -1:-8]`)
# 
# sea_18 <- as.data.frame(sea_18_rm[-c(199),]) 
# 
# season_18 <- cbind(time_18, sea_18)
# season_18
# 
# mean_18 <- season_18 %>% 
#   group_by(sea_18_rm[-c(199), ]) %>% 
#   summarise(mean_tp_18 = mean(tp_circ),
#             mean_hs_18 = mean(hs_circ),
#             mean_dp_18 = mean(dp_circ),
#             mean_u_18 = mean(u_circ),
#             mean_v_18 = mean(v_circ))

season_18_rm <- as.data.frame(seasons_18[, -1:-8])

sea_18_rm <- season_18_rm %>% 
  select(season = `seasons_18[, -1:-8]`)

sea_18 <- as.data.frame(sea_18_rm[-c(199),]) 

sea_18_1 <- sea_18 %>% 
  select(season = `sea_18_rm[-c(199), ]`)

season_18 <- cbind(time_18, sea_18_1)

mean_18 <- season_18 %>% 
  group_by(season) %>% 
  summarise(mean_tp_18 = mean(tp_circ),
            mean_hs_18 = mean(hs_circ),
            mean_dp_18 = mean(dp_circ),
            mean_u_18 = mean(u_circ),
            mean_v_18 = mean(v_circ))
# Plots by season ---------------------------------------------------------

# neworder <- c("autumn", "winter", "spring")
# library(plyr)  ## or dplyr (transform -> mutate)
# season_order <- arrange(transform(season_16_1,
#                            season = factor(sea_16_1, levels = neworder)), sea_16_1)

hs_season_2016 <- ggplot(season_16_1, aes(x = as.Date(value), y = hs_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  geom_smooth() +
  geom_hline(data = mean_16, aes(yintercept = mean_hs_16), colour = "red") +
  labs(x = "", y = "HS (m)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ sea_16_1) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_bw()
hs_season_2016

hs_season_2017 <- ggplot(season_17_1, aes(x = as.Date(value), y = hs_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  geom_smooth() +
  geom_hline(data = mean_17, aes(yintercept = mean_hs_17), colour = "red") +
  labs(x = "", y = "HS (m)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ sea_17_1) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_bw()
hs_season_2017

hs_season_2018 <- ggplot(season_18, aes(x = as.Date(value), y = hs_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  geom_smooth() +
  geom_hline(data = mean_18, aes(yintercept = mean_hs_18), colour = "red") +
  labs(x = "", y = "HS (m)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ season) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_bw()
hs_season_2018

ggarrange(hs_season_2016, hs_season_2017, hs_season_2018, nrow = 3, ncol = 1)

tp_season_2016 <- ggplot(season_16_1, aes(x = as.Date(value), y = tp_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  geom_smooth() +
  labs(x = "Date", y = "TP (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ sea_16_1) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_bw()
tp_season_2016

tp_season_2017 <- ggplot(season_17_1, aes(x = as.Date(value), y = tp_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  geom_smooth() +
  labs(x = "Date", y = "TP (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ sea_17_1) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_bw()
tp_season_2017

tp_season_2018 <- ggplot(season_18, aes(x = as.Date(value), y = tp_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  geom_smooth() +
  labs(x = "Date", y = "TP (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  facet_wrap(~ sea_18) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_bw()
tp_season_2018

ggarrange(tp_season_2016, tp_season_2017, tp_season_2018, nrow = 3, ncol = 1)

dp_season_2016 <- ggplot(season_16_1, aes(x = as.Date(value), y = dp_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  geom_smooth() +
  labs(x = "Date", y = "DP (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ sea_16_1) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_bw()
dp_season_2016

dp_season_2017 <- ggplot(season_17_1, aes(x = as.Date(value), y = dp_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  geom_smooth() +
  labs(x = "Date", y = "dp (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ sea_17_1) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_bw()
dp_season_2017

dp_season_2018 <- ggplot(season_18, aes(x = as.Date(value), y = dp_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  geom_smooth() +
  labs(x = "Date", y = "dp (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ sea_18) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_bw()
dp_season_2018

ggarrange(dp_season_2016, dp_season_2017, dp_season_2018, nrow = 3, ncol = 1)

u_season_2016 <- ggplot(season_16_1, aes(x = as.Date(value), y = u_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  geom_smooth() +
  labs(x = "", y = "u (m)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ sea_16_1) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_bw()
u_season_2016

u_season_2017 <- ggplot(season_17_1, aes(x = as.Date(value), y = u_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  geom_smooth() +
  labs(x = "", y = "u (m)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ sea_17_1) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_bw()
u_season_2017

u_season_2018 <- ggplot(season_18, aes(x = as.Date(value), y = u_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  geom_smooth() +
  labs(x = "Date", y = "u (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ sea_18) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_bw()
u_season_2018

ggarrange(u_season_2016, u_season_2017, u_season_2018, nrow = 3, ncol = 1)

v_season_2016 <- ggplot(season_16_1, aes(x = as.Date(value), y = v_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  geom_smooth() +
  labs(x = "", y = "v (m)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ sea_16_1) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_bw()
v_season_2016

v_season_2017 <- ggplot(season_17_1, aes(x = as.Date(value), y = v_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  geom_smooth() +
  labs(x = "", y = "v (m)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ sea_17_1) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_bw()
v_season_2017

v_season_2018 <- ggplot(season_18, aes(x = as.Date(value), y = v_circ, group = 1)) +
  #  geom_point(data = casts.summary, aes(x = as.Date(date), y = n_casts)) +
  geom_line() +
  geom_smooth() +
  labs(x = "Date", y = "v (s)", title = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ sea_18) +
  #  scale_y_continuous(sec.axis = sec_axis(~. *50, name = "Total number of casts")) +
  theme_bw()
v_season_2018

ggarrange(v_season_2016, v_season_2017, v_season_2018, nrow = 3, ncol = 1)

# Stats by season ---------------------------------------------------------


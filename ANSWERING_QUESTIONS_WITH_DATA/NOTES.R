# ANSWERING QUESTIONS WITH DATA
# MATTHEW J. C. CRUMP
# 2024-07-05

# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(datasauRus)

# 02 - DESCRIBING DATA ----------------------------------------------------


happiness <- rnorm(n = 500, mean = 100, sd = 500)

plot(happiness)
hist(happiness)

qplot(x = happiness, bins = 5) 
qplot(x = happiness, bins = 10)
qplot(x = happiness, bins = 20)
qplot(x = happiness, bins = 50)

# Measures of central tendency : Mode
rstatix::get_mode(c(1, 1, 1, 2, 3, 4, 5, 6))
rstatix::get_mode(c(1, 1, 1, 2, 2, 2, 3, 4, 5, 6))
rstatix::get_mode(c(1, 1, 2, 3, 4, 5, 6))

# Measures of central tendency : Median
median(c(1, 5, 4, 3, 6, 7, 9))
median(c(1, 2, 3, 4, 5, 6))
median(c(1, 2, 3, 4, 4, 4, 5, 6, 6, 6, 7, 7, 1000))

# Measures of central tendency : Mean
mean(c(3, 7, 9, 2, 6))

exp_num<-round(rexp(1000,.15),digits=1)

qplot(exp_num, col = I("grey"), fill = I("white")) +
  geom_vline(xintercept = mean(exp_num), col = "red", linewidth = 1.5) +
  geom_vline(xintercept = median(exp_num), col = "green", linewidth = 1.5) +
  geom_vline(xintercept = rstatix::get_mode(exp_num), col = "blue", linewidth = 1.5)

# Measures of variation : Range
range(c(1, 3, 4, 5, 5, 6, 7, 8, 9, 24))

# Measures of variation : Difference scores
tibble(x = rep(c(1, 3, 4, 5, 5, 6, 7, 8, 9, 24), each = 10),
       y = rep(c(1, 3, 4, 5, 5, 6, 7, 8, 9, 24), times = 10)) |> 
  mutate(diff = x - y)

tibble(x = rep(1:3, each = 3),
       y = rep(1:3, times = 3)) |> 
  mutate(diff = x - y) |> 
  summarise(mean_diff = mean(diff))

# Measures of variation : Variance and standard deviation
dt <- tibble(values = c(1, 6, 4, 2, 6, 8)) |> 
  mutate(values_sum = sum(values),
         values_mean = mean(values),
         diff_from_mean = values - values_mean) |> 
  mutate(squared_deviations = diff_from_mean^2)

dt |> 
  summarise(sum_diff_from_mean = sum(diff_from_mean),
            sum_squared_deviations = sum(squared_deviations))

dt |> 
  summarise(variance = sum(squared_deviations) / n()) |> 
  mutate(st_dev = sqrt(variance))

# Mean absolute deviation
dt |> 
  mutate(absolute_deviation_from_mean = abs(diff_from_mean)) |> 
  summarise(mean_absolute_deviation = sum(absolute_deviation_from_mean) / n())

# Remember to look at your data : Anscombe's quartet
aq <- read_csv("ANSWERING_QUESTIONS_WITH_DATA/anscombe.csv")

aq |> 
  ggplot(aes(x, y)) +
  geom_point() +
  facet_wrap(~quartet)

aq |> 
  summarise(mean_x = mean(x),
            var_x = var(x),
            mean_y = mean(y),
            var_y = var(y),
            .by = quartet)

# Remember to look at your data : Datasaurus dozen
datasaurus_dozen |> 
  ggplot(aes(x, y)) +
  geom_point() +
  facet_wrap(~dataset, ncol = 3)

datasaurus_dozen |> 
  summarise(mean_x = mean(x),
            var_x = var(x),
            mean_y = mean(y),
            var_y = var(y),
            corr = cor(x,y),
            .by = dataset)

# 03 - CORRELATION --------------------------------------------------------



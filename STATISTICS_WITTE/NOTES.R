# STATISTICS
# WITTE
# 2024-05-22

# PACKAGES ----------------------------------------------------------------

library(tidyverse)

# DESCRIBING DATA WITH TABLES AND GRAPHS ----------------------------------

# PROGRESS CHECK 2.2

scores <- c(91, 87, 95, 123, 98, 110, 112, 85, 96, 71, 80, 69, 109, 90,
            84, 75, 105, 100, 99, 94, 90, 79, 86, 90, 93, 95, 100, 98,
            80, 104, 77, 108, 90, 103, 89)

boundaries_low <- seq(65, 120, 5)
boundaries_high <- seq(69, 124, 5)

paste(boundaries_low, boundaries_high, sep = "-")

data.frame(
  table(
    cut(x = scores,
    breaks = seq(65, 125, 5),
    labels = paste(boundaries_low, boundaries_high, sep = "-"),
    right = FALSE)))

# PROGRESS CHECK 2.4

summer_income <- c(6450, 4820, 5650, 1720, 600, 0, 3482, 25700, 8548)
age <- c(20, 19, 61, 32, 19, 22, 23, 27, 21)
family_size <- c(2, 4, 3, 6, 18, 2, 6, 3, 4)
gpa <- c(2.3, 4, 3.56, 2.89, 2.15, 3.01, 3.09, 3.5, 3.2)

boxplot(summer_income)
boxplot(age)
boxplot(family_size)
boxplot(gpa)

boxplot.stats(summer_income)$out
boxplot.stats(age)$out
boxplot.stats(family_size)$out
boxplot.stats(gpa)$out

# PROGRESS CHECK 2.5

df <- tibble(
  GRE = paste(seq(725, 475, -25), seq(749, 499, -25), sep = "-"),
  f = c(1, 3, 14, 30, 34, 42, 30, 27, 13, 4, 2))

df |> 
  mutate(f_rel = f / sum(f))

# PROGRESS CHECK 2.6

df |> 
  arrange(GRE) |> 
  mutate(f_cumul = cumsum(f),
         f_cumul_pct = round(100 * f_cumul / max(f_cumul)))

# PROGRESS CHECK 2.8

ratings <- tibble(rating = c("PG", "G", "R", "NC-17", "PG", "PG-13", "PG", "NC-17",
                             "PG", "R", "R", "PG", "PG-13", "PG", "PG","G",
                             "G", "PG", "R", "PG-13"))

ratings |> 
  summarise(n = n(), .by = rating) |> 
  mutate(rating = fct(rating, levels = c("NC-17", "R", "PG-13", "PG", "G"))) |> 
  arrange(rating) |> 
  mutate(rel_freq_pct = 100 * n / sum(n)) |> 
  arrange(rev(rating)) |> 
  mutate(cumul_f = cumsum(rel_freq_pct))

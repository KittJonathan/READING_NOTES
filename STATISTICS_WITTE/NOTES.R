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

# PROGRESS CHECK 2.9

df <- tibble(
  INCOME = paste(seq(130000, 0, -10000),
                 seq(139999, 9999, -10000), sep = "-"),
  f = c(1, 0, 1, 3, 1, 5, 7, 10, 14, 23, 17, 10, 8, 3))

df |> 
  separate(col = INCOME, into = c("START", "END"), sep = "-") |> 
  mutate(START = as.numeric(START),
         END = as.numeric(END)) |> 
  ggplot() +
  geom_rect(aes(xmin = START, xmax = END, ymin = 0, ymax = f),
            fill = "lightgrey", col = "darkgrey") +
  geom_point(aes(x = START + (END - START) / 2, y = f)) +
  geom_line(aes(x = START + (END - START) / 2, y = f))

# PROGRESS CHECK 2.10

scores <- c(120, 126, 108, 102, 98, 85, 141, 132, 118, 88, 123, 109,
            117, 124, 137, 106, 99, 104, 78, 143, 111, 113, 96)

stem(scores, scale = 2)

# PROGRESS CHECK 2.12

pop <- tibble(
  Ethnicity = c("African American", "Asian American", "Hispanic", "White"),
  f = c(37.7, 17.2, 50.5, 196.8)
)

ggplot(pop) +
  geom_col(aes(x = Ethnicity, y = f))

# REVIEW QUESTION 2.14
nb_res <- c(1, 4, 2, 3, 3, 1, 6, 7, 4, 3, 3, 9, 2, 4, 2, 2, 3, 2, 3, 4,
            4, 2, 3, 3, 5)
data.frame(table(nb_res))

# REVIEW QUESTION 2.15
friends <- c(rep("400 - above", 2),
             rep("350 - 359", 5),
             rep("300 - 349", 12),
             rep("250 - 299", 17),
             rep("200 - 249", 23),
             rep("150 - 199", 49),
             rep("100 - 149", 27),
             rep("50 - 99", 29),
             rep("0 - 49", 36))

friends <- fct_rev(fct_inorder(friends))

df <- data.frame(table(friends))

df |> 
  ggplot(aes(x = friends, y = Freq)) +
  geom_point()

df |> 
  mutate(RelFreq = Freq / sum(Freq),
         RelFreqPct = 100 * RelFreq,
         RelFreqPctCumul = cumsum(RelFreqPct))

df |> 
  ggplot(aes(x = friends, y = Freq)) +
  geom_col(width = 1)

df

# REVIEW QUESTION 2.16
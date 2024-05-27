# STATISTICS
# WITTE
# 2024-05-27

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

meditators <- c(3.25, 3.56, 3.57, 2.95, 3.56, 3.45, 3.10, 2.58, 3.30,
                2.25, 3.33, 2.45, 3.30, 3.78, 3.00, 2.75, 2.95, 3.43,
                2.75, 2.25, 3.75, 3.56, 3.75, 3.35, 3.09, 3.56, 3.47)

non_meditators <- c(3.67, 2.50, 3.50, 2.80, 2.83, 3.25, 2.90, 2.34, 3.59,
                    3.79, 2.75, 2.67, 2.65, 3.10, 2.76, 2.10, 3.20, 3.00,
                    3.00, 1.90, 2.90, 2.58, 3.37, 2.86, 2.66, 2.37, 3.08)
summary(meditators)
summary(non_meditators)

boundaries_low <- seq(1.75, 3.75, 0.25)
boundaries_high <- seq(2, 4, 0.25)

paste(boundaries_low, boundaries_high, sep = "-")

freq_med <- data.frame(
  table(
    cut(x = meditators,
        breaks = seq(1.75, 4, 0.25),
        labels = paste(boundaries_low, boundaries_high, sep = "-"),
        right = FALSE))) |> 
  tibble() |> 
  select(class = Var1, freq_med = Freq)

freq_nonmed <- data.frame(
  table(
    cut(x = non_meditators,
        breaks = seq(1.75, 4, 0.25),
        labels = paste(boundaries_low, boundaries_high, sep = "-"),
        right = FALSE))) |> 
  tibble() |> 
  select(class = Var1, freq_nonmed = Freq)

freq_tbl <- left_join(freq_med, freq_nonmed)


freq_tbl

non_meditators[non_meditators < 2]

ggplot(freq_tbl) +
  geom_col(aes(x = class, y = freq_med),
           col = "lightblue", fill = "lightblue", alpha = 0.5,
           width = 1) +
  geom_col(aes(x = class, y = freq_nonmed),
           col = "lightgreen", fill = "lightgreen", alpha = 0.5,
           width = 1)

# REVIEW QUESTION 2.17

ages_low <- seq(65, 0, -5)
ages_high <- c("above", seq(64, 4, -5))

df <- tibble(
  AGE = paste(ages_low, ages_high, sep = "-"),
  SMALL_TOWN_F = c(105, 53, 45, 40, 44, 38, 31, 27, 25, 20, 20, 19, 17, 16),
  US_POP_PCT = c(13, 5, 6, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7)
)

df |> 
  mutate(SMALL_TOWN_PCT = 100 * SMALL_TOWN_F / sum(SMALL_TOWN_F)) |> 
  rowid_to_column() |> 
  ggplot() +
  geom_line(aes(x = rowid, y = SMALL_TOWN_PCT), col = "red", linewidth = 1) +
  geom_line(aes(x = rowid, y = US_POP_PCT), col = "blue", linewidth = 1)

# REVIEW QUESTION 2.18

df <- tibble(
  FIELD = c("Business", "Social sciences", "Education", "Health sciences", "Psychology",
            "Engineering", "Life sciences", "Fine arts",
            "Communications", "Computer sciences", "English"),
  MALES = c(190, 90.6, 21.8, 24.9, 25.4, 81.3, 39.5, 37.2, 33.5, 39.8, 17),
  FEMALES = c(176.7, 87.9, 84, 138.6, 83.6, 17.3, 56.3, 58.6, 55.2, 8.6, 36.8)
)

df |> 
  mutate(M_PCT = 100 * MALES / sum(MALES),
         F_PCT = 100 * FEMALES / sum(FEMALES)) |> 
  select(FIELD, M_PCT, F_PCT) |> 
  pivot_longer(cols = -FIELD, names_to = "SEX", values_to = "PCT") |> 
  mutate(SEX = case_when(SEX == "M_PCT" ~ "MALE",
                         .default = "FEMALE")) |> 
  ggplot() +
  geom_col(aes(x = FIELD, y = PCT, fill = SEX), position = "dodge")

# DESCRIBING DATA WITH AVERAGES -------------------------------------------

# PROGRESS CHECK 3.1
ages <- c(60, 63, 45, 63, 65, 70, 55, 63, 60, 65, 63)
names(sort(-table(ages)))[1]

# PROGRESS CHECK 3.2
mpg <- c(26.3, 28.7, 27.4, 26.6, 27.4, 26.9)
names(sort(-table(mpg)))[1]

# PROGRESS CHECK 3.3
ages <- c(60, 63, 45, 63, 65, 70, 55, 63, 60, 63, 65)
ages_sorted <- sort(ages)
val <- (length(ages_sorted) + 1) / 2
ages_sorted[val]
median(ages)

# PROGRESS CHECK 3.4
miles <- c(26.3, 28.7, 27.4, 26.6, 27.4, 26.9)
miles_sorted <- sort(miles)
val <- (length(miles_sorted) + 1) / 2
miles_median <- (miles_sorted[floor(val)] + miles_sorted[ceiling(val)]) / 2
median(miles)

# PROGRESS CHECK 3.5
ages <- c(60, 63, 45, 63, 65, 70, 55, 63, 60, 63, 65)
sum(ages) / length(ages)
mean(ages)

# PROGRESS CHECK 3.6
miles <- c(26.3, 28.7, 27.4, 26.6, 27.4, 26.9)
sum(miles) / length(miles)
mean(miles)

# PROGRESS CHECK 3.8
locations <- c("DB", "C", "O", "DB", "DB", "SP", "SP", "C",
               "C", "LH", "C", "DB", "LH", "DB", "DB", "O",
               "DB", "O", "LH", "DB")
names(sort(-table(locations)))[1]
table(locations)

# REVIEW QUESTION 3.9
scores <- c(1, 3, 4, 1, 0, 2, 5, 8, 0, 2, 3, 4, 7, 11, 0, 2, 3, 3)
table(scores)
names(sort(-table(scores)))[1]  # 3

scores_sorted <- sort(scores)
val <- (length(scores_sorted) + 1) / 2

scores_median <- (scores_sorted[floor(val)] + scores_sorted[ceiling(val)]) / 2  # 3
median(scores)

scores_mean <- sum(scores) / length(scores)  # 3.278
mean(scores)

# REVIEW QUESTION 3.10
errors <- c(2, 17, 5, 3, 28, 7, 5, 8, 5, 6, 2, 12, 10, 4, 3)

names(sort(table(errors), decreasing = TRUE)[1])  # mode = 5

errors_sorted <- sort(errors)
val <- (length(errors) + 1) / 2
errors_sorted[8]  # median = 5
median(errors)

sum(errors) / length(errors)  # mean = 7.8
mean(errors)

# REVIEW QUESTION 3.11
mean(c(5, 5, 15))
median(c(5, 5, 15))  # median

mean(c(5, 10, 10))  # mean
median(c(5, 10, 10))

mean(c(3, 3, 10))
median(c(3, 3, 10))  # median

mean(c(1, 6, 11))  # mean or median
median(c(1, 6, 11))

# REVIEW QUESTION 3.14
scores <- c(3, 6, 2, 0, 4)
sum(scores - mean(scores))

sum(scores - 5)

# REVIEW QUESTION 3.15
ratings <- tibble(rating = c("PG", "G", "R", "NC-17", "PG", "PG-13", "PG", "NC-17",
                             "PG", "R", "R", "PG", "PG-13", "PG", "PG","G",
                             "G", "PG", "R", "PG-13"))

ratings |> 
  summarise(n = n(), .by = rating) |> 
  mutate(rating = fct(rating, levels = c("NC-17", "R", "PG-13", "PG", "G"))) |> 
  mutate(pct = n / sum(n),
         cumul_n = cumsum(n),
         cumul_pct = cumsum(100 * cumul_n / sum(cumul_n)))

# REVIEW QUESTION 3.18

mean(c(1, 2, 10, 7))
mean(c(2, 4, 1, 5, 7, 7, 9))
mean(c(6, 9, 2, 7, 1, 2, 8))

# DESCRIBING VARIABILITY --------------------------------------------------

# PROGRESS CHECK 4.4
scores <- c(1, 3, 4, 4)
n <- length(scores)
sum_scores <- sum(scores)
mean_scores <- sum(scores) / n
score_to_mean <- scores - mean_scores
scores_to_mean_sq <- score_to_mean ^ 2
scores_to_mean_sq_sum <- sum(scores_to_mean_sq)
var_scores <- scores_to_mean_sq_sum / (n-1)
sd_scores <- sqrt(var_scores)

# PROGRESS CHECK 4.5.a
scores <- c(1, 3, 7, 2, 0, 4, 7, 3)
scores_n <- length(scores)
scores_sum <- sum(scores)
scores_sum_sq <- scores_sum^2
scores_sq_sum <- sum(scores^2)
sum_of_squares <- scores_sq_sum - scores_sum_sq / scores_n
scores_var <- sum_of_squares / scores_n
scores_sd <- sqrt(scores_var)

# PROGRESS CHECK 4.5.b
scores <- c(10, 8, 5, 0, 1, 1, 7, 9, 2)
scores_n <- length(scores)
scores_sum <- sum(scores)
scores_sum_sq <- scores_sum^2
scores_sq_sum <- sum(scores^2)
sum_of_squares <- scores_sq_sum - scores_sum_sq / (scores_n)
scores_var <- sum_of_squares / (scores_n - 1)
scores_sd <- sqrt(scores_var)

# PROGRESS CHECK 4.6
scores <- c(8, 5, 7, 1, 4, 0, 5, 7, 2, 9)
n <- length(scores)
sum_scores <- sum(scores)
mean_scores <- sum(scores) / n
score_to_mean <- scores - mean_scores
scores_to_mean_sq <- score_to_mean ^ 2
scores_to_mean_sq_sum <- sum(scores_to_mean_sq)
var_scores <- scores_to_mean_sq_sum / (n-1)
sd_scores <- sqrt(var_scores)

# PROGRESS CHECK 4.8.a
ages <- c(60, 63, 45, 63, 65, 70, 55, 63, 60, 65, 63)
ages_sorted <- sort(ages)
value <- (length(ages_sorted) + 1) / 4
Q3 <- rev(ages_sorted)[value]
Q1 <- ages_sorted[value]
IQR <- Q3 - Q1
IQR(ages, type = 1)

# PROGRESS CHECK 4.8.b
changes <- c(1, 3, 4, 1, 0, 2, 5, 8, 0, 2, 3, 4, 7, 11, 0, 2, 3, 4)
changes_sorted <- sort(changes)
value <- (length(changes_sorted) + 1) / 4  # 4.75 -> 5
Q3 <- rev(changes_sorted)[5]
Q1 <- changes_sorted[5]
IQR <- Q3 - Q1
IQR(changes, type = 1)

# REVIEW QUESTION 4.14.a
weights <- c(160, 193, 226, 157, 180, 205, 165, 168, 169, 160, 163, 172, 151, 157,
             133, 245, 170, 152, 160, 220, 190, 170, 160, 180, 158, 170, 166, 206,
             150, 152, 150, 225, 145, 152, 172, 165, 190, 156, 135, 185, 159, 175,
             158, 179, 190, 165, 152, 156, 154, 165, 157, 156, 135)

weights_n <- length(weights)
weights_sum <- sum(weights)
weights_mean <- weights_sum / weights_n
sum_sq <- sum((weights - weights_mean)^2)
weights_var <- sum_sq / (weights_n - 1)
weights_sd <- sqrt(weights_var)
weights_sd == sd(weights)

# REVIEW QUESTION 4.14.b
tibble(weight = weights) |> 
  filter(weight >= weights_mean - weights_sd &  weight <= weights_mean + weights_sd) |> 
  count() |> 
  mutate(pct = 100 * n / 53)
  
169.51 - 23.33
169.51 + 23.33

tibble(weight = weights) |> 
  filter(weight <= weights_mean - 2*weights_sd | weight >= weights_mean + 2*weights_sd) |> 
  count() |> 
  mutate(pct = 100 * n / 53)

# NORMAL DISTRIBUTIONS AND STANDARD (Z) SCORES ----------------------------



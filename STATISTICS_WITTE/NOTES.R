# STATISTICS
# WITTE
# 2024-05-27

# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(rMR)
library(rstatix)
library(tidymodels)

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

# PROGRESS CHECK 5.1
(135-100)/15
(470-500)/100
(2100-2180)/50
(69-69)/3
(-3-0)/2

# PROGRESS CHECK 5.2
1-pnorm(1.80) 
# 0.5-pnorm(-0.43)
pnorm(0.43)-0.5
1-pnorm(3)
pnorm(1.65)-0.5
# 0.5-pnorm(-1.96)
pnorm(1.96)-0.5

# PROGRESS CHECK 5.3
pnorm(400, mean = 500, sd = 100)
1-pnorm(650, mean = 500, sd = 100)
pnorm(700, mean = 500, sd = 100)

# PROGRESS CHECK 5.5
1 - pnorm(570, mean = 500, sd = 100)
pnorm(515, mean = 500, sd = 100)
pnorm(540, mean = 500, sd = 100) - pnorm(520, mean = 500, sd = 100)
pnorm(520, mean = 500, sd = 100) - pnorm(470, mean = 500, sd = 100)
1 - pnorm(550, mean = 500, sd = 100)
pnorm(400, mean = 500, sd = 100) + (1 - pnorm(600, mean = 500, sd = 100))
pnorm(550, mean = 500, sd = 100) - pnorm(450, mean = 500, sd = 100)

# PROGRESS CHECK 5.6
qnorm(p = 0.01, mean = 1200, sd = 120)
qnorm(p = 0.5, mean = 1200, sd = 120)
qnorm(p = 0.95, mean = 1200, sd = 120)
qnorm(p = 0.08, mean = 1200, sd = 120)

# PROGRESS CHECK 5.7
(53-50)/9
(38-40)/10
(45-30)/20
(28-20)/20

# PROGRESS CHECK 5.8
(64-50)/9
(64-40)/10
(64-30)/20
(64-20)/20

# PROGRESS CHECK 5.9.a
z <- (24-20)/5
50 + 10*z
100 + 15*z
500 + 100*z

# PROGRESS CHECK 5.9.b
z <- (37-42)/3
50 + 10*z
100 + 15*z
500 + 100*z

# REVIEW QUESTION 5.11
1 - pnorm(q = 125, mean = 100, sd = 15)
pnorm(q = 82, mean = 100, sd = 15)
pnorm(q = 109, mean = 100, sd = 15) - pnorm(q = 91, mean = 100, sd = 15)
pnorm(q = 60, mean = 100, sd = 15) + (1 - pnorm(q = 140, mean = 100, sd = 15))

# REVIEW QUESTION 5.12
pnorm(q = 960, mean = 1200, sd = 120)
1 - pnorm(q = 1500, mean = 1200, sd = 120)
pnorm(q = 1250, mean = 1200, sd = 120) - pnorm(q = 1150, mean = 1200, sd = 120)
pnorm(q = 1400, mean = 1200, sd = 120) - pnorm(q = 1300, mean = 1200, sd = 120)

# REVIEW QUESTION 5.13
qnorm(p = 0.98, mean = 100, sd = 15)
qnorm(p = 0.10, mean = 100, sd = 15)
qnorm(p = 0.60, mean = 100, sd = 15)
c(qnorm(p = 0.025, mean = 100, sd = 15), qnorm(p = 0.975, mean = 100, sd = 15))
c(qnorm(p = 0.005, mean = 100, sd = 15), qnorm(p = 0.995, mean = 100, sd = 15))

# REVIEW QUESTION 5.14
qnorm(p = 0.5, mean = 1200, sd = 120)
qnorm(p = 0.25, mean = 1200, sd = 120)
qnorm(p = 0.01, mean = 1200, sd = 120)
c(qnorm(p = 0.05, mean = 1200, sd = 120), qnorm(p = 0.95, mean = 1200, sd = 120))

# REVIEW QUESTION 5.15
qnorm(p = 0.05, mean = 83, sd = 20)
1 - pnorm(q = 48, mean = 83, sd = 20)
pnorm(q = 61, mean = 83, sd = 20)
c(qnorm(p = 0.01, mean = 83, sd = 20), qnorm(p = 0.99, mean = 83, sd = 20))
pnorm(q = 72, mean = 83, sd = 20) - pnorm(q = 24, mean = 83, sd = 20)
c(qnorm(p = 0.025, mean = 83, sd = 20), qnorm(p = 0.975, mean = 83, sd = 20))
pnorm(q = 96, mean = 83, sd = 20) - pnorm(q = 48, mean = 83, sd = 20)
qnorm(p = 0.8, mean = 83, sd = 20)
c(qnorm(p = 0.03, mean = 83, sd = 20), qnorm(p = 0.97, mean = 83, sd = 20))
1 - pnorm(q = 61, mean = 83, sd = 20)

# REVIEW QUESTION 5.16
1 - pnorm(q = 3.5, mean = 3.20, sd = 0.30)
pnorm(q = 2.5, mean = 3.20, sd = 0.30)
1 - pnorm(q = 3.75, mean = 3.20, sd = 0.30)
qnorm(p = 0.9, mean = 3.2, sd = 0.3)

# REVIEW QUESTION 5.17.a
z <- (34-41)/5
50 + 10*z
100 + 15*z
500 + 100*z

# REVIEW QUESTION 5.17.b
z <- (880-700)/120
50 + 10*z
100 + 15*z
500 + 100*z

# REVIEW QUESTION 5.17.c
z <- (-3-12)/10
50 + 10*z
100 + 15*z
500 + 100*z

# REVIEW QUESTION 5.18
# a. mean exceeds median
(25-28)/4
(30-28)/4

# DESCRIBING RELATIONSHIPS : CORRELATION ----------------------------------

cards <- tibble(
  name = c("Doris", "Steve", "Mike", "Andrea", "John"),
  sent = c(13, 9, 7, 5, 1),
  received = c(14, 18, 12, 10, 6)
  )

ggplot(cards) +
  geom_point(aes(x = sent, y = received))

# PROGRESS CHECK 6.5
df <- tibble(
  COUPLE = LETTERS[1:6],
  X = c(1, 3, 2, 3, 1, 2),
  Y = c(2, 4, 3, 2, 0, 3)
  )

cor(df$X, df$Y)

SSx <- rMR::sumsq(df$X)
SSy <- rMR::sumsq(df$Y)
SPxy <- sum((as.vector(scale(df$X)) * sd(df$X)) * (as.vector(scale(df$Y)) * sd(df$Y)))
SPxy / sqrt(SSx * SSy)

cards <- tibble(
  name = c("Doris", "Steve", "Mike", "Andrea", "John"),
  sent = c(13, 9, 7, 5, 1),
  received = c(14, 18, 12, 10, 6)
  )

cor(cards$sent, cards$received)

# REVIEW QUESTION 6.7
df <- tibble(
  X = c(64, 40, 30, 71, 55, 31, 61, 42, 57),
  Y = c(66, 79, 98, 65, 76, 83, 68, 80, 72)
  )
ggplot(df, aes(X, Y)) + geom_point()

SS_X <- sum((df$X - mean(df$X))^2)  # rMR::sumsq(df$X)
SS_Y <- sum((df$Y - mean(df$Y))^2)  # rMR::sumsq(df$Y)
SP_XY <- sum((df$X - mean(df$X)) * (df$Y - mean(df$Y)))
r_coeff <- SP_XY / sqrt(SS_X * SS_Y)
cor(df$X, df$Y)

# REVIEW QUESTION 6.8
df <- tibble(
  X = c(2, 4, 5, 3, 1, 7, 2),
  Y = c(8, 6, 2, 3, 4, 1, 4)
)
ggplot(df, aes(X, Y)) + geom_point()

SS_X <- sum((df$X - mean(df$X))^2)  # rMR::sumsq(df$X)
SS_Y <- sum((df$Y - mean(df$Y))^2)  # rMR::sumsq(df$Y)
SP_XY <- sum((df$X - mean(df$X)) * (df$Y - mean(df$Y)))
r_coeff <- SP_XY / sqrt(SS_X * SS_Y)
cor(df$X, df$Y)


# REGRESSION --------------------------------------------------------------

cards <- tibble(
  name = c("Doris", "Steve", "Mike", "Andrea", "John"),
  sent = c(13, 9, 7, 5, 1),
  received = c(14, 18, 12, 10, 6)
  )

slope <- (cor(cards$sent, cards$received)) * sqrt(rMR::sumsq(cards$received) / rMR::sumsq(cards$sent))
intercept <- mean(cards$received) - slope * mean(cards$sent)
fit <- lm(received ~ sent, data = cards)
test <- data.frame(sent = c(0, 4, 8, 10, 12, 20, 30))
predict(fit, newdata = cards$sent)

summary(fit)

StErrEst <- sqrt((rMR::sumsq(cards$received) * (1 - 0.8^2)) / 3)

# PROGRESS CHECK 7.3
df <- tibble(
  X = c(0, 4, 8, 10, 12, 20, 30),
  Ypred = c(6.4, 9.6, 12.8, 14.4, 16, 22.4, 30.4)
  )

sqrt(50 * (1 - 0.3^2) / (35 - 2))

# PROGRESS CHECK 7.4
100*0.3^2
100 - (100*0.3^2)
0.5^2 / 0.27^2

# REVIEW QUESTION 7.7
b <- -0.80 * sqrt(70/35)
a <- 60 - (b * 5)
# Y' = -1.13x + 65.66
sqrt((70 * (1 - (-0.80^2))) / (50 - 2))
- 1.13*8 + 65.66
- 1.13*0 + 65.66

# REVIEW QUESTION 7.8
df <- tibble(drivers = c(5, 5, 2, 2, 3, 1, 2),
             cars = c(4, 3, 2, 2, 2, 1, 2))
ggplot(df, aes(drivers, cars)) + geom_point()
cor_coeff <- cor(x = df$drivers, y = df$cars)
SS_X <- rMR::sumsq(df$drivers)
SS_Y <- rMR::sumsq(df$cars)
b <- cor_coeff * sqrt(SS_Y / SS_X)
a <- mean(df$cars) - b * mean(df$drivers)
# Y' = 0.56x + 0.69
sqrt((SS_Y * (1 - cor_coeff^2)) / 5)
0.56*2 + 0.69
0.56*5 + 0.69

# SAMPLING DISTRIBUTION OF THE MEAN ---------------------------------------

scores <- seq(2, 10, 2)

tidyr::crossing(var1 = scores, var2 = scores) |> 
  mutate(mean = (var1 + var2) / 2) |> 
  ggplot(aes(x = mean)) +
  geom_histogram()

# INTRODUCTION TO HYPOTHESIS TESTING: THE z TEST --------------------------

# PROGRESS CHECK 10.1
(566-560)/(30/sqrt(36))
(24-25)/(4/sqrt(64))
(82-75)/(14/sqrt(49))
(136-146)/(15/sqrt(25))

# PROGRESS CHECK 10.5
z <- (80100 - 82500) / (6000 / sqrt(100))
z

# REVIEW QUESTION 10.6
(12 - 15) / (9 / sqrt(25))
(3600 - 3500) / (4000 / sqrt(100))
(0.25 - 0.22) / (0.10 / sqrt(36))

# REVIEW QUESTION 10.8
(105 - 100) / (15 / sqrt(25))  # z = 1.67 retain H0

# REVIEW QUESTION 10.9
(22.2 - 21.75) / (3.1 / sqrt(30))  # z = 0.795 retain H0

# REVIEW QUESTION 10.10
(30 - 35) / (10 / sqrt(20))  # z = -2.24 reject H0

# REVIEW QUESTION 10.11
(385 - 400) / (53 / sqrt(48))  # z = -1.96 reject H0

# REVIEW QUESTION 10.12
(22.5 - 25) / (13 / sqrt(169))  # z = -2.5 reject H0

# MORE ABOUT HYPOTHESIS TESTING -------------------------------------------

# PROGRESS CHECK 11.2
(22.5 - 25) / (13 / sqrt(169))  # z = -2.5 reject H0

# ESTIMATION --------------------------------------------------------------

# REVIEW QUESTION 12.7
80100 - (2.58 * (6000 / sqrt(100)))
80100 + (2.58 * (6000 / sqrt(100)))

# REVIEW QUESTION 12.8
33.09 - (1.96 * (0.3 / sqrt(36)))
33.09 + (1.96 * (0.3 / sqrt(36)))

# T TEST FOR ONE SAMPLE ---------------------------------------------------

# PROGRESS CHECK 13.2
weights <- c(16, 15, 14, 15, 14, 15, 16, 14, 14, 14)
n <- length(weights)
sum_weights <- sum(weights)
mean_weights <- sum_weights / n
weights_sq <- weights^2
sum_weights_sq <- sum(weights_sq)
sum_of_squares <- sum_weights_sq - ((sum_weights^2) / n)
rMR::sumsq(weights)
standard_dev <- sqrt(sum_of_squares / (n - 1))
sd(weights)
est_sd_mean <- standard_dev / sqrt(n)

# PROGRESS CHECK 13.3
t_val <- (14.7 - 16) / 0.26
t.test(weights, alternative = "less", mu = 16, conf.level = 0.95)

# PROGRESS CHECK 13.4
14.7 - 2.26 * 0.26
14.7 + 2.26 * 0.26

# REVIEW QUESTION 13.5
duration <- c(21, 15, 12, 24, 20, 21, 13, 16)
t_val <- (mean(duration) - 21) / (sd(duration) / sqrt(8))

t.test(duration, alternative = "two.sided", mu = 21, conf.level = 0.95)

# REVIEW QUESTION 13.6
t_crit <- 2.447
t_val <- (34.89 - 32) / (3.02 / sqrt(7))
t_val > t_crit

# REVIEW QUESTION 13.7
temp <- c(1.15, 1.15, 1.01, 1.03, 1.15, 1.22, 1.03, 1.13, 1.21, 1.35)
(mean(temp) - 0) / (sd(temp) / sqrt(10))
t.test(temp, alternative = "greater", mu = 0, conf.level = 0.99)

1.14 - 3.25 * (sd(temp)/sqrt(10))
1.14 + 3.25 * (sd(temp)/sqrt(10))

1.14 - 3.25 * 0.03
1.14 + 3.25 * 0.03

# REVIEW QUESTION 13.8
# H0 : mean = 90
# H1 : mean != 90
(88 - 90) / (9 / sqrt(28))
dream <- c(88, 78, 98, 68, 108, 58, 118, 88, 78, 98, 68, 108, 58, 118,
           88, 78, 98, 68, 108, 58, 118, 88, 78, 98, 68, 108, 58, 118)
# t_crit = 2.052
t.test(dream, alternative = "two.sided", mu = 90, conf.level = 0.95)

# T TEST FOR TWO INDEPENDENT SAMPLES --------------------------------------

x1 <- c(12, 5, 11, 11, 9, 18)
x2 <- c(7, 3, 4, 6, 3, 13)

(rMR::sumsq(x1) + rMR::sumsq(x2)) / 10

sqrt((16.2 / 6) + (16.2 / 6))

t_val <- (mean(x1) - mean(x2)) / 2.32

t.test(x = x1, y = x2)

# PROGRESS CHECK 14.3
task_difficult <- c(5, 20, 7, 23, 30, 24, 9, 8, 20, 12)
task_easy <- c(13, 6, 6, 5, 3, 6, 10, 20, 9, 12)

sum_squares_difficult <- rMR::sumsq(task_difficult)
sum_squares_easy <- rMR::sumsq(task_easy)

pooled_var <- (sum_squares_difficult + sum_squares_easy) / (10 + 10 - 2)

st_err <- sqrt((pooled_var / 10) + (pooled_var / 10))

t_val <- (mean(task_difficult) - mean(task_easy)) / st_err

# t_crit = 2.101 -> reject H0

t.test(x = task_difficult, y = task_easy, mu = 0, conf.level = 0.95)

# PROGRESS CHECK 14.8
d_val <- (15.8 - 9) / 7.06

# REVIEW QUESTION 14.11
x1 <- c(2, 5, 20, 15, 4, 10)
x2 <- c(3, 8, 7, 10, 14, 0)

sum_squares_x1 <- rMR::sumsq(x1)
sum_squares_x2 <- rMR::sumsq(x2)

pooled_var <- (sum_squares_x1 + sum_squares_x2) / (6 + 6 - 2)

st_err <- sqrt((pooled_var / 6) + (pooled_var / 6))

t_val <- (mean(x1) - mean(x2)) / st_err
qt(p = 0.025, df = 10, lower.tail = FALSE)

t.test(x1, x2, conf.level = 0.95)

# REVIEW QUESTION 14.12
t_val <- (110 - 108) / 1.80
qt(p = 0.01, df = 68, lower.tail = FALSE)

# REVIEW QUESTION 14.13
t_val <- (86.2 - 81.6) / 1.5
qt(p = 0.025, df = 38, lower.tail = FALSE)
d_val <- (86.2 - 81.6) / 5

# REVIEW QUESTION 14.14
t_val <- (26.4 - 18.6) / 2.4
qt(p = 0.05, df = 118, lower.tail = FALSE)
(26.4 - 18.6) - (2.4*1.66)
(26.4 - 18.6) + (2.4*1.66)
d_val <- (26.4 - 18.6) / 13.15

# REVIEW QUESTION 14.15
t_val <- (3.19 - 2.90) / 0.20
qt(p = 0.05, df = 52, lower.tail = FALSE)

# T TEST FOR TWO RELATED SAMPLES (REPEATED MEASURES) ----------------------

# PROGRESS CHECK 15.2
x1 <- c(2, 5, 7, 0, 3, 7, 4, 5, 1, 3)
x2 <- c(3, 4, 9, 3, 5, 7, 6, 8, 2, 5)

D <- x1 - x2
D_mean <- sum(D) / length(x1)

D_sq <- D^2
D_sum_sq <- sum(D^2) - (sum(D)^2 / length(x1))

st_dev_D <- sqrt(D_sum_sq / (length(x1 - 1)))

st_err_D_bar <- st_dev_D / sqrt(length(x1))

t_val <- (D_mean - 0) / st_err_D_bar

t_crit <- qt(p = 0.05, df = 9)

t.test(x1, x2, paired = TRUE)

# PROGRESS CHECK 15.3
-1.5 - 2.262 * st_err_D_bar
-1.5 + 2.262 * st_err_D_bar
t.test(x1, x2, paired = TRUE)

# PROGRESS CHECK 15.4
cohen_d <- -1.5 / 1.27

# PROGRESS CHECK 15.6
t_val <- 0.43 / sqrt((1 - 0.43^2) / (27 - 2))
t_crit <- qt(p = 0.025, df = 25, lower.tail = FALSE)

# REVIEW QUESTION 15.7
x1 <- c(4, 2.67, 3.65, 2.11, 3.21, 3.6, 2.8)
x2 <- c(3.75, 2.74, 3.42, 1.67, 3, 3.25, 2.65)

D <- x1 - x2
D_mean <- sum(D) / length(x1)

D_sq <- D^2
D_sum_sq <- sum(D^2) - (sum(D)^2 / length(x1))

st_dev_D <- sqrt(D_sum_sq / (length(x1 - 1)))

st_err_D_bar <- st_dev_D / sqrt(length(x1))

cohen_d <- D_mean / st_dev_D
t_val <- (D_mean - 0) / st_err_D_bar

t_crit <- qt(p = 0.01, df = 6, lower.tail = FALSE)

t.test(x1, x2, paired = TRUE)

# REVIEW QUESTION 15.8
x1 <- c(28, 29, 31, 44, 35, 20, 50, 25)
x2 <- c(26, 27, 32, 44, 35, 16, 47, 23)

D <- x1 - x2
D_mean <- sum(D) / length(x1)

D_sq <- D^2
D_sum_sq <- sum(D^2) - (sum(D)^2 / length(x1))

st_dev_D <- sqrt(D_sum_sq / (length(x1 - 1)))

st_err_D_bar <- st_dev_D / sqrt(length(x1))

t_val <- (D_mean - 0) / st_err_D_bar

t_crit <- qt(p = 0.05, df = 7, lower.tail = FALSE)

t.test(x1, x2, paired = TRUE)

cohen_d <- D_mean / st_dev_D

# REVIEW QUESTION 15.9
# x1 <- c(28, 29, 31, 44, 35, 20, 50, 25)
# x2 <- c(26, 27, 32, 44, 35, 16, 47, 23)

# D <- x1 - x2
D_mean <- 2.12

D_sq <- D^2
D_sum_sq <- sum(D^2) - (sum(D)^2 / length(x1))

st_dev_D <- sqrt(D_sum_sq / (length(x1 - 1)))

st_err_D_bar <- 1.5

t_val <- (D_mean - 0) / st_err_D_bar

t_crit <- qt(p = 0.05, df = 29, lower.tail = FALSE)

t.test(x1, x2, paired = TRUE)

cohen_d <- D_mean / st_dev_D

# REVIEW QUESTION 15.10
# x1 <- c(28, 29, 31, 44, 35, 20, 50, 25)
# x2 <- c(26, 27, 32, 44, 35, 16, 47, 23)

# D <- x1 - x2
D_mean <- 51.33

# D_sq <- D^2
# D_sum_sq <- sum(D^2) - (sum(D)^2 / length(x1))

st_dev_D <- 66.33

st_err_D_bar <- 66.33 / sqrt(12)

t_crit <- qt(p = 0.05, df = 11, lower.tail = FALSE)

t_val <- D_mean / st_err_D_bar

D_mean - 2.2 * st_err_D_bar
D_mean + 2.2 * st_err_D_bar

cohen_d <- D_mean / st_dev_D

# REVIEW QUESTION 15.13
t_val <- -0.24 / sqrt((1 - (-0.24)^2) / (38 - 2))
t_crit <- qt(p = 0.01, df = 36, lower.tail = TRUE)

# REVIEW QUESTION 15.14
D <- c(28, 53, 17, 37, 27, 27, 22, -25, -7, 0)
D_mean <- mean(D)

# D_sq <- D^2
# D_sum_sq <- sum(D^2) - (sum(D)^2 / length(x1))

st_dev_D <- sd(D)

st_err_D_bar <- st_dev_D / sqrt(10)

t_crit <- qt(p = 0.05, df = 9, lower.tail = FALSE)

t_val <- D_mean / st_err_D_bar

D_mean - 2.262 * st_err_D_bar
D_mean + 2.262 * st_err_D_bar

cohen_d <- D_mean / st_dev_D

# ANALYSIS OF VARIANCE (ONE FACTOR) ---------------------------------------

#  Calculation of ss terms
df <- tibble(
  hrs0 = c(0, 4, 2),
  hrs24 = c(3, 6, 6),
  hrs48 = c(6, 8, 10)) |> 
  pivot_longer(cols = everything(), names_to = "hrs_deprivation",
               values_to = "score", names_prefix = "hrs") |> 
  arrange(hrs_deprivation) |> 
  mutate(hrs_deprivation = fct(hrs_deprivation))

df

group_totals <- df |> 
  summarise(gp_total = sum(score), .by = hrs_deprivation)

grand_total <- sum(df$score)

ss_between <- sum(group_totals$gp_total^2 / 3) - (grand_total^2 / 9)

ss_within <- sum(df$score^2) - sum(group_totals$gp_total^2 / 3)

ss_total <- sum(df$score^2) - (grand_total^2 / 9)

ss_total == ss_between + ss_within

mean_sq_between <- ss_between / (3 - 1)

mean_sq_within <- ss_within / (9 - 3)

f_ratio <- mean_sq_between / mean_sq_within

# PROGRESS CHECK 16.3
qf(p = 0.05, df1 = 1, df2 = 18, lower.tail = FALSE)
qf(p = 0.01, df1 = 3, df2 = 56, lower.tail = FALSE)
qf(p = 0.05, df1 = 2, df2 = 36, lower.tail = FALSE)
qf(p = 0.05, df1 = 4, df2 = 95, lower.tail = FALSE)

# PROGRESS CHECK 16.3
qf(df1 = 2, df2 = 11, p = c(0.05, 0.01), lower.tail = FALSE)
qf(df1 = 1, df2 = 13, p = c(0.05, 0.01), lower.tail = FALSE)
qf(df1 = 3, df2 = 20, p = c(0.05, 0.01), lower.tail = FALSE)
qf(df1 = 2, df2 = 29, p = c(0.05, 0.01), lower.tail = FALSE)

# PROGRESS CHECK 16.5
df <- tibble(
  gp0 = c(1, 0, 0, 2, 3, 4, 2, 1),
  gp1 = c(2, 1, 2, 4, 4, 6, 3, 3),
  gp2 = c(4, 2, 3, 6, 7, 8, 5, 5),
  gp3 = c(7, 1, 6, 9, 10, 12, 8, 7)) |> 
  pivot_longer(cols = everything(), names_to = "nb_sessions",
               values_to = "eye_contacts", names_prefix = "gp") |> 
  arrange(nb_sessions) |> 
  mutate(nb_sessions = fct(nb_sessions))

df

group_totals <- df |> 
  summarise(gp_total = sum(eye_contacts), .by = nb_sessions)

grand_total <- sum(df$eye_contacts)

ss_between <- sum(group_totals$gp_total^2 / 8) - (grand_total^2 / 32)

ss_within <- sum(df$eye_contacts^2) - sum(group_totals$gp_total^2 / 8)

ss_total <- sum(df$eye_contacts^2) - (grand_total^2 / 32)

ss_total == ss_between + ss_within

mean_sq_between <- ss_between / (4 - 1)

mean_sq_within <- ss_within / (32 - 4)

f_ratio <- mean_sq_between / mean_sq_within

f_crit <- qf(df1 = 3, df2 = 28, p = 0.05, lower.tail = FALSE)

test <- rstatix::anova_test(data = df, formula = eye_contacts ~ nb_sessions)

rstatix::anova_summary(test)

df |> 
  specify(eye_contacts ~ nb_sessions) |> 
  hypothesize(null = "independence") |> 
  calculate(stat = "F")

test <- aov(eye_contacts ~ nb_sessions, df)
rstatix::anova_summary(test, detailed = TRUE)

# PROGRESS CHECK 16.6
effect_size <- ss_between / ss_total

TukeyHSD(x = test)

# Tukey's HSD
df <- tibble(
  hrs0 = c(0, 4, 2),
  hrs24 = c(3, 6, 6),
  hrs48 = c(6, 8, 10)) |> 
  pivot_longer(cols = everything(), names_to = "hrs_deprivation",
               values_to = "score", names_prefix = "hrs") |> 
  arrange(hrs_deprivation) |> 
  mutate(hrs_deprivation = fct(hrs_deprivation))

group_totals <- df |> 
  summarise(gp_total = sum(score), .by = hrs_deprivation)

grand_total <- sum(df$score)

ss_between <- sum(group_totals$gp_total^2 / 3) - (grand_total^2 / 9)

ss_within <- sum(df$score^2) - sum(group_totals$gp_total^2 / 3)

ss_total <- sum(df$score^2) - (grand_total^2 / 9)

ss_total == ss_between + ss_within

mean_sq_between <- ss_between / (3 - 1)

mean_sq_within <- ss_within / (9 - 3)

f_ratio <- mean_sq_between / mean_sq_within

q_val <- qtukey(p = 0.05, nmeans = 3, df = 6, lower.tail = FALSE)

HSD <- q_val * sqrt(mean_sq_within / 3) 

fit <- aov(score ~ hrs_deprivation, df)
fit

TukeyHSD(x = fit)

fit |> 
  tukey_hsd()

#  Standardized effect size, Cohen's d
d_val <- (8 - 2) / sqrt(mean_sq_within)

# PROGRESS CHECK 16.7
df <- tibble(
  gp0 = c(1, 0, 0, 2, 3, 4, 2, 1),
  gp1 = c(2, 1, 2, 4, 4, 6, 3, 3),
  gp2 = c(4, 2, 3, 6, 7, 8, 5, 5),
  gp3 = c(7, 1, 6, 9, 10, 12, 8, 7)) |> 
  pivot_longer(cols = everything(), names_to = "nb_sessions",
               values_to = "eye_contacts", names_prefix = "gp") |> 
  arrange(nb_sessions) |> 
  mutate(nb_sessions = fct(nb_sessions))

df |> 
  summarise(mean = mean(eye_contacts), .by = nb_sessions)

group_totals <- df |> 
  summarise(gp_total = sum(eye_contacts), .by = nb_sessions)

grand_total <- sum(df$eye_contacts)

ss_between <- sum(group_totals$gp_total^2 / 8) - (grand_total^2 / 32)

ss_within <- sum(df$eye_contacts^2) - sum(group_totals$gp_total^2 / 8)

ss_total <- sum(df$eye_contacts^2) - (grand_total^2 / 32)

ss_total == ss_between + ss_within

mean_sq_between <- ss_between / (4 - 1)

mean_sq_within <- ss_within / (32 - 4)

f_ratio <- mean_sq_between / mean_sq_within

f_crit <- qf(df1 = 3, df2 = 28, p = 0.05, lower.tail = FALSE)

fit <- aov(eye_contacts ~ nb_sessions, data = df)
fit

q_val <- qtukey(p = 0.05, nmeans = 4, df = 24, lower.tail = FALSE)

HSD <- q_val * sqrt(mean_sq_within / 8) 

TukeyHSD(x = fit)

d_val_2_0 <- 3.375 / sqrt(mean_sq_within)
d_val_3_0 <- 5.875 / sqrt(mean_sq_within)
d_val_3_1 <- 4.375 / sqrt(mean_sq_within)

# REVIEW QUESTTION 16.9
df <- tibble(
  hr0 = c(3, 5, 7),
  hr24 = c(4, 8, 6),
  hr48 = c(2, 4, 6)) |> 
  pivot_longer(cols = everything(), names_to = "hrs_deprivation",
               values_to = "score", names_prefix = "hr") |> 
  arrange(hrs_deprivation) |> 
  mutate(hrs_deprivation = fct(hrs_deprivation))

fit <- aov(data = df, formula = score ~ hrs_deprivation)

anova_summary(fit)

# REVIEW QUESTTION 16.10
df <- tibble(
  group = c(rep(0, 5), rep(24, 3), rep(48, 4)),
  score = c(1, 3, 6, 2, 1, 4, 7, 5, 7, 12, 10, 9)
  )

group_totals <- df |> 
  summarise(gp_total = sum(score), .by = group)

grand_total <- sum(df$score)

ss_between <- (13^2 / 5) + (16^2 / 3) + (38^2 / 4) - (grand_total^2 / 12)

ss_within <- sum(df$score^2) - ((13^2 / 5) + (16^2 / 3) + (38^2 / 4))

ss_total <- sum(df$score^2) - (grand_total^2 / 12)

ss_total == ss_between + ss_within

mean_sq_between <- ss_between / (3 - 1)

mean_sq_within <- ss_within / (12 - 3)

f_ratio <- mean_sq_between / mean_sq_within

f_crit <- qf(df1 = 2, df2 = 9, p = 0.05, lower.tail = FALSE)

effect_size <- ss_between / ss_total

q_val <- qtukey(p = 0.05, nmeans = 3, df = 9, lower.tail = FALSE)

HSD <- q_val * sqrt(mean_sq_within / 4) 

2.6 - 5.33 > HSD
abs(2.6 - 9.5) > HSD
9.5 - 5.33 > HSD

d_val_48_0 <- (9.5 - 2.6) / sqrt(mean_sq_within)
d_val_48_24 <- (9.5 - 5.33) / sqrt(mean_sq_within)

# REVIEW QUESTTION 16.11
df <- tibble(
  group = c(rep(0, 5), rep(1, 5), rep(2, 5), rep(4, 5), rep(6, 5)),
  score = c(1, 1, 3, 6, 4, 4, 3, 1, 7, 5, 6, 1, 2, 10, 7,
            15, 6, 9, 17, 9, 20, 25, 10, 10, 9)) |> 
  mutate(group = fct(group))

group_totals <- df |> 
  summarise(gp_total = sum(score), .by = group)

grand_total <- sum(df$score)

ss_between <- (15^2 / 5) + (20^2 / 5) + (26^2 / 5) + (56^2 / 5) + (74^2 / 5) - (grand_total^2 / 25)

ss_within <- sum(df$score^2) - ((15^2 / 5) + (20^2 / 5) + (26^2 / 5) + (56^2 / 5) + (74^2 / 5))

ss_total <- sum(df$score^2) - (grand_total^2 / 25)

ss_total == ss_between + ss_within

mean_sq_between <- ss_between / (5 - 1)

mean_sq_within <- ss_within / (25 - 5)

f_ratio <- mean_sq_between / mean_sq_within

f_crit <- qf(df1 = 4, df2 = 20, p = 0.05, lower.tail = FALSE)

effect_size <- ss_between / ss_total

q_val <- qtukey(p = 0.05, nmeans = 5, df = 24, lower.tail = FALSE)

HSD <- q_val * sqrt(mean_sq_within / 5) 

4 - 3 > HSD
5.2 - 3 > HSD
11.2 - 3 > HSD
14.8 - 3 > HSD  # T

5.2 - 4 > HSD
11.2 - 4 > HSD
14.8 - 4 > HSD  # T

11.2 - 5.2 > HSD
14.8 - 5.2 > HSD  # T

14.8 - 11.2 > HSD

d_val_6_0 <- (14.8 - 3) / sqrt(mean_sq_within)
d_val_6_1 <- (14.8 - 4) / sqrt(mean_sq_within)
d_val_6_2 <- (14.8 - 5.2) / sqrt(mean_sq_within)

fit <- aov(score ~ group, df)

summary(fit)

TukeyHSD(df)

# REVIEW QUESTION 16.13
df <- tibble(
  group = c(rep("gp1", 8), rep("gp2", 6), rep("gp3", 9)),
  score = c(3, 4, 0, -3, 5, 10, 3, 0,
            -1, 8, 4, 2, 2, -3,
            7, 1, 10, 0, 18, 12, 4, 6, 5)) |> 
  mutate(group = fct(group))

group_totals <- df |> 
  summarise(gp_total = sum(score), .by = group)

grand_total <- sum(df$score)

ss_between <- (22^2 / 8) + (12^2 / 6) + (63^2 / 9) - (grand_total^2 / 23)

ss_within <- sum(df$score^2) - ((22^2 / 8) + (12^2 / 6) + (63^2 / 9))

ss_total <- sum(df$score^2) - (grand_total^2 / 23)

ss_total == ss_between + ss_within

mean_sq_between <- ss_between / (3 - 1)

mean_sq_within <- ss_within / (23 - 3)

f_ratio <- mean_sq_between / mean_sq_within

f_crit <- qf(df1 = 2, df2 = 20, p = 0.05, lower.tail = FALSE)

effect_size <- ss_between / ss_total

q_val <- qtukey(p = 0.05, nmeans = 3, df = 20, lower.tail = FALSE)

HSD <- q_val * sqrt(mean_sq_within / 20) 

df |> 
  summarise(mean_group = mean(score), .by = group)

2.75 - 2 > HSD
7 - 2.75 > HSD  # T
7 - 2 > HSD  # T


d_val_3_1 <- (7 - 2.75) / sqrt(mean_sq_within)
d_val_3_2 <- (7 - 2) / sqrt(mean_sq_within)

fit <- aov(score ~ group, df)

summary(fit)

TukeyHSD(df)

# ANALYSIS OF VARIANCE (REPEATED MEASURES) --------------------------------

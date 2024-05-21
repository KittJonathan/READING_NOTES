# COMMON STATISTICAL TESTS ARE LINEAR MODELS
# A WORK THROUGH
# https://steverxd.github.io/Stat_tests/
# 2024-05-21

# PACKAGES ----------------------------------------------------------------

library(tidyverse)

# THE DATA ----------------------------------------------------------------

# https://steverxd.github.io/Stat_tests/data.html

# SAMPLE VALUES

rnorm_fixed <- function(N, mu = 0, sd = 1) {
  
  scale(rnorm(N)) * sd + mu
  
  }

set.seed(40)

y <- rnorm_fixed(50, mu = 0.3, sd = 2)
x <- rnorm_fixed(50, mu = 0, sd = 1)
y2 <- rnorm_fixed(50, mu = 0.5, sd = 1.5)

mydata_wide <- tibble(x = x, y = y, y2 = y2)

mydata_long <- mydata_wide |> 
  gather(group, value, x:y2)

ggplot(mydata_long,
       aes(x = group, y = value, col = group)) +
  geom_jitter(width = 0.1)

# 'SIGNED RANK' VALUES

signed_rank <- function(x) {
  
  sign(x) * rank(abs(x))
  
  }

# THE LINEAR MODEL --------------------------------------------------------

# https://steverxd.github.io/Stat_tests/linearmodel.html

lm(y ~ 1 + x, data = mydata_wide) |> 
  summary() |> 
  print(digits = 5)

# CORRELATIONS ------------------------------------------------------------

# https://steverxd.github.io/Stat_tests/correlations.html

# PEARSON CORRELATION

cor.test(mydata_wide$y, mydata_wide$x, method = "pearson")

lm(mydata_wide$y ~ 1 + mydata_wide$x) |> 
  summary() |> 
  print(digits = 5)

# SPEARMAN CORRELATION

cor.test(mydata_wide$y, mydata_wide$x, method = "spearman")

cor.test(rank(mydata_wide$y), rank(mydata_wide$x), method = "pearson")

lm(rank(mydata_wide$y) ~ 1 + rank(mydata_wide$x)) |> 
  summary() |> 
  print(digits = 5)

# ONE MEAN ----------------------------------------------------------------

# https://steverxd.github.io/Stat_tests/one-mean.html

t.test(mydata_wide$y, mu = 0, alternative = "two.sided")

lm_fit <- lm(mydata_wide$y ~ 1)

lm_fit |> 
  summary() |> 
  print(digits = 5)
confint(lm_fit)

# Testing a null hypothesis other than zero
t.test(mydata_wide$y, mu = 0.1, alternative = "two.sided")
lm((mydata_wide$y - 0.1) ~ 1) |> 
  summary() |> 
  print(digits = 5)

# Wilcoxon signed-rank
wilcox.test(mydata_wide$y)
lm(signed_rank(mydata_wide$y) ~ 1) |> 
  summary() |> 
  print(digits = 8)

# PAIRED SAMPLES
t.test(mydata_wide$y2, mydata_wide$y, paired = TRUE, mu = 0, alternative = "two.sided")
lm2 <- lm((mydata_wide$y2 - mydata_wide$y) ~ 1)
lm2 |> 
  summary() |> 
  print(digits = 8)
confint(lm2)

# Wilcoxon matched pairs
wilcox.test(mydata_wide$y2, mydata_wide$y, paired = TRUE, mu = 0, alternative = "two.sided")
lm3 <- lm(signed_rank(mydata_wide$y2 - mydata_wide$y) ~ 1)
lm3 |> summary() |> print(digits = 8)
t.test(signed_rank(mydata_wide$y2 - mydata_wide$y), mu = 0, alternative = "two.sided")

# TWO MEANS (INDEPENDENT SAMPLES) -----------------------------------------

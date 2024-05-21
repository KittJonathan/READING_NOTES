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

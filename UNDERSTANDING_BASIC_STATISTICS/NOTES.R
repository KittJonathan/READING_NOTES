# UNDERSTANDING BASIC STATISTICS
# BRASE & BRASE
# 2024-07-24

# PACKAGES ----------------------------------------------------------------

library(plyr)
library(tidyverse)
library(DescTools)

# HELPER FUNCTIONS --------------------------------------------------------

scan_freq <- function(x, nb_classes) {
  
  class_width <- plyr::round_any(
    x = (max(x) - min(x)) / nb_classes,
    accuracy = 1,
    f = ceiling
  )
  
  print(paste0("For ", nb_classes, " classes, the class width is ", class_width))
  
  class_limits <- tibble(
    lower_cl = seq(from = min(x), by = class_width,
                   length.out = nb_classes),
    upper_cl = seq(from = (min(x) + class_width - 1), by = class_width,
                   length.out = nb_classes)
  )
  
  counts <- list()
  
  for (i in 1:nrow(class_limits)) {
    
    counts[[i]] <- x |>
      as_tibble() |> 
      filter(between(value, class_limits$lower_cl[i],
                     class_limits$upper_cl[i])) |> 
      nrow()
  }
  
  x_freq <<- class_limits |> 
    mutate(frequency = unlist(counts),
           class_midpoint = (lower_cl + upper_cl) / 2,
           lower_cb = lower_cl - 0.5,
           upper_cb = upper_cl + 0.5) |> 
    mutate(relative_frequency = frequency / sum(frequency),
            cumulative_frequency = cumsum(frequency),
            relative_cumulative_frequency = cumulative_frequency / max(cumulative_frequency)) |> 
    select(lower_cl, upper_cl, lower_cb, upper_cb, class_midpoint,
           frequency, relative_frequency:relative_cumulative_frequency)
  
  return(x_freq)
  
}

linear_reg <- function(d) {
  
  d <- d |> 
    mutate(x_sq = x^2,
           y_sq = y^2,
           xy = x * y,
           y_sq = y^2)
  
  x_sum <- sum(d$x)
  y_sum <- sum(d$y)
  x_sq_sum <- sum(d$x_sq)
  y_sq_sum <- sum(d$y_sq)
  xy_sum <- sum(d$xy)
  n <- nrow(d)
  
  x_mean <- x_sum / n
  y_mean <- y_sum / n
  
  slope_b <- (n * xy_sum - x_sum * y_sum) / (n * x_sq_sum - x_sum^2)
  
  intercept_a <- y_mean - slope_b * x_mean
  
  coeff_cor <- (n * xy_sum - x_sum * y_sum) / ((sqrt(n * x_sq_sum - x_sum^2)) * sqrt(n * y_sq_sum - y_sum^2))
  coeff_det <- coeff_cor^2
  
  res <- tibble(
    x_sum = x_sum,
    y_sum = y_sum,
    x_sq_sum = x_sq_sum,
    y_sq_sum = y_sq_sum,
    xy_sum = xy_sum,
    x_mean = x_mean,
    y_mean = y_mean,
    slope_b = slope_b,
    intercept_a = intercept_a,
    coeff_cor = coeff_cor,
    coeff_det = coeff_det
  )
  
  return(res)
  
  # p1x <- min(d$x)
  # p1y <- slope_b * p1x + intercept_a
  # 
  # p2x <- x_mean
  # p2y <- slope_b * p2x + intercept_a
  # 
  # p3x <- max(d$x)
  # p3y <- slope_b * p3x + intercept_a
  # 
  # ggplot() +
  #   geom_point(data = d, aes(x, y)) +
  #   geom_segment(aes(x = p1x, xend = p2x,
  #                    y = p1y, yend = p2y)) +
  #   geom_segment(aes(x = p2x, xend = p3x,
  #                    y = p2y, yend = p3y))
  
  }

# 2. ORGANIZING DATA ------------------------------------------------------
## 2.1. FREQUENCY DISTRIBUTIONS, HISTOGRAMS, AND RELATED TOPICS -----------

miles <- c(13, 47, 10, 3, 16, 20, 17, 40, 4, 2,
           7, 25, 8, 21, 19, 15, 3, 17, 14, 6,
           12, 45, 1, 8, 4, 16, 11, 18, 23, 12,
           6, 2, 14, 13, 7, 15, 46, 12, 9, 18,
           34, 13, 41, 28, 36, 17, 24, 27, 29, 9,
           14, 26, 10, 24, 37, 31, 8, 16, 12, 16)

bin_nb <- 6

class_width <- plyr::round_any(
  x = (max(miles) - min(miles)) / bin_nb, 
  accuracy = 1,
  f = ceiling
  )

class_limits <- tibble(
  lower_cl = seq(from = 1, by = class_width, length.out = 6),
  upper_cl = seq(from = class_width, by = class_width, length.out = 6)) |> 
  mutate(ranges = paste(lower_cl, upper_cl, sep = "-"))

class_limits |>
  ggplot() +
  geom_histogram(aes(x = tibble(miles)), breaks = class_limits$lower_cl)

counts <- list()

for (i in 1:nrow(class_limits)) {
  
  counts[[i]] <- miles |>
    as_tibble() |> 
    filter(between(value, class_limits$lower_cl[i],
                   class_limits$upper_cl[i])) |> 
    nrow()
  }

miles_freq <- class_limits |> 
  mutate(frequency = unlist(counts),
         class_midpoint = (lower_cl + upper_cl) / 2,
         lower_cb = lower_cl - 0.5,
         upper_cb = upper_cl + 0.5) |> 
  select(lower_cl, upper_cl, lower_cb, upper_cb,
         frequency, class_midpoint)

miles_rel_freq <- miles_freq |> 
  mutate(relative_frequency = round(frequency / sum(frequency), 2))

miles_rel_freq |> 
  ggplot() +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = frequency),
            col = "black", fill = "white") +
  labs(x = "Miles", y = "Frequency")

miles_rel_freq |> 
  ggplot() +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = relative_frequency),
            col = "black", fill = "white") +
  labs(x = "Miles", y = "Relative frequency")

waiting <- c(1, 5, 5, 6, 7, 4, 8, 7, 6, 5,
             5, 6, 7, 6, 6, 5, 8, 9, 9, 10,
             7, 8, 11, 2, 4, 6, 5, 12, 13, 6,
             3, 7, 8, 8, 9, 9, 10, 9, 8, 9)

class_width <- plyr::round_any(
  x = (max(waiting) - min(waiting)) / 5, 
  accuracy = 1,
  f = ceiling
)

class_limits <- tibble(
  lower_cl = seq(from = 1, by = class_width, length.out = 5),
  upper_cl = seq(from = class_width, by = class_width, length.out = 5)
)

counts <- list()

for (i in 1:nrow(class_limits)) {
  
  counts[[i]] <- waiting |>
    as_tibble() |> 
    filter(between(value, class_limits$lower_cl[i],
                   class_limits$upper_cl[i])) |> 
    nrow()
  }

waiting_freq <- class_limits |> 
  mutate(frequency = unlist(counts),
         class_midpoint = (lower_cl + upper_cl) / 2,
         lower_cb = lower_cl - 0.5,
         upper_cb = upper_cl + 0.5) |> 
  select(lower_cl, upper_cl, lower_cb, upper_cb,
         frequency, class_midpoint)

waiting_freq |> 
  ggplot() +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = frequency),
            col = "black", fill = "white") +
  labs(x = "Miles", y = "Frequency")

waiting_rel_freq <- waiting_freq |> 
  mutate(relative_frequency = round(frequency / sum(frequency), 3))

waiting_rel_freq |> 
  ggplot() +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = relative_frequency),
            col = "black", fill = "white") +
  labs(x = "Miles", y = "Relative frequency")

temp_freq <- tibble(
  lower_cb = c(10.5, 20.5, 30.5, 40.5, 50.5),
  upper_cb = c(20.5, 30.5, 40.5, 50.5, 60.5),
  frequency = c(23, 43, 51, 27, 7))

temp_freq |> 
  mutate(cumulative_frequency = cumsum(frequency)) |> 
  mutate(x = upper_cb) |> 
  select(x, cumulative_frequency) |> 
  add_row(x = 10.5, cumulative_frequency = 0) |> 
  arrange(x) |> 
  ggplot(aes(x, cumulative_frequency)) +
  geom_point() +
  geom_line() +
  geom_text(aes(x, cumulative_frequency + 5, label = cumulative_frequency))

class_width <- plyr::round_any(
  x = (82 - 20) / 7, 
  accuracy = 1,
  f = ceiling
)

class_limits <- tibble(
  lower_cl = seq(from = 20, by = class_width, length.out = 7),
  upper_cl = seq(from = 28, by = class_width, length.out = 7)
)

class_limits



class_width <- plyr::round_any(
  x = (120 - 10) / 5, 
  accuracy = 1,
  f = ceiling
)

class_limits <- tibble(
  lower_cl = seq(from = 10, by = class_width, length.out = 5),
  upper_cl = seq(from = 31, by = class_width, length.out = 5)
)

class_limits

mpg <- tibble(
  lower_cb = seq(8.5, 36.5, 4),
  upper_cb = seq(12.5, 40.5, 4),
  mpg_global = c(6, 15, 7, 4, 3, 12, 6, 1))

mpg |> 
  ggplot() +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = mpg_global),
            col = "black", fill = "white") +
  geom_text(aes(x = lower_cb + 2, y = mpg_global + 0.5, label = mpg_global))

mpg <- mpg |> 
  mutate(mpg_city = c(6, 15, 5, 1, 0, 0, 0, 0))

mpg |> 
  ggplot() +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = mpg_city),
            col = "black", fill = "white") +
  geom_text(aes(x = lower_cb + 2, y = mpg_city + 0.5, label = mpg_city)) +
  scale_x_continuous(limits = c(8.5, 40.5))

mpg |> 
  ggplot() +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = mpg_global - mpg_city),
            col = "black", fill = "white") +
  geom_text(aes(x = lower_cb + 2, y = (mpg_global - mpg_city) + 0.5, label = mpg_global - mpg_city)) +
  scale_x_continuous(limits = c(8.5, 40.5))

salaries <- tibble(
  lower_cb = seq(53.5, 237.5, 46),
  upper_cb = seq(99.5, 283.5, 46),
  frequency = c(36, 0, 0, 0, 0)
)

salaries |>
  ggplot() +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = frequency),
            col = "black", fill = "white")

salaries <- tibble(
  lower_cb = seq(53.5, 89.5, 9),
  upper_cb = seq(62.5, 98.5, 9),
  frequency = c(7, 11, 5, 6, 6)
)

salaries |>
  ggplot() +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = frequency),
            col = "black", fill = "white")


racing <- c(261, 271, 236, 244, 279, 296, 284, 299, 288, 288, 247, 256,
            338, 360, 341, 333, 261, 266, 287, 296, 313, 311, 307, 307,
            299, 303, 277, 283, 301, 305, 288, 290, 288, 289, 297, 299,
            332, 330, 309, 328, 307, 328, 285, 291, 295, 298, 306, 315,
            310, 318, 318, 320, 333, 321, 323, 324, 327)

scan_freq(racing, 5)

ggplot(x_freq) +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = frequency),
            col = "black", fill = "white") +
  geom_text(aes(x = class_midpoint, y = frequency + 1,
                label = frequency)) +
  labs(title = "Frequency", x = "Class midpoint", y = "Frequency")

ggplot(x_freq) +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = relative_frequency),
            col = "black", fill = "white") +
  geom_text(aes(x = class_midpoint, y = relative_frequency + 0.01,
                label = round(relative_frequency, 2))) +
  labs(title = "Relative frequency", x = "Class midpoint", y = "Relative frequency")

min_lower_cb <- min(x_freq$lower_cb)

x_freq |> 
  mutate(x = upper_cb) |> 
  select(x, cumulative_frequency) |> 
  add_row(x = min_lower_cb, cumulative_frequency = 0) |> 
  arrange(x) |> 
  ggplot(aes(x, cumulative_frequency)) +
  geom_point() +
  geom_line() +
  geom_text(aes(x, cumulative_frequency + 5, label = cumulative_frequency))

glucose <- c(45, 66, 83, 71, 76, 64, 59, 59,
             76, 82, 80, 81, 85, 77, 82, 90,
             87, 72, 79, 69, 83, 71, 87, 69,
             81, 76, 96, 83, 67, 94, 101, 94,
             89, 94, 73, 99, 93, 85, 83, 80,
             78, 80, 85, 83, 84, 74, 81, 70,
             65, 89, 70, 80, 84, 77, 65, 46,
             80, 70, 75, 45, 101, 71, 109, 73,
             73, 80, 72, 81, 63, 74)

scan_freq(glucose, 6)

ggplot(x_freq) +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = frequency),
            col = "black", fill = "white") +
  geom_text(aes(x = class_midpoint, y = frequency + 1,
                label = frequency)) +
  labs(title = "Frequency", x = "Class midpoint", y = "Frequency")

ggplot(x_freq) +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = relative_frequency),
            col = "black", fill = "white") +
  geom_text(aes(x = class_midpoint, y = relative_frequency + 0.01,
                label = round(relative_frequency, 2))) +
  labs(title = "Relative frequency", x = "Class midpoint", y = "Relative frequency")

min_lower_cb <- min(x_freq$lower_cb)

x_freq |> 
  mutate(x = upper_cb) |> 
  select(x, cumulative_frequency) |> 
  add_row(x = min_lower_cb, cumulative_frequency = 0) |> 
  arrange(x) |> 
  ggplot(aes(x, cumulative_frequency)) +
  geom_point() +
  geom_line() +
  geom_text(aes(x, cumulative_frequency + 5, label = cumulative_frequency))

tumor <- c(19, 18, 17, 1, 21, 22, 54, 46, 25, 49,
           50, 1, 59, 39, 43, 39, 5, 9, 38, 18,
           14, 45, 54, 59, 46, 50, 29, 12, 19, 36,
           38, 40, 43, 41, 10, 50, 41, 25, 19, 39,
           27, 20)

scan_freq(tumor, 5)

ggplot(x_freq) +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = frequency),
            col = "black", fill = "white") +
  geom_text(aes(x = class_midpoint, y = frequency + 1,
                label = frequency)) +
  labs(title = "Frequency", x = "Class midpoint", y = "Frequency")

ggplot(x_freq) +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = relative_frequency),
            col = "black", fill = "white") +
  geom_text(aes(x = class_midpoint, y = relative_frequency + 0.01,
                label = round(relative_frequency, 2))) +
  labs(title = "Relative frequency", x = "Class midpoint", y = "Relative frequency")

min_lower_cb <- min(x_freq$lower_cb)

x_freq |> 
  mutate(x = upper_cb) |> 
  select(x, cumulative_frequency) |> 
  add_row(x = min_lower_cb, cumulative_frequency = 0) |> 
  arrange(x) |> 
  ggplot(aes(x, cumulative_frequency)) +
  geom_point() +
  geom_line() +
  geom_text(aes(x, cumulative_frequency + 5, label = cumulative_frequency))

depths <- c(85, 45, 75, 60, 90, 90, 115, 30, 55, 58,
            78, 120, 80, 65, 65, 140, 65, 50, 30, 125,
            75, 137, 80, 120, 15, 45, 70, 65, 50, 45,
            95, 70, 70, 28, 40, 125, 105, 75, 80, 70,
            90, 68, 73, 75, 55, 70, 95, 65, 200, 75,
            15, 90, 46, 33, 100, 65, 60, 55, 85, 50,
            10, 68, 99, 145, 45, 75, 45, 95, 85, 65,
            65, 52, 82)

scan_freq(depths, 7)

ggplot(x_freq) +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = frequency),
            col = "black", fill = "white") +
  geom_text(aes(x = class_midpoint, y = frequency + 1,
                label = frequency)) +
  labs(title = "Frequency", x = "Class midpoint", y = "Frequency")

ggplot(x_freq) +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = relative_frequency),
            col = "black", fill = "white") +
  geom_text(aes(x = class_midpoint, y = relative_frequency + 0.01,
                label = round(relative_frequency, 2))) +
  labs(title = "Relative frequency", x = "Class midpoint", y = "Relative frequency")

min_lower_cb <- min(x_freq$lower_cb)

x_freq |> 
  mutate(x = upper_cb) |> 
  select(x, cumulative_frequency) |> 
  add_row(x = min_lower_cb, cumulative_frequency = 0) |> 
  arrange(x) |> 
  ggplot(aes(x, cumulative_frequency)) +
  geom_point() +
  geom_line() +
  geom_text(aes(x, cumulative_frequency + 5, label = cumulative_frequency))

males <- c(31, 39, 53, 47, 40, 49, 53, 47,
           45, 26, 39, 79, 45, 50, 36, 49,
           45, 49, 43, 48, 54, 50, 43, 42,
           42, 35, 49, 45, 42, 58, 42, 55,
           45, 71, 50, 57, 49, 50, 45, 46,
           53, 48, 53, 37, 56, 63, 41, 41,
           51, 48)

scan_freq(males, 5)

ggplot(x_freq) +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = frequency),
            col = "black", fill = "white") +
  geom_text(aes(x = class_midpoint, y = frequency + 1,
                label = frequency)) +
  labs(title = "Frequency", x = "Class midpoint", y = "Frequency")

ggplot(x_freq) +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = relative_frequency),
            col = "black", fill = "white") +
  geom_text(aes(x = class_midpoint, y = relative_frequency + 0.01,
                label = round(relative_frequency, 2))) +
  labs(title = "Relative frequency", x = "Class midpoint", y = "Relative frequency")

min_lower_cb <- min(x_freq$lower_cb)

x_freq |> 
  mutate(x = upper_cb) |> 
  select(x, cumulative_frequency) |> 
  add_row(x = min_lower_cb, cumulative_frequency = 0) |> 
  arrange(x) |> 
  ggplot(aes(x, cumulative_frequency)) +
  geom_point() +
  geom_line() +
  geom_text(aes(x, cumulative_frequency + 5, label = cumulative_frequency))

words <- c(34, 21, 37, 31, 10, 24, 39, 10, 17, 18, 32,
           17, 3, 10, 6, 5, 6, 6, 13, 22, 25, 3,
           5, 2, 9, 3, 0, 4, 29, 26, 5, 5, 24,
           15, 3, 8, 16, 9, 10, 3, 12, 10, 10, 10,
           11, 12, 13, 1, 9, 43, 13, 14, 32, 24, 15)

scan_freq(words, 8)

ggplot(x_freq) +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = frequency),
            col = "black", fill = "white") +
  geom_text(aes(x = class_midpoint, y = frequency + 1,
                label = frequency)) +
  labs(title = "Frequency", x = "Class midpoint", y = "Frequency")

ggplot(x_freq) +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = relative_frequency),
            col = "black", fill = "white") +
  geom_text(aes(x = class_midpoint, y = relative_frequency + 0.01,
                label = round(relative_frequency, 2))) +
  labs(title = "Relative frequency", x = "Class midpoint", y = "Relative frequency")

min_lower_cb <- min(x_freq$lower_cb)

x_freq |> 
  mutate(x = upper_cb) |> 
  select(x, cumulative_frequency) |> 
  add_row(x = min_lower_cb, cumulative_frequency = 0) |> 
  arrange(x) |> 
  ggplot(aes(x, cumulative_frequency)) +
  geom_point() +
  geom_line() +
  geom_text(aes(x, cumulative_frequency + 5, label = cumulative_frequency))

tons <- c(2.71, 1.62, 2.60, 1.64, 2.20, 2.02, 1.67, 1.99, 2.34, 1.26, 1.31,
          1.80, 2.82, 2.15, 2.07, 1.62, 1.47, 2.19, 0.59, 1.48, 0.77, 2.04,
          1.32, 0.89, 1.35, 0.95, 0.94, 1.39, 1.19, 1.18, 0.45, 0.70)
scan_freq(tons*100, 6)

ggplot(x_freq) +
  geom_rect(aes(xmin = lower_cb / 100, xmax = upper_cb / 100,
                ymin = 0, ymax = frequency),
            col = "black", fill = "white") +
  geom_text(aes(x = class_midpoint / 100, y = frequency + 1,
                label = frequency)) +
  labs(title = "Frequency", x = "Class midpoint", y = "Frequency")

min_lower_cb <- min(x_freq$lower_cb)

x_freq |> 
  mutate(x = upper_cb / 100) |> 
  select(x, cumulative_frequency) |> 
  add_row(x = min_lower_cb / 100, cumulative_frequency = 0) |> 
  arrange(x) |> 
  ggplot(aes(x, cumulative_frequency)) +
  geom_point() +
  geom_line() +
  geom_text(aes(x, cumulative_frequency + 5, label = cumulative_frequency))

batting <- c(0.194, 0.258, 0.190, 0.291, 0.158, 0.295, 0.261, 0.250, 0.181,
             0.125, 0.107, 0.260, 0.309, 0.309, 0.276, 0.287, 0.317, 0.252,
             0.215, 0.250, 0.246, 0.260, 0.265, 0.182, 0.113, 0.200)

scan_freq(batting*1000, 5)

ggplot(x_freq) +
  geom_rect(aes(xmin = lower_cb / 1000, xmax = upper_cb / 1000,
                ymin = 0, ymax = frequency),
            col = "black", fill = "white") +
  geom_text(aes(x = class_midpoint / 1000, y = frequency + 1,
                label = frequency)) +
  labs(title = "Frequency", x = "Class midpoint", y = "Frequency")

min_lower_cb <- min(x_freq$lower_cb)

x_freq |> 
  mutate(x = upper_cb / 1000) |> 
  select(x, cumulative_frequency) |> 
  add_row(x = min_lower_cb / 1000, cumulative_frequency = 0) |> 
  arrange(x) |> 
  ggplot(aes(x, cumulative_frequency)) +
  geom_point() +
  geom_line() +
  geom_text(aes(x, cumulative_frequency + 5, label = cumulative_frequency))

racing |> 
  as_tibble() |> 
  ggplot() +
  geom_dotplot(aes(x = value))

tumor |> 
  as_tibble() |> 
  ggplot() +
  geom_dotplot(aes(x = value))

## 2.2 BAR GRAPHS, CIRCLE GRAPHS, AND TIME-SERIES GRAPHS ------------------

life_exp <- tibble(
  year = rep(c(1980, 1990, 2000, 2010), each = 2),
  sex = rep(c("M", "F"), times = 4),
  age = c(70, 77.4, 71.8, 78.8, 73, 79.7, 74.1, 80.6)
)

life_exp |> 
  ggplot(aes(x = year, y = age,
             fill = fct_inorder(sex))) +
  geom_bar(stat = "identity",
           position = "dodge")

causes <- tibble(
  cause = c(rep("Snooze", 15), rep("Car", 5), rep("Breakfast", 13),
            rep("Study", 20), rep("Dress", 8), rep("Talk", 9),
            rep("Other", 3))
)

causes |> 
  ggplot(aes(x = fct_infreq(cause))) +
  geom_bar(width = 0.5)

tibble(
  frequency = c("Frequently", "Occasionally", "Rarely", "Never"),
  number = c(894, 1161, 615, 372)) |> 
  ggplot(aes(x = "", y = number, fill = frequency)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0, direction = -1)

distances <- tibble(
  week = 1:20,
  distance = c(1.5, 1.4, 1.7, 1.6, 1.9, 2.0, 1.8, 2.0, 1.9, 2.0,
               2.1, 2.1, 2.3, 2.3, 2.2, 2.4, 2.5, 2.6, 2.4, 2.7))
  
distances |> 
  ggplot() +
  geom_line(aes(x = week, y = distance), col = "blue",
            linewidth = 2) +
  geom_point(aes(x = week, y = distance),
             shape = 21, col = "blue", fill = "red",
             size = 4, stroke = 2)

salaries <- tibble(
  degree = c("Ninth grade", "High school", "Associate", "Bachelor", "Master",
             "Doctoral"),
  salary = c(24.3, 41.4, 59.7, 82.7, 100.8, 121.6)
)

salaries |> 
  ggplot() +
  geom_col(aes(x = fct_inorder(degree), y = salary), position = "identity")

tibble(age = c("18-34", "18-34", "45-54", "45-54"),
       influence = c("Influential", "Not influential", "Influential", "Not influential"),
       percentage = c(45, 44, 25, 60)) |> 
  ggplot() +
  geom_col(aes(x = age, y = percentage, fill = influence),
           position = "dodge")

tibble(
  fish = c("flatfish", "Pacific cod", "sablefish", "walleye pollock", "rockfish"),
  thd_metric_tons = c(36.3, 68.6, 16, 71.2, 18.9)) |> 
  arrange(-thd_metric_tons) |> 
  ggplot() +
  geom_bar(aes(x = fct_inorder(fish), y = thd_metric_tons),
           stat = "identity")

tibble(
  river = c("Bann", "Blackwater", "Erne", "Shannon", "Barrow"),
  count = c(19, 8, 15, 33, 14)) |> 
  arrange(-count) |> 
  ggplot() +
  geom_bar(aes(x = fct_inorder(river), y = count),
           stat = "identity")

tibble(
  river = c("Bann", "Blackwater", "Erne", "Shannon", "Barrow"),
  count = c(19, 8, 15, 33, 14)) |> 
  arrange(-count) |> 
  ggplot(aes(x = "", y = count, fill = river)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0, direction = 1)

tibble(
  place = c("closet", "bed", "bathtub", "freezer"),
  count = c(68, 23, 6, 3)) |> 
  arrange(-count) |> 
  ggplot(aes(x = "", y = count, fill = place)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0, direction = 1)

tibble(
  object = c("teaching", "research", "growth", "community service",
             "college service", "consulting"),
  time = c(51, 16, 5, 11, 11, 6)) |> 
  arrange(-time) |> 
  ggplot(aes(x = "", y = time, fill = object)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0, direction = 1)

tibble(
  crime = c("murder", "rape", "robbery", "house burglary",
            "motor vehicle theft", "assault"),
  rate = c(2.6, 33.4, 93.3, 911.6, 550.7, 125.3)) |> 
  arrange(-rate) |> 
  ggplot() +
  geom_bar(aes(x = fct_inorder(crime), y = rate),
           stat = "identity")

tibble(
  habit = c("tail-gating", "signals", "cut off", "slow", "inconsiderate"),
  percentage = c(22, 19, 16, 11, 8)) |> 
  arrange(-percentage) |> 
  ggplot() +
  geom_bar(aes(x = fct_inorder(habit), y = percentage),
           stat = "identity") +
  geom_text(aes(x = habit, y = percentage + 2,
                label = percentage))

tibble(
  period = 1:15,
  elevation = c(3817, 3815, 3810, 3812, 3808, 3803, 3798, 3797, 3795, 3797, 
                3802, 3807, 3811, 3816, 3817)) |> 
  ggplot(aes(x = period, y = elevation)) +
  geom_line() +
  geom_point()

tibble(
  age = c(0.5, 1:14),
  height = c(26, 29, 33, 36, 39, 42, 45, 47, 50, 52, 54, 56, 58, 60, 62)) |> 
  ggplot(aes(x = age, y = height)) +
  geom_line() +
  geom_point()


## 2.3. STEM-AND-LEAF DISPLAYS --------------------------------------------

weights <- c(30, 27, 12, 42, 35, 47, 38, 36, 27, 35,
             22, 17, 29, 3, 21, 0, 38, 32, 41, 33,
             26, 45, 18, 43, 18, 32, 31, 32, 19, 21,
             33, 31, 28, 29, 51, 12, 32, 18, 21, 26)

stem(weights)

scores <- c(132, 118, 124, 109, 104, 101, 125, 83, 99,
            131, 98, 125, 97, 106, 112, 92, 120, 103,
            111, 117, 135, 143, 112, 112, 116, 106, 117,
            119, 110, 105, 128, 112, 126, 105, 102)

stem(scores)

years <- c(58, 52, 68, 86, 72, 66, 97, 89, 84, 91, 91,
           92, 66, 68, 87, 86, 73, 61, 70, 75, 72, 73,
           85, 84, 90, 57, 77, 76, 84, 93, 58, 47)

stem(years)

loss <- c(46, 37, 36, 42, 81, 20, 73, 59, 35, 50,
          87, 52, 24, 27, 38, 56, 39, 74, 56, 31,
          27, 91, 46, 9, 54, 52, 30, 33, 28, 35,
          35, 23, 90, 72, 85, 42, 59, 50, 49,
          48, 38, 60, 46, 87, 50, 89, 49, 67)

stem(loss)

days <- c(7, 5.7, 5.5, 7, 6, 6.8, 7.4, 6.8, 7.5, 7, 7.2, 9.4, 7.1, 6.6, 7.3, 8.4, 7.8,
          6.9, 6.7, 7.2, 6.8, 7, 7.3, 8.7, 7.2, 7.4, 10, 7.3, 11.1, 9.6, 6.4, 7, 9.9, 7.6,
          5.5, 5.3, 6.6, 6.7, 7.5, 6.9, 7.1, 10.3, 6.8, 6.2, 5.2, 7.6, 7, 5.6, 7.1, 7.3, 8.5)
stem(days)

hospitals <- c(119, 16, 61, 88, 440, 71, 35, 8, 11, 227, 162, 19, 41, 113, 209, 123, 133,
               107, 136, 38, 51, 101, 175, 148, 102, 133, 53, 117, 47, 90, 21, 27, 231, 96,
               37, 66, 193, 113, 236, 12, 68, 52, 122, 421, 42, 15, 98, 92, 59, 129, 27)
stem(hospitals)

stem(c(23, 23, 18, 19, 16, 17, 15, 22, 13, 10,
       18, 15, 16, 13, 9, 20, 14, 10, 9, 12), scale = 1)

stem(c(9, 8, 9, 10, 14, 7, 11, 8, 9, 8,
       11, 8, 9, 7, 9, 9, 10, 7, 9, 9))

stem(c(71, 65, 67, 73, 74, 73, 71, 71, 74, 73, 71,
       70, 75, 71, 72, 71, 75, 75, 71, 71, 74, 75,
       66, 75, 75, 75, 71, 72, 72, 73, 71, 67), scale = 4)

stem(c(69, 69, 73, 74, 72, 72, 70, 71, 71, 70, 72,
       73, 73, 72, 71, 71, 71, 69, 70, 71, 72, 73,
       74, 72, 71, 68, 69, 70, 69, 71, 73, 74), scale = 2)

cigarettes <- tibble(
  tar = c(14.1, 16, 29.8, 8, 4.1, 15, 8.8, 12.4, 16.6, 14.9, 13.7, 15.1, 7.8,
          11.4, 9, 1, 17, 12.8, 15.8, 4.5, 14.5, 7.3, 8.6, 15.2, 12),
  nicotine = c(0.86, 1.06, 2.03, 0.67, 0.4, 1.04, 0.76, 0.95, 1.12, 1.02, 1.01, 0.9, 0.57,
               0.78, 0.74, 0.13, 1.26, 1.08, 0.96, 0.42, 1.01, 0.61, 0.69, 1.02, 0.82),
  CO = c(13.6, 16.6, 23.5, 10.2, 5.4, 15, 9, 12.3, 16.3, 15.4, 13, 14.4, 10,
         10.2, 9.5, 1.5, 18.5, 12.6, 17.5, 4.9, 15.9, 8.5, 10.6, 13.9, 14.9)
)

stem(cigarettes$tar)
stem(cigarettes$CO)
stem(cigarettes$nicotine)

tibble(
  aspect = c("jargon", "deductions", "form", "numbers", "other"),
  percentage = c(43, 28, 10, 8, 10)) |> 
  arrange(-percentage) |> 
  ggplot(aes(x = "", y = percentage, fill = aspect)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0, direction = 1)

dui_age <- c(46, 16, 41, 26, 22, 33, 30, 22, 36, 34,
             63, 21, 26, 18, 27, 24, 31, 38, 26, 55,
             31, 47, 27, 43, 35, 22, 64, 40, 58, 20,
             49, 37, 53, 25, 29, 32, 23, 49, 39, 40,
             24, 56, 30, 51, 21, 45, 27, 34, 47, 35)

stem(dui_age)

scan_freq(dui_age, 7)



class_width <- plyr::round_any((max(dui_age) - min(dui_age)) / 7,
                               accuracy = 1, f = ceiling)

breaks <- seq(from = min(dui_age), by = class_width, length.out = class_width + 1)

dui_tbl <- dui_age |> 
  as_tibble() |> 
  mutate(bin = cut(value, breaks, include.lowest = T, right = F)) |>
  count(bin) |> 
  mutate(lower_cl = breaks[-8],
         upper_cl = lower_cl + 6,
         class_midpoint = (upper_cl + lower_cl) / 2,
         lower_cb = lower_cl - 0.5,
         upper_cb = upper_cl + 0.5) |> 
  select(lower_cl, class_midpoint, upper_cl, lower_cb, upper_cb, freq = n) |> 
  mutate(rel_freq = freq / sum(freq))

dui_tbl |> 
  ggplot() +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = freq),
            col = "black", fill = "white") 

dui_tbl |> 
  ggplot() +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = rel_freq),
            col = "black", fill = "white")

dui_tbl |> 
  mutate(cumul_freq = cumsum(freq)) |> 
  select(x = upper_cb, cumul_freq) |> 
  add_row(x = 15.5, cumul_freq = 0) |> 
  arrange(x) |> 
  ggplot(aes(x, cumul_freq)) +
  geom_point() +
  geom_line() +
  geom_text(aes(x, cumul_freq + 5, label = cumul_freq))


trees <- c(109, 99, 106, 102, 115, 120, 120, 117, 122, 142, 
           106, 111, 119, 109, 125, 108, 116, 105, 117, 123, 
           103, 114, 101, 99, 112, 120, 108, 91, 115, 109,
           114, 105, 99, 122, 106, 113, 114, 75, 96, 124,
           91, 102, 108, 110, 83, 90, 69, 117, 84, 142,
           122, 113, 105, 112, 117, 122, 129, 100, 138, 117)

stem(trees)

trees_tbl <- scan_freq(trees, 7)

trees_tbl |> 
  ggplot() +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = frequency),
            col = "black", fill = "white") 

trees_tbl |> 
  ggplot() +
  geom_rect(aes(xmin = lower_cb, xmax = upper_cb,
                ymin = 0, ymax = relative_frequency),
            col = "black", fill = "white")

trees_tbl |>
  select(x = upper_cb, cumulative_frequency) |> 
  add_row(x = 68.5, cumulative_frequency = 0) |> 
  arrange(x) |> 
  ggplot(aes(x, cumulative_frequency)) +
  geom_point() +
  geom_line() +
  geom_text(aes(x, cumulative_frequency + 5, label = cumulative_frequency))

tibble(
  case = c("contracts", "torts", "absestos", "other product", "other"),
  count = c(107, 191, 49, 38, 21)) |> 
  arrange(-count) |> 
  ggplot(aes(x = fct_inorder(case), y = count)) +
  geom_bar(stat = "identity")

tibble(
  case = c("contracts", "torts", "absestos", "other product", "other"),
  count = c(107, 191, 49, 38, 21)) |> 
  mutate(pct = 100 * count / sum(count)) |> 
  arrange(-pct) |> 
  ggplot() +
  geom_bar(aes(x = "", y = pct, fill = case),
           stat = "identity") +
  coord_polar("y")

# 3. AVERAGES AND VARIATION -----------------------------------------------

## 3.1. MEASURES OF CENTRAL TENDENCY: MODE, MEDIAN, AND MEAN --------------

hours <- c(17, 12, 14, 17, 13, 16, 18, 20, 13, 12,
           12, 17, 16, 15, 14, 12, 12, 13, 17, 14,
           15, 12, 15, 16, 12, 18, 20, 19, 12, 15,
           18, 14, 16, 17, 15, 19, 12, 13, 12, 15)

median(hours)
modeest::mfv(hours)

mean(c(58, 67, 60, 84, 93, 98, 100))

class <- c(14, 20, 20, 20, 20, 23, 25, 30, 30, 30,
           35, 35, 35, 40, 40, 42, 50, 50, 80, 80)
mean(class)
mean(class, trim = 1/20)
median(class)

scores <- tibble(
  exam = c("midterm", "final"),
  score = c(83, 95),
  weights = c(0.4, 0.6)
)

((83 * 0.4) + (95 * 0.6)) / (0.4 + 0.6)

weighted.mean(x = scores$score, w = scores$weights)

x <- c(8, 2, 7, 2, 6)
x <- c(10, 12, 20, 15, 20)
x <- c(8, 2, 7, 2, 6, 5)

mean(x)
median(x)
modeest::mfv(x)

mean(c(5, 2, 8, 10, 6, 6))
mean(c(5, -3, 8, 20, 6, 6))

median(c(5, 2, 8, 10, 6, 6))
median(c(5, -3, 8, 20, 6, 6))

modeest::mfv(c(5, 2, 8, 10, 6, 6))
modeest::mfv(c(5, -3, 8, 20, 6, 6))

modeest::mfv(c(5, 2, 2, 2, 8, 10, 6, 6))
modeest::mfv(c(5, -3, -3, -3, 8, 10, 6, 6))


mean(c(5, 2, 8, 10, 6, 6))
mean(c(5, -8, 8, 20, 6, 6))

median(c(5, 2, 8, 10, 6, 6))
median(c(5, -8, 8, 20, 6, 6))

modeest::mfv(c(5, 2, 8, 10, 6, 6))
modeest::mfv(c(5, -8, 8, 20, 6, 6))

modeest::mfv(c(5, 2, 2, 2, 8, 10, 6, 6))
modeest::mfv(c(5, -8, -8, -8, 8, 10, 6, 6))

median(c(1, 3, 3, 5))

mean(c(2, 3, 4, 5, 5))
median(c(2, 3, 4, 5, 5))
modeest::mfv(c(2, 3, 4, 5, 5))

mean_a <- 10
nb_a <- 5
sum_a <- mean_a * nb_a
(sum_a + 20) / (nb_a + 1)

mean_b <- 50
nb_b <- 10
sum_b <- mean_b * nb_b
(sum_b + 20) / (nb_b + 1)
mean_b

x <- c(2, 2, 3, 6, 10)
mean(x)
median(x)
modeest::mfv(x)

mean(c(x, 5))
median(c(x, 5))
modeest::mfv(c(x, 5))

mean(x * 5)
median(x * 5)
modeest::mfv(x * 5)

70 * 2.54
68 * 2.54
71 * 2.54

x1 <- sample(1:1000, 15)

median(x1)
mean(x1)

x2 <- sort(x1)
x2
max(x1)
x2[15] <- 1040

median(x2)
mean(x2)

x3 <- sort(x1)
x3[15] <- 520

median(x3)
mean(x3)

x4 <- sort(x1)
x4[15] <- 450

median(x4)
mean(x4)

temp <- c(146, 152, 168, 174, 180, 178, 179,
          180, 178, 178, 168, 165, 152, 144)
mean(temp)
median(temp)
modeest::mfv(temp)

wolf <- c(13, 10, 7, 5, 7, 7, 2, 4, 3,
          2, 3, 15, 4, 4, 2, 8, 7, 8)
mean(wolf)
median(wolf)
modeest::mfv(wolf)

upper_canyon <- c(2, 3, 1, 1, 3, 4, 6, 9, 3, 1, 3)
lower_canyon <- c(8, 1, 1, 0, 6, 7, 2, 14, 3, 0, 1, 13, 2, 1)

mean(upper_canyon)
mean(lower_canyon)

median(upper_canyon)
median(lower_canyon)

modeest::mfv(upper_canyon)
modeest::mfv(lower_canyon)

mean(lower_canyon, trim = 0.05)

age <- c(24, 23, 25, 23, 30, 29, 28, 26, 33, 29,
         24, 37, 25, 23, 22, 27, 28, 25, 31, 29,
         25, 22, 31, 29, 22, 28, 27, 26, 23, 21,
         25, 21, 25, 24, 22, 26, 25, 32, 26, 29)
mean(age)
median(age)
modeest::mfv(age)

maui <- c(89, 50, 68, 60, 375, 55, 500, 71, 40, 350,
          60, 50, 250, 45, 45, 125, 235, 65, 60, 130)

mean(maui)
median(maui)
modeest::mfv(maui)

mean(maui, trim = 0.05)

weighted.mean(x = c(10, 20, 30), w = c(5, 3, 2))
weighted.mean(x = c(10, 20, 30), w = c(2, 3, 5))

weighted.mean(x = c(92, 81, 93, 85), w = c(0.25, 0.225, 0.225, 0.3))

weighted.mean(x = c(9, 7, 6, 10), w = c(2, 3, 1, 4))

weighted.mean(x = c(64.1, 75.8, 23.9, 68.2),
              w = c(0.38, 0.47, 0.07, 0.08))

hmean <- 2 / (1/60 + 1/75)
DescTools::Hmean(x = c(60, 75)) == hmean

sqrt(1.1 * 1.12 * 1.148 * 1.038 * 1.06)

growth <- c(1.1, 1.12, 1.148, 1.038, 1.06)
exp(mean(log(growth)))
DescTools::Gmean(growth)

## 3.2. MEASURES OF VARIATION ----------------------------------------------

range(c(17, 22, 22, 22, 27))
diff(range(c(17, 22, 22, 22, 27)))

range(c(17, 19, 20, 27, 27))
diff(range(c(17, 19, 20, 27, 27)))

mean(c(17, 22, 22, 22, 27))
mean(c(17, 19, 20, 27, 27))

x <- c(2, 3, 3, 8, 10, 10)

tibble(x = x) |> 
  mutate(dist_to_mean = x - mean(x),
         dist_mean_sq = dist_to_mean^2) |> 
  summarise(sum_dist_mean_sq = sum(dist_mean_sq)) |> 
  mutate(x_var = sum_dist_mean_sq / (length(x) - 1),
         x_sd = sqrt(x_var))

var(x)
sd(x)

x <- c(5, 5, 5, 6, 7, 8)

tibble(x = x) |> 
  mutate(x_sq = x^2,
         sum_x = sum(x),
         sum_x_sq = sum(x_sq),
         x_var = (sum_x_sq - (sum_x^2 / length(x))) / (length(x) - 1),
         x_sd = sqrt(x_var)) |> 
  slice(1) |> 
  select(sum_x:x_sd)

x <- c(2.1, 1.95, 2.6, 2, 1.85, 2.25, 2.15, 2.25)

mean(x)
sjstats::sd_pop(x)

sjstats::sd_pop(x) / mean(x) * 100
sjstats::cv(x)

x <- c(1.69, 1.49, 3.09, 1.79, 1.39, 2.89, 1.49, 1.39, 1.49, 1.99)
mean(x)
sd(x)
sd(x) / mean(x) * 100
sjstats::cv(x) * 100

# Chebyshev's theorem
1 - 1 / 2^2
1 - 1 / 3^2
1 - 1 / 4^2

525 - 3 * 30
525 + 3 * 30

x <- 2:6
diff(range(x))
sd(x)
sjstats::sd_pop(x)

x <- 1:5
diff(range(x))
sd(x)
sjstats::sd_pop(x)

x1 <- rnorm(n = 20, mean = 25, sd = 2)
x2 <- rnorm(n = 50, mean = 25, sd = 2)

sd(x1) - sjstats::sd_pop(x1)
sd(x2) - sjstats::sd_pop(x2)

sd(c(5, 9, 10, 11, 15))
sd(c(10, 14, 15, 16, 20))

sd(c(5, 9, 10, 11, 15))
sd(c(25, 45, 50, 55, 75))

3.1 * 1.6

70 + 2.5 * 5
70 + 2.5 * 3

x <- c(23, 17, 15, 30, 25)
diff(range(x))
sum(x)
sum(x^2)

(sum(x^2) - (sum(x)^2 / length(x))) / (length(x) - 1)
var(x)

sqrt((sum(x^2) - (sum(x)^2 / length(x))) / (length(x) - 1))
sd(x)

sum((x - mean(x))^2) / (length(x) - 1)
sqrt(sum((x - mean(x))^2) / (length(x) - 1))

sum((x - mean(x))^2) / length(x)
sqrt(sum((x - mean(x))^2) / length(x))

sjstats::var_pop(x)
sjstats::sd_pop(x)

3 / 15 * 100
15 - 2 * 3 ; 15 + 2 * 3

2 / 20 * 100
20 - 3 * 2 ; 20 + 3 * 2

x <- c(11, 0, 36, 21, 31, 23, 24, -11, -11, -21)
y <- c(10, -2, 29, 14, 22, 18, 14, -2, -3, -10)

x_sum <- sum(x)
x_sq_sum <- sum(x^2)
y_sum <- sum(y)
y_sq_sum <- sum(y^2)

x_mean <- x_sum / length(x)
x_var <- (x_sq_sum - (x_sum^2 / length(x))) / (length(x) - 1)
x_sd <- sqrt((x_sq_sum - (x_sum^2 / length(x))) / (length(x) - 1))

y_mean <- y_sum / length(y)
y_var <- (y_sq_sum - (y_sum^2 / length(y))) / (length(y) - 1)
y_sd <- sqrt((y_sq_sum - (y_sum^2 / length(y))) / (length(y) - 1))

x_mean - 2 * x_sd ; x_mean + 2 * x_sd
y_mean - 2 * y_sd ; y_mean + 2 * y_sd

x_sd / x_mean * 100
y_sd / y_mean * 100

x <- c(0.54, 1.80, 1.52, 2.05, 1.03, 1.18, 0.80, 1.33, 1.29, 1.11,
       3.34, 1.54, 0.08, 0.12, 0.60, 0.72, 0.92, 1.05, 1.43, 3.03,
       1.81, 2.17, 0.63, 0.56, 0.03, 0.09, 0.18, 0.34, 1.51, 1.45,
       1.52, 0.19, 1.55, 0.02, 0.07, 0.65, 0.40, 0.24, 1.51, 1.45,
       1.60, 1.80, 4.69, 0.08, 7.89, 1.58, 1.64, 0.03, 0.23, 0.72)

diff(range(x))
x_sum <- sum(x)
x_sq_sum <- sum(x^2)

x_mean <- x_sum / length(x)
x_var <- (x_sq_sum - (x_sum^2 / length(x))) / (length(x) - 1)
x_sd <- sqrt((x_sq_sum - (x_sum^2 / length(x))) / (length(x) - 1))

x_cv <- x_sd / x_mean * 100
x_cv

x <- c(13.20, 5.60, 19.80, 15.05, 21.40, 17.25, 27.45,
       16.95, 23.90, 32.40, 40.75, 5.10, 17.75, 28.35)

y <- c(11.85, 15.25, 21.30, 17.30, 27.50, 10.35, 14.90,
       48.70, 25.40, 25.95, 57.60, 34.35, 38.80, 41.00,
       31.25)

x_sum <- sum(x)
x_sq_sum <- sum(x^2)

y_sum <- sum(y)
y_sq_sum <- sum(y^2)

x_mean <- sum(x) / length(x)
x_var <- (x_sq_sum - (x_sum^2 / length(x))) / (length(x) - 1)
x_sd <- sqrt((x_sq_sum - (x_sum^2 / length(x))) / (length(x) - 1))

y_mean <- sum(y) / length(y)
y_var <- (y_sq_sum - (y_sum^2 / length(y))) / (length(y) - 1)
y_sd <- sqrt((y_sq_sum - (y_sum^2 / length(y))) / (length(y) - 1))

x_mean - 2 * x_sd ; x_mean + 2 * x_sd
y_mean - 2 * y_sd ; y_mean + 2 * y_sd

x_sd / x_mean * 100
y_sd / y_mean * 100

x <- c(56, 85, 52, 13, 39)
y <- c(24, 53, 60, 69, 18)

x_sum <- sum(x)
x_sq_sum <- sum(x^2)

y_sum <- sum(y)
y_sq_sum <- sum(y^2)

x_mean <- sum(x) / length(x)
x_var <- (x_sq_sum - (x_sum^2 / length(x))) / (length(x) - 1)
x_sd <- sqrt((x_sq_sum - (x_sum^2 / length(x))) / (length(x) - 1))

y_mean <- sum(y) / length(y)
y_var <- (y_sq_sum - (y_sum^2 / length(y))) / (length(y) - 1)
y_sd <- sqrt((y_sq_sum - (y_sum^2 / length(y))) / (length(y) - 1))

x_cv <- x_sd / x_mean * 100
y_cv <- y_sd / y_mean * 100

x_mean - 2 * x_sd ; x_mean + 2 * x_sd
y_mean - 2 * y_sd ; y_mean + 2 * y_sd

14.05 / 9.58 * 100
12.5 / 9.02 * 100

9.58 - 2 * 14.05 ; 9.58 + 2 * 14.05
9.02 - 2 * 12.5 ; 9.02 + 2 * 12.5

1.5 * 2.2 / 100

tbl <- tibble(
  x_midpoint = c(5.5, 15.5, 25.5, 35.5),
  count = c(34, 18, 17, 11)) |> 
  mutate(xf = x_midpoint * count,
         x_sq = x_midpoint^2,
         x_sq_f  = x_sq * count)

(sum(tbl$x_midpoint * tbl$count)) / (sum(tbl$count))
(sum(((tbl$x_midpoint - mean(tbl$x_midpoint))^2) * tbl$count)) / (sum(tbl$count) - 1)
sqrt((sum(((tbl$x_midpoint - mean(tbl$x_midpoint))^2) * tbl$count)) / (sum(tbl$count) - 1))

tbl <- tibble(
  x_midpoint = c(25.5, 35.5, 45.5),
  count = c(260, 348, 287)) |> 
  mutate(xf = x_midpoint * count,
         x_sq = x_midpoint^2,
         x_sq_f  = x_sq * count)

(sum(tbl$x_midpoint * tbl$count)) / (sum(tbl$count))
(sum(((tbl$x_midpoint - mean(tbl$x_midpoint))^2) * tbl$count)) / (sum(tbl$count) - 1)
sqrt((sum(((tbl$x_midpoint - mean(tbl$x_midpoint))^2) * tbl$count)) / (sum(tbl$count) - 1))

tbl <- tibble(
  x_midpoint = c(3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5),
  count = c(2, 2, 4, 22, 64, 90, 14, 2)) |> 
  mutate(xf = x_midpoint * count,
         x_sq = x_midpoint^2,
         x_sq_f  = x_sq * count)

(sum(tbl$x_midpoint * tbl$count)) / (sum(tbl$count))
(sum(((tbl$x_midpoint - mean(tbl$x_midpoint))^2) * tbl$count)) / (sum(tbl$count) - 1)
sqrt((sum(((tbl$x_midpoint - mean(tbl$x_midpoint))^2) * tbl$count)) / (sum(tbl$count) - 1))




tbl <- tibble(
  x_midpoint = c(10.55, 14.55, 18.55, 22.55, 26.55),
  count = c(15, 20, 5, 7, 3)) |> 
  mutate(xf = x_midpoint * count,
         x_sq = x_midpoint^2,
         x_sq_f  = x_sq * count)

(sum(tbl$x_midpoint * tbl$count)) / (sum(tbl$count))
(sum(((tbl$x_midpoint - mean(tbl$x_midpoint))^2) * tbl$count)) / (sum(tbl$count) - 1)
sqrt((sum(((tbl$x_midpoint - mean(tbl$x_midpoint))^2) * tbl$count)) / (sum(tbl$count) - 1))

tbl <- tibble(
  year = 1:11,
  return = c(1.78, 17.79, 7.46, 5.95, -4.74, 25.85, 9.03, 18.92, 17.49, 6.80, -2.38))

mean(tbl$return)
sd(tbl$return)

zoo::rollmean(x = tbl$return, k = 3)
mean(zoo::rollmean(x = tbl$return, k = 3))
sd(zoo::rollmean(x = tbl$return, k = 3))

N1 <- 310
s1 <- 3
N2 <- 420
s2 <- 12
N3 <- 516
s3 <- 6

n1 <- round(((N1 * s1) / (N1 * s1 + N2 * s2 + N3 * s3)) * 100)
n2 <- round(((N2 * s2) / (N1 * s1 + N2 * s2 + N3 * s3)) * 100)
n3 <- round(((N3 * s3) / (N1 * s1 + N2 * s2 + N3 * s3)) * 100)

(n1 * 82 / 100) + (n2 * 115 / 100) + (n3 * 90 / 100)



N1 <- 1525
s1 <- 2.2
N2 <- 917
s2 <- 1.4
N3 <- 2890
s3 <- 3.3

n1 <- round(((N1 * s1) / (N1 * s1 + N2 * s2 + N3 * s3)) * 250)
n2 <- round(((N2 * s2) / (N1 * s1 + N2 * s2 + N3 * s3)) * 250)
n3 <- round(((N3 * s3) / (N1 * s1 + N2 * s2 + N3 * s3)) * 250)

(n1 * 6.2 / 250) + (n2 * 3.1 / 250) + (n3 * 8.5 / 250)


N1 <- 183
N2 <- 371
N3 <- 255
m <- 150

n1 <- round((N1 / (N1 + N2 + N3)) * m)
n2 <- round((N2 / (N1 + N2 + N3)) * m)
n3 <- round((N3 / (N1 + N2 + N3)) * m)

n1 + n2 + n3

(n1 * 96 / m) + (n2 * 85 / m) + (n3 * 88 / m)

## 3.3. PERCENTILES AND BOX-AND-WHISKER PLOTS -----------------------------

# GUIDED EXERCISE 7

x <- c(342, 377, 319, 353, 295, 234, 294, 286, 377, 182,
       310, 439, 111, 201, 182, 197, 209, 147, 190, 151,
       131, 151)

x_sorted <- sort(x)

x_q50 <- (x_sorted[11] + x_sorted[12]) / 2
x_q25 <- x_sorted[6]
x_q75 <- x_sorted[17]
x_q75 - x_q25

boxplot.stats(x)$stats
quantile(x, type = 1)
IQR(x, type = 1)

boxplot(x)
boxplot(x)$stats

# EX.5
x <- c(2, 5, 5, 6, 7, 7, 8, 9, 10)
# Q3 = 7, Q1 = 5, Q2 = 8.5 
quantile(x, type = 6)
boxplot(x)

# EX.6
x <- c(2, 5, 5, 6, 7, 8, 8, 9, 10, 12)
# Q1 = 5, Q3 = 7.5, Q2 = 9 
quantile(x, type = 5)
boxplot(x)

# EX.7
x <- c(23, 2, 5, 14, 25, 36, 27, 42, 12, 8,
       7, 23, 29, 26, 28, 11, 20, 31, 8, 36)
x <- sort(x)
# Q3 = 29, Q1 = 8, Q = 9 
quantile(x, type = 5)
boxplot(x)
IQR(x)

# EX.8
x <- c(25, 22, 7, 24, 26, 31, 18, 14, 17, 20,
       31, 42, 6, 25, 22, 3, 29, 32, 15, 72)
boxplot(x)
IQR(x)
boxplot.stats(x)$stats

# EX.9
x <- c(17, 18, 18, 18, 19, 20, 20, 20, 21, 21,
       21, 21, 22, 22, 22, 22, 22, 22, 23, 23,
       24, 24, 24, 24, 24, 24, 24, 24, 25, 26,
       26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 
       28, 28, 29, 31, 31, 32, 32, 34, 35, 38)
boxplot(x)
IQR(x)
quantile(x)

# EX.10
x <- c(5, 6, 7, 7, 7, 7, 8, 8, 8, 8,
       8,  9,  9,  9,  9,  9,  9,  9,  10, 10,
       10, 10, 10, 10, 10, 10, 11, 11, 11, 11,
       11, 11, 11, 11, 12, 12, 12, 12, 13, 13,
       13, 13, 13, 13, 14, 14, 14, 14, 14, 15)
boxplot(x)
IQR(x)
quantile(x)

# EX.12

x <- c(65, 72, 68, 64, 60, 55, 73, 71, 52, 63, 61, 74,
       69, 67, 74, 50, 4, 75, 67, 62, 66, 80, 64, 65)
boxplot(x)
IQR(x)
quantile(x)
61.75 - 1.5 * IQR(x)
71.25 + 1.5 * IQR(x)
boxplot.stats(x)$out

# VIEWPOINT
tbl <- tibble(
  x_midpoint = c(76, 159.5, 262.5),
  count = c(67, 29, 4)) |> 
  mutate(xf = x_midpoint * count,
         x_sq = x_midpoint^2,
         x_sq_f  = x_sq * count)

(sum(tbl$x_midpoint * tbl$count)) / (sum(tbl$count))
(sum(((tbl$x_midpoint - mean(tbl$x_midpoint))^2) * tbl$count)) / (sum(tbl$count) - 1)
sqrt((sum(((tbl$x_midpoint - mean(tbl$x_midpoint))^2) * tbl$count)) / (sum(tbl$count) - 1))

# REVIEW 10
tbl <- tibble(
  x_midpoint = c(4:10),
  count = c(4, 3, 2, 5, 2, 3, 4)) |> 
  mutate(xf = x_midpoint * count,
         x_sq = x_midpoint^2,
         x_sq_f  = x_sq * count)

(sum(tbl$x_midpoint * tbl$count)) / (sum(tbl$count))
(sum(((tbl$x_midpoint - mean(tbl$x_midpoint))^2) * tbl$count)) / (sum(tbl$count) - 1)
sqrt((sum(((tbl$x_midpoint - mean(tbl$x_midpoint))^2) * tbl$count)) / (sum(tbl$count) - 1))

tbl <- tibble(
  x_midpoint = c(4:10),
  count = c(2, 3, 4, 5, 4, 3, 2)) |> 
  mutate(xf = x_midpoint * count,
         x_sq = x_midpoint^2,
         x_sq_f  = x_sq * count)

(sum(tbl$x_midpoint * tbl$count)) / (sum(tbl$count))
(sum(((tbl$x_midpoint - mean(tbl$x_midpoint))^2) * tbl$count)) / (sum(tbl$count) - 1)
sqrt((sum(((tbl$x_midpoint - mean(tbl$x_midpoint))^2) * tbl$count)) / (sum(tbl$count) - 1))

# REVIEW 12
x <- c(1.9, 2.8, 5.7, 4.2, 1.9, 8.6, 3.9, 7.2)
mean(x)
median(x)
modeest::mfv(x)
sd(x)
sd(x) / mean(x) * 100
diff(range(x))

# REVIEW 13
x <- c(31, 33, 34, 34, 35, 35, 35, 36, 38, 38, 38, 39, 40, 40, 40, 40,
       41, 41, 41, 41, 41, 41, 41, 42, 42, 43, 44, 44, 44, 45, 45, 46,
       46, 46, 46, 47, 48, 49, 49, 49, 49, 50, 51, 52, 52, 53, 53, 53,
       53, 53, 55, 56, 56, 57, 57, 59, 62, 66, 66, 68)

boxplot(x)
IQR(x)

scan_freq(x, 5)

tbl <- tibble(
  mid = c(34.5, 42.5, 50.5, 58.5, 66.5),
  count = c(11, 24, 15, 7, 3)) |> 
  mutate(xf = mid * count,
         x_sq = mid^2,
         x_sq_f  = x_sq * count)

x_mean <- (sum(tbl$mid * tbl$count)) / (sum(tbl$count))
x_var <- (sum(((tbl$mid - mean(tbl$mid))^2) * tbl$count)) / (sum(tbl$count) - 1)
x_sd <- sqrt(x_var)

x_mean - 2 * x_sd ; x_mean + 2 * x_sd

sum(tbl$x_sq_f)

# REVIEW 14
weighted.mean(x = c(92, 73, 81, 85, 87, 83, 90),
              w = c(5, 8, 8, 15, 15, 15, 34))
weighted.mean(x = c(20, 73, 81, 85, 87, 83, 90),
              w = c(5, 8, 8, 15, 15, 15, 34))

# REVIEW 15
2500 / 16

# REVIEW 16
x <- c(7.8,  9.1,  9.5,  10.0, 10.2, 10.5, 11.1, 11.5, 11.7, 11.8,
       12.2, 12.2, 12.5, 13.1, 13.5, 13.7, 13.7, 14.0, 14.4, 14.5,
       14.6, 15.2, 15.5, 16.0, 16.0, 16.1, 16.5, 17.2, 17.8, 18.2,
       19.0, 19.1, 19.3, 19.8, 20.0, 20.2, 20.3, 20.5, 20.9, 21.1,
       21.4, 21.8, 22.0, 22.0, 22.4, 22.5, 22.5, 22.8, 22.8, 23.1,
       23.1, 23.2, 23.7, 23.8, 23.8, 23.8, 23.8, 24.0, 24.1, 24.1,
       24.5, 24.5, 24.9, 25.1, 25.2, 25.5, 26.1, 26.4, 26.5, 26.7,
       27.1, 29.5)

summary(x)
quantile(x)

IQR(x)

boxplot(x)
hist(x)

# REVIEW 18
x <- c(15, 14, 14, 14, 13, 12, 11, 11, 11, 11, 10, 11, 13, 16, 10,
       9, 15, 12, 9, 10, 7, 14, 13, 14, 8, 9, 8, 11, 13, 13,
       15, 12, 9, 10, 9, 9, 16, 16, 12, 10, 11, 11, 12, 15, 6,
       10, 10, 10, 11, 9)
boxplot(x)
IQR(x)

tbl <- scan_freq(x, 4) |> 
  select(mid = class_midpoint, frequency) |> 
  mutate(xf = mid * frequency,
         x_sq = mid^2,
         x_sq_f = x_sq * frequency)

x_mean <- (sum(tbl$mid * tbl$frequency)) / (sum(tbl$frequency))
x_var <- (sum(((tbl$mid - mean(tbl$mid))^2) * tbl$frequency)) / (sum(tbl$frequency) - 1)
x_sd <- sqrt(x_var)

weighted.mean(x = c(5, 8, 7, 9, 7),
              w = c(2, 3, 3, 5, 3))

x <- c(rep(7.0, 8), rep(7.1, 10), rep(7.2, 10), rep(7.3, 11),
       rep(7.4, 9), rep(7.5, 8), rep(7.6, 9), rep(7.7, 6),
       rep(7.8, 5), rep(7.9, 5), 8.0, rep(8.1, 7), rep(8.2, 7),
       8.4, 8.5, 8.6, 8.7, 8.8, 8.8)

# CUMULATIVE REVIEW 7
stem(x)

# CUMULATIVE REVIEW 8
tbl <- scan_freq(x * 10, 5)

tbl |> 
  ggplot() +
  geom_rect(aes(xmin = lower_cb / 10, xmax = upper_cb / 10,
                ymin = 0, ymax = frequency),
            col = "black", fill = "white")

tbl |> 
  ggplot() +
  geom_rect(aes(xmin = lower_cb / 10, xmax = upper_cb / 10,
                ymin = 0, ymax = relative_frequency),
            col = "black", fill = "white")

# CUMULATIVE REVIEW 9
tbl |> 
  select(x = upper_cb, cumulative_frequency) |> 
  add_row(x = 69.5, cumulative_frequency = 0) |> 
  arrange(x) |> 
  ggplot(aes(x = x / 10, y = cumulative_frequency)) +
  geom_line() +
  geom_point()

# CUMULATIVE REVIEW 10
diff(range(x))
mean(x)
median(x)
modeest::mfv(x)

# CUMULATIVE REVIEW 11
sum(x)
sum(x^2)
var(x)
sd(x)
sd(x) / mean(x) * 100

# CUMULATIVE REVIEW 12
mean(x) - 2 * sd(x) ; mean(x) + 2 * sd(x)

# CUMULATIVE REVIEW 13
boxplot(x)
IQR(x)

# CUMULATIVE REVIEW 14
tbl |> 
  ggplot() +
  geom_rect(aes(xmin = lower_cb / 10, xmax = upper_cb / 10,
                ymin = 0, ymax = frequency),
            col = "black", fill = "white")

# CUMULATIVE REVIEW 15
tbl |> 
  select(x = upper_cb, cumulative_frequency) |> 
  add_row(x = 69.5, cumulative_frequency = 0) |> 
  arrange(x) |> 
  ggplot(aes(x = x / 10, y = cumulative_frequency)) +
  geom_line() +
  geom_point()

# CUMULATIVE REVIEW 16
stem(x)

# CUMULATIVE REVIEW 17
boxplot(x)

# 4. CORRELATION AND REGRESSION -------------------------------------------
## 4.1. SCATTER DIAGRAMS AND LINEAR CORRELATION ---------------------------


# GUIDED EXERCISE 1
tibble(x = c(10, 19, 30, 45, 50, 65, 80),
       y = c(80, 65, 68, 55, 35, 10, 12)) |> 
  ggplot(aes(x, y)) +
  geom_point()

# EXAMPLE 2

d <- tibble(
  wind = c(70, 115, 105, 82, 93, 125, 88),
  drift = c(3, 45, 21, 7, 16, 62, 12))

d |> 
  ggplot(aes(wind, drift)) +
  geom_point()

x_sum <- sum(d$wind)
y_sum <- sum(d$drift)
x_sq_sum <- sum(d$wind^2)
y_sq_sum <- sum(d$drift^2)
xy_sum <- sum(d$wind * d$drift)

nominator <- (nrow(d) * xy_sum) - (x_sum * y_sum) 
denominator_1 <- sqrt((nrow(d) * x_sq_sum) - (x_sum^2))
denominator_2 <- sqrt((nrow(d) * y_sq_sum) - (y_sum^2))
r <- nominator / (denominator_1 * denominator_2)

x_mean <- mean(d$wind)
x_sd <- sd(d$wind)
y_mean <- mean(d$drift)
y_sd <- sd(d$drift)

(1 / 6) * sum(((d$wind - x_mean) / x_sd) * ((d$drift - y_mean) / y_sd))

cor(d$wind, d$drift) 

# GUIDED EXERCISE 3

d <- tibble(
  officers = c(10, 15, 16, 1, 4, 6, 18, 12, 14, 7),
  muggings = c(5, 2, 1, 9, 7, 8, 1, 5, 3, 6))

d |> 
  ggplot(aes(x = officers, y = muggings)) +
  geom_point()

x_sum <- sum(d$officers)
y_sum <- sum(d$muggings)
x_sq_sum <- sum(d$officers^2)
y_sq_sum <- sum(d$muggings^2)
xy_sum <- sum(d$officers * d$muggings)

nominator <- (nrow(d) * xy_sum) - (x_sum * y_sum) 
denominator_1 <- sqrt((nrow(d) * x_sq_sum) - (x_sum^2))
denominator_2 <- sqrt((nrow(d) * y_sq_sum) - (y_sum^2))
r <- nominator / (denominator_1 * denominator_2)

cor(d$officers, d$muggings)

# PROBLEM 13
d <- tibble(
  age = c(3, 6, 12, 18, 24),
  weight = c(60, 95, 140, 170, 185))

d |> 
  ggplot(aes(x = age, y = weight)) +
  geom_point()

x_sum <- sum(d$age)
y_sum <- sum(d$weight)
x_sq_sum <- sum(d$age^2)
y_sq_sum <- sum(d$weight^2)
xy_sum <- sum(d$age * d$weight)

nominator <- (nrow(d) * xy_sum) - (x_sum * y_sum) 
denominator_1 <- sqrt((nrow(d) * x_sq_sum) - (x_sum^2))
denominator_2 <- sqrt((nrow(d) * y_sq_sum) - (y_sum^2))
r <- nominator / (denominator_1 * denominator_2)

cor(d$age, d$weight)

# PROBLEM 14
d <- tibble(
  x = c(3, 7, 15, 35, 75),
  y = c(40, 35, 30, 25, 18))

d |> 
  ggplot(aes(x, y)) +
  geom_point()

x_sum <- sum(d$x)
y_sum <- sum(d$y)
x_sq_sum <- sum(d$x^2)
y_sq_sum <- sum(d$y^2)
xy_sum <- sum(d$x * d$y)

nominator <- (nrow(d) * xy_sum) - (x_sum * y_sum) 
denominator_1 <- sqrt((nrow(d) * x_sq_sum) - (x_sum^2))
denominator_2 <- sqrt((nrow(d) * y_sq_sum) - (y_sum^2))
r <- nominator / (denominator_1 * denominator_2)

cor(d$x, d$y)

# PROBLEM 15
d <- tibble(
  x = c(1004, 975, 992, 935, 985, 932),
  y = c(40, 100, 65, 145, 80, 150))

d |> 
  ggplot(aes(x, y)) +
  geom_point()

x_sum <- sum(d$x)
y_sum <- sum(d$y)
x_sq_sum <- sum(d$x^2)
y_sq_sum <- sum(d$y^2)
xy_sum <- sum(d$x * d$y)

nominator <- (nrow(d) * xy_sum) - (x_sum * y_sum) 
denominator_1 <- sqrt((nrow(d) * x_sq_sum) - (x_sum^2))
denominator_2 <- sqrt((nrow(d) * y_sq_sum) - (y_sum^2))
r <- nominator / (denominator_1 * denominator_2)

cor(d$x, d$y)

# PROBLEM 16
d <- tibble(
  x = c(2.9, 4.2, 3.3, 4.5, 2.6, 3.2, 3.4),
  y = c(5, 10, 11.2, 10, 7.9, 3.9, 5.5))

d |> 
  ggplot(aes(x, y)) +
  geom_point()

x_sum <- sum(d$x)
y_sum <- sum(d$y)
x_sq_sum <- sum(d$x^2)
y_sq_sum <- sum(d$y^2)
xy_sum <- sum(d$x * d$y)

nominator <- (nrow(d) * xy_sum) - (x_sum * y_sum) 
denominator_1 <- sqrt((nrow(d) * x_sq_sum) - (x_sum^2))
denominator_2 <- sqrt((nrow(d) * y_sq_sum) - (y_sum^2))
r <- nominator / (denominator_1 * denominator_2)

cor(d$x, d$y)

# PROBLEM 17
d <- tibble(
  x = c(0.243, 0.259, 0.286, 0.263, 0.268, 0.339, 0.299),
  y = c(1.4, 3.6, 5.5, 3.8, 3.5, 7.3, 5.0))

d |> 
  ggplot(aes(x, y)) +
  geom_point()

x_sum <- sum(d$x)
y_sum <- sum(d$y)
x_sq_sum <- sum(d$x^2)
y_sq_sum <- sum(d$y^2)
xy_sum <- sum(d$x * d$y)

nominator <- (nrow(d) * xy_sum) - (x_sum * y_sum) 
denominator_1 <- sqrt((nrow(d) * x_sq_sum) - (x_sum^2))
denominator_2 <- sqrt((nrow(d) * y_sq_sum) - (y_sum^2))
r <- nominator / (denominator_1 * denominator_2)

cor(d$x, d$y)

# PROBLEM 18
d <- tibble(
  x = c(12.5, 30, 24.5, 14.3, 7.5, 27.7, 16.2, 20.1),
  y = c(26, 73, 39, 23, 15, 30, 15, 25))

d |> 
  ggplot(aes(x, y)) +
  geom_point()

x_sum <- sum(d$x)
y_sum <- sum(d$y)
x_sq_sum <- sum(d$x^2)
y_sq_sum <- sum(d$y^2)
xy_sum <- sum(d$x * d$y)

nominator <- (nrow(d) * xy_sum) - (x_sum * y_sum) 
denominator_1 <- sqrt((nrow(d) * x_sq_sum) - (x_sum^2))
denominator_2 <- sqrt((nrow(d) * y_sq_sum) - (y_sum^2))
r <- nominator / (denominator_1 * denominator_2)

cor(d$x, d$y)

# PROBLEM 19
d <- tibble(
  x = 1:6,
  y = c(1, 4, 6, 3, 6, 7))

d |> 
  ggplot(aes(x, y)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 8)) +
  scale_y_continuous(limits = c(0, 8))

d |> 
  ggplot(aes(x, y)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 8)) +
  scale_y_continuous(limits = c(0, 16))

d |> 
  ggplot(aes(x, y)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 16)) +
  scale_y_continuous(limits = c(0, 8))

# PROBLEM 20
cor(x = c(1, 3, 4), y = c(2, 1, 6))
cor(x = c(2, 1, 6), y = c(1, 3, 4))

# PROBLEM 23
d1 <- tibble(
  x = c(28, 5, 20, 35, 20, 23, 18, 5),
  y = c(48, 3, 34, 55, 34, 38, 28, 9))

d1 |> 
  ggplot(aes(x, y)) +
  geom_point()

x1_sum <- sum(d1$x)
y1_sum <- sum(d1$y)
x1_sq_sum <- sum(d1$x^2)
y1_sq_sum <- sum(d1$y^2)
x1y1_sum <- sum(d1$x * d1$y)

nominator <- (nrow(d1) * x1y1_sum) - (x1_sum * y1_sum) 
denominator_1 <- sqrt((nrow(d1) * x1_sq_sum) - (x1_sum^2))
denominator_2 <- sqrt((nrow(d1) * y1_sq_sum) - (y1_sum^2))
r1 <- nominator / (denominator_1 * denominator_2)

cor(d1$x, d1$y)

x <- c(20, 4, 18, 42, 15, 25, 2, 35)
y <- c(60, 8, 12, 50, 21, 30, 4, 70)

d2 <- tibble(
  x = c(20, 4, 18, 42, 15, 25, 2, 35),
  y = c(60, 8, 12, 50, 21, 30, 4, 70))

d2 |> 
  ggplot(aes(x, y)) +
  geom_point()

x2_sum <- sum(d2$x)
y2_sum <- sum(d2$y)
x2_sq_sum <- sum(d2$x^2)
y2_sq_sum <- sum(d2$y^2)
x2y2_sum <- sum(d2$x * d2$y)

nominator <- (nrow(d2) * x2y2_sum) - (x2_sum * y2_sum) 
denominator_1 <- sqrt((nrow(d2) * x2_sq_sum) - (x2_sum^2))
denominator_2 <- sqrt((nrow(d2) * y2_sq_sum) - (y2_sum^2))
r2 <- nominator / (denominator_1 * denominator_2)

cor(d2$x, d2$y)

mean(d1$x) ; mean(d2$x)
mean(d1$y) ; mean(d2$y)

sd(d1$x) ; sd(d2$x)
sd(d1$y) ; sd(d2$y)

## 4.2. LINEAR REGRESSION AND THE COEFFICIENT OF DETERMINATION ------------

# EXAMPLE 

d <- tibble(
  x = c(30, 34, 27, 25, 17, 23, 20),
  y = c(66, 79, 70, 60, 48, 55, 60))

d |> 
  ggplot(aes(x, y)) +
  geom_point()

x_mean <- mean(d$x)
y_mean <- mean(d$y)

x_sum <- sum(d$x)
y_sum <- sum(d$y)
xy_sum <- sum(d$x * d$y)
x_sq_sum <- sum(d$x^2)

b <- ((nrow(d) * xy_sum) - (x_sum * y_sum)) / ((nrow(d) * x_sq_sum) - (x_sum^2))

a <- y_mean - (b * x_mean)

p1 <- a + 17 * b
p2 <- a + 34 * b

ggplot() +
  geom_point(data = d, aes(x, y)) +
  geom_segment(aes(x = 17, y = p1, xend = 34, yend = p2))

d

lm(y ~ x, d)

# EXAMPLE 5
d <- tibble(
  x = c(30, 34, 27, 25, 17, 23, 20),
  y = c(66, 79, 70, 60, 48, 55, 60))

x_mean <- mean(d$x)
y_mean <- mean(d$y)

x_sum <- sum(d$x)
y_sum <- sum(d$y)
xy_sum <- sum(d$x * d$y)
x_sq_sum <- sum(d$x^2)

b <- ((nrow(d) * xy_sum) - (x_sum * y_sum)) / ((nrow(d) * x_sq_sum) - (x_sum^2))

a <- y_mean - (b * x_mean)

a + b * 21

# GUIDED EXERCISE 4

d <- tibble(
  x = c(6, 20, 0, 14, 25, 16, 28, 18, 10, 8),
  y = c(15, 31, 10, 16, 28, 20, 40, 25, 12, 15))

d |> 
  ggplot(aes(x, y)) +
  geom_point()

d <- d |> 
  mutate(x_sq = x^2,
         xy = x * y)

x_sum <- sum(d$x)
y_sum <- sum(d$y)
x_sq_sum <- sum(d$x_sq)
xy_sum <- sum(d$xy)
n <- nrow(d)

x_mean <- x_sum / n
y_mean <- y_sum / n

slope_b <- (n * xy_sum - x_sum * y_sum) / (n * x_sq_sum - x_sum^2)

intercept_a <- y_mean - slope_b * x_mean

lm(d$y ~ d$x)

intercept_a + slope_b * 12
cor(d$x, d$y)

# GUIDED EXERCISE 5

d <- tibble(
  x = c(6, 20, 0, 14, 25, 16, 28, 18, 10, 8),
  y = c(15, 31, 10, 16, 28, 20, 40, 25, 12, 15))

d <- d |> 
  mutate(x_sq = x^2,
         xy = x * y,
         y_sq = y^2)

x_sum <- sum(d$x)
y_sum <- sum(d$y)
x_sq_sum <- sum(d$x_sq)
y_sq_sum <- sum(d$y_sq)
xy_sum <- sum(d$xy)
n <- nrow(d)

coeff_cor <- (n * xy_sum - x_sum * y_sum) / ((sqrt(n * x_sq_sum - x_sum^2)) * sqrt(n * y_sq_sum - y_sum^2))
coeff_det <- coeff_cor^2

# PROBLEM 7
d <- tibble(x = c(16, 33, 50, 28, 50, 25),
            y = c(2, 3, 6, 5, 9, 3))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
0.161 * 40 - 0.748

# PROBLEM 8
d <- tibble(x = c(1, 3, 10, 16, 26, 36),
            y = c(42, 50, 75, 100, 150, 200))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
4.51 * 12 + 33.7

# PROBLEM 9
d <- tibble(x = c(27, 44, 32, 47, 23, 40, 34, 52),
            y = c(30, 19, 24, 13, 29, 17, 21, 14))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
-0.601 * 38 + 43.3

# PROBLEM 10
d <- tibble(x = c(0, 2, 5, 6),
            y = c(50, 45, 33, 26))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
-3.93 * 4 + 51.3

# PROBLEM 11
d <- tibble(x = c(17, 27, 37, 47, 57, 67, 77),
            y = c(36, 25, 20, 12, 10, 7, 5))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
-0.496 * 25 + 39.8

# PROBLEM 12
d <- tibble(x = c(37, 47, 57, 67, 77, 87),
            y = c(5, 8, 10, 16, 30, 43))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
0.749 * 70 -27.7

# PROBLEM 13
d <- tibble(x = c(8.6, 9.3, 10.1, 8.0, 8.3, 8.7),
            y = c(9.6, 18.5, 20.9, 10.2, 11.4, 13.1))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
5.76 * 10 -36.9

# PROBLEM 14
d <- tibble(x = c(6.1, 5.7, 3.9, 5.2, 6.2, 6.5, 11.1),
            y = c(-1.4, -4.1, -7.0, -4.0, 3.6, -0.1, -4.4))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)

# PROBLEM 15
d <- tibble(x = c(24.2, 19.0, 18.2, 14.9, 19.0, 17.5),
            y = c(13.0, 4.4, 9.3, 1.3, 0.8, 3.6))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
1.2 * 24 -17.2

# PROBLEM 16
d <- tibble(x = seq(10, 20, 2),
            y = c(1.8, 1.7, 1.5, 1.4, 1.0, 0.7))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
-0.110 * 15 + 3

# PROBLEM 17
d <- tibble(x = c(5.25, 5.75, 6.25, 6.75, 7.25),
            y = c(19, 13, 33, 37, 62))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
22 * 6.5 -105

# PROBLEM 18
d <- tibble(x = c(20, 16, 19.8, 18.4, 17.1, 15.5, 14.7, 17.1, 15.4, 16.2, 15, 17.2, 16, 17, 14.4),
            y = c(88.6, 71.6, 93.3, 84.3, 80.6, 75.2, 69.7, 82, 69.4, 83.3, 79.6, 82.6, 80.6, 83.5, 76.3))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
3.29 * 19 + 25.2

# PROBLEM 19
d <- tibble(
  x = c(6, 20, 0, 14, 25, 16, 28, 18, 10, 8),
  y = c(15, 31, 10, 16, 28, 20, 40, 25, 12, 15))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)

d |> 
  mutate(y_hat = 6.54 + 1.01 * x,
         residual = y - y_hat) |> 
  ggplot(aes(x, residual)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed")

fit <- lm(y ~ x, data = d)
fit$coefficients
fit$residuals

# PROBLEM 20
d <- tibble(x = c(27, 44, 32, 47, 23, 40, 34, 52),
            y = c(30, 19, 24, 13, 29, 17, 21, 14))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)

d |> 
  mutate(residual = y - (-0.601 * x + 43.3)) |> 
  ggplot(aes(x, residual)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed")

# PROBLEM 21
d <- tibble(x = c(1, 3, 4),
            y = c(2, 1, 6))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)

lm(y ~ x, d)$coeff

d <- tibble(x = c(2, 1, 6),
            y = c(1, 3, 4))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)

lm(y ~ x, d)$coeff

# PROBLEM 22
d <- tibble(x = 1:5,
            y = c(3, 12, 22, 51, 145))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)

d |> 
  mutate(log_y = log10(y)) |> 
  ggplot(aes(x, log_y)) +
  geom_point()

d <- d |> 
  mutate(y = log10(y))

ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)

alpha <- 10^0.154
beta <- 10^0.4

d <- tibble(x = 1:5,
            y = c(3, 12, 22, 51, 145)) |> 
  mutate(y_hat = alpha * (beta^x))
d

# PROBLEM 23
d <- tibble(x = c(2, 3, 5, 8, 10),
            y = c(2, 3, 12, 125, 630))

d |> ggplot(aes(x, y)) + geom_point()

d |> 
  mutate(y_prime = log10(y)) |> 
  ggplot(aes(x, y_prime)) +
  geom_point()

d2 <- d |> 
  mutate(y = log10(y))
linear_reg(d2)

lm(log10(y) ~ x, d)

alpha <- 10^-0.43
beta <- 10^0.32

d |> 
  mutate(y_hat = alpha * (beta^x))

# PROBLEM 24
d <- tibble(x = seq(2, 10, 2),
            y = c(1.81, 2.9, 3.2, 3.68, 4.11))
d |> ggplot(aes(x, y)) + geom_point()

d |> 
  mutate(x_prime = log10(x),
         y_prime = log10(y)) |> 
  ggplot(aes(x_prime, y_prime)) +
  geom_point()

d2 <- d |> 
  mutate(x = log10(x),
         y = log10(y))
linear_reg(d2)
lm(log10(y) ~ log10(x), d)

alpha <- 10^0.128
beta <- slope_b

# PROBLEM 25
d <- tibble(x = c(4, 5, 6, 8, 10),
            y = c(3.4, 4.2, 6.3, 10.9, 13.3))
d |> ggplot(aes(x, y)) + geom_point()

d |> 
  mutate(x_prime = log10(x),
         y_prime = log10(y)) |> 
  ggplot(aes(x_prime, y_prime)) +
  geom_point()

lm(log10(y) ~ log10(x), d)

# REVIEW PROBLEM 9
d <- tibble(x = 1:5,
            y = c(14, 18.9, 14.4, 19.6, 20))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
lm(y~x, d)$coeff

# REVIEW PROBLEM 10
d <- tibble(x = c(4, 7, 5, 6, 1, 5, 9, 10, 10, 3),
            y = c(33, 37, 34, 32, 32, 38, 43, 37, 40, 33))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
lm(y~x, d)$coeff
0.939 * 2 + 30.3 

# REVIEW PROBLEM 11
d <- tibble(x = c(21, 25, 23, 24, 20, 15, 25, 21, 17, 24, 26, 22, 18, 19),
            y = c(125, 125, 120, 125, 130, 120, 145, 130, 130, 130, 130, 140, 110, 115))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
lm(y~x, d)$coeff
1.28 * 20 + 99.3 

# REVIEW PROBLEM 12
d <- tibble(x = c(11, 19, 16, 13, 28, 5, 20, 14, 22, 7, 15, 29, 8, 25, 16),
            y = c(3, 11, 8, 5, 8, 2, 5, 6, 8, 3, 5, 10, 6, 10, 7))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
lm(y~x, d)$coeff
0.293 * 18 + 1.63 

# REVIEW PROBLEM 13
d <- tibble(x = c(11, 20, 16, 6, 12, 18, 23, 25),
            y = c(6, 10, 9, 5, 8, 14, 13, 16))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
lm(y~x, d)$coeff
0.554 * 15 + 1.05 

# REVIEW PROBLEM 14
d <- tibble(x = c(29, 2, 11, 17, 7, 6),
            y = c(173, 35, 132, 127, 69, 53))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
lm(y~x, d)$coeff
5.11 * 12 + 36.9 

# GROUP PROJECT
d <- tibble(x = c(1, 4, 5, 9, 10, 15),
            y = c(3, 7, 6, 10, 12, 4))
ggplot(data = d, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d)
lm(y~x, d)$coeff

d2 <- d |> 
  slice(1:5)
ggplot(data = d2, aes(x, y)) + geom_point() + geom_smooth(se = F, method = "lm")
linear_reg(d2)
lm(y~x, d)$coeff

# 5. ELEMENTARY PROBABILITY THEORY ----------------------------------------

## 5.1. WHAT IS PROBABILITY? ----------------------------------------------

# GUIDED EXERCISE 1
375/500
1/4

# GUIDED EXERCISE 2
1/8
3/8

# GUIDED EXERCISE 3
1
1 - 0.25

# PROBLEM 6
49/200
27/100
22/100

# PROBLEM 7
round(627/1010, 2)

# PROBLEM 8
1/7

# PROBLEM 11
0.5 * 0.5 * 0.5
1 - 0.125

# PROBLEM 12
0.5 * 0.5 * 0.5
1 - 0.125

# PROBLEM 17
15/375 + 71/375 + 124/375 + 131/375 + 34/375

# PROBLEM 18
1/6 + 1/6 + 1/6 + 1/6 + 1/6 + 1/6
4/6
2/6

# PROBLEM 19
290/966
135/966
319/966
222/966
290/966 + 135/966 + 319/966 + 222/966

# PROBLEM 20
2430/3000
1 - (2430/3000)

# PROBLEM 23
58/127
25/58
58/127 * 25/58
1 - (25/58)

## 5.2. SOME PROBABILITY RULES - COMPOUND EVENTS --------------------------

# GUIDED EXERCISE 4
0.72 * 0.92

# GUIDED EXERCISE 5
10/100
9/99
10/100 * 9/99

# EXAMPLE 6
15/31 + 8/31
14/31 + 8/31 - 5/31

# GUIDED EXERCISE 9

# P(I) & P(PW) 
17/140 ; 92/140

# P(I|PW)
8/92

# P(I AND PW)
8/140

# P(I AND PW)
92/140 * 8/92

# P(I OR PW)
17/140 + 92/140 - 8/140

# PROBLEM 3
0.3 + 0.4
0.3 + 0.4 - 0.1

# PROBLEM 4
0.7 + 0.4 - 0.2

# PROBLEM 5
0.2 * 0.4
0.4 * 0.1

# PROBLEM 6
0.7 * 0.8
0.7 * 0.9

# PROBLEM 7
0.5 * 0.2
0.2 + 0.5 - 0.1

# PROBLEM 8
0.2 * 0.2
0.2 + 0.5 - 0.04

# PROBLEM 10
# P(A) = 0.6
# P(B) = 0.7
# P(A|B) = 0.1
0.7 * 0.1
0.6 + 0.7 - 0.07

# PROBLEM 16
nb_trees <- 111 + 96 + 30 + 33 + 18
111 / nb_trees
(30 + 33 + 18) / nb_trees
(111 + 96 + 20) / nb_trees
(96 + 30 + 33) / nb_trees
18 / nb_trees

# PROBLEM 17
1/6 * 1/6
1/6 * 1/6
2/6 * 2/6

d <- tibble(
  d1 = rep(1:6, each = 6),
  d2 = rep(1:6, times = 6))

# PROBLEM 19
d |> 
  mutate(result = d1 + d2) |> 
  filter(result == 6)

d |> 
  mutate(result = d1 + d2) |> 
  filter(result == 4)

d |> 
  mutate(result = d1 + d2) |> 
  filter(result %in% c(4, 6))

# PROBLEM 20
d |> 
  mutate(result = d1 + d2) |> 
  filter(result == 7)

d |> 
  mutate(result = d1 + d2) |> 
  filter(result == 11)

d |> 
  mutate(result = d1 + d2) |> 
  filter(result %in% c(7, 11))

c <- tibble(
  suit = rep(c("hearts", "diamonds", "clubs", "spades"), each = 13),
  color = rep(c("red", "black"), each = 26),
  card = rep(c(2:10, "Jack", "Queen", "King", "Ace"), times = 4))

# PROBLEM 21
4/52 * 4/51

# PROBLEM 22
4/52 * 4/51

# PROBLEM 23
4/52 * 4/52

# PROBLEM 24
4/52 * 4/52

# PROBLEM 25
(27 + 14 + 22) / 100
(15 + 22 + 27 + 14) / 100
(27 + 14) / 100
(22 + 27) / 100

# PROBLEM 26
0.125 * 0.14
(1 - 0.125) * 0.24
0.95 * 0.14
(1 - 0.95) * 0.24
0.5 * 0.14
0.5 * 0.24

# PROBLEM 27
0.9 * 0.07
0.1 * 0.72
0.5 * 0.07
0.5 * 0.72
0.15 * 0.07
0.85 * 0.72

# PROBLEM 28
110 / 130
20 / 130
50 / 70
20 / 70
110 / 200
20 / 100

# PROBLEM 29
72 / 154
82 / 154
79 / 116
37 / 116
72 / 270
79 / 270

# PROBLEM 30
291 / 2008
77 / 452
(291 + 535) / 2008
(45 + 86) / 373
41 / 157
53 / 157
(452 - 32) / 452
(373 - 41) / 373

# PROBLEM 31
686 / 1160
270 / 580
416 / 580
270 / 1160
416 / 1160
474 / 1160
310 / 580
(580 / 1160) + (686 / 1160) - (270 / 1160)

## 5.3 TREES AND COUNTING TECHNIQUES --------------------------------------

# GUIDED EXERCISE 12
factorial(12) / factorial(9)
factorial(12) / (factorial(3) * factorial(9))
choose(n = 12, k = 3)
DescTools::CombN(n = 12, m = 3, ord = FALSE)
DescTools::CombN(n = 12, m = 3, ord = TRUE)

# PROBLEMS 13-20
DescTools::CombN(n = 5, m = 2, ord = TRUE)
DescTools::CombN(n = 5, m = 2, ord = FALSE)
DescTools::CombN(n = 8, m = 3, ord = TRUE)
DescTools::CombN(n = 8, m = 3, ord = FALSE)
DescTools::CombN(n = 7, m = 7, ord = TRUE)
DescTools::CombN(n = 7, m = 7, ord = FALSE)
DescTools::CombN(n = 9, m = 9, ord = TRUE)
DescTools::CombN(n = 8, m = 8, ord = FALSE)

# PROBLEM 21
DescTools::CombN(n = 15, m = 3, ord = TRUE)

# PROBLEM 22
DescTools::CombN(n = 10, m = 3, ord = TRUE)

# PROBLEM 23
DescTools::CombN(n = 5, m = 3, ord = TRUE)

# PROBLEM 24
DescTools::CombN(n = 10, m = 3, ord = FALSE)

# PROBLEM 25
DescTools::CombN(n = 15, m = 5, ord = FALSE)

# PROBLEM 26
DescTools::CombN(n = 12, m = 5, ord = FALSE)
DescTools::CombN(n = 7, m = 5, ord = FALSE)
21/792

# PROBLEM 27
DescTools::CombN(n = 12, m = 6, ord = FALSE)
DescTools::CombN(n = 7, m = 6, ord = FALSE)
7/924

# PROBLEM 28
DescTools::CombN(n = 69, m = 5, ord = FALSE)
DescTools::CombN(n = 69, m = 5, ord = FALSE) * 26

# CHAPTER REVIEW PROBLEM 9
0.7 * 0.8
0.7 + 0.8 - 0.56

# CHAPTER REVIEW PROBLEM 13
0.24
0.45
0.24 * 0.45

# CHAPTER REVIEW PROBLEM 14
0.2
0.59
0.2 * 0.59

# CHAPTER REVIEW PROBLEM 16
470 / 1000
390 / 1000
140 / 1000
420 / 500
20 / 500
50 / 500
120 / 500
0.420 * 0.5
0.06 * 0.5
(470 + 390) / 1000

# CHAPTER REVIEW PROBLEM 17
d <- tibble(
  d1 = rep(1:6, each = 6),
  d2 = rep(1:6, times = 6))

d |> 
  mutate(x = d1 + d2) |> 
  distinct(x)

d |> 
  mutate(x = d1 + d2) |> 
  count(x)

# CHAPTER REVIEW PROBLEM 18
0.77 * 0.9

# CHAPTER REVIEW PROBLEM 19
DescTools::CombN(n = 8, m = 2, ord = FALSE)

# CHAPTER REVIEW PROBLEM 20
DescTools::CombN(n = 7, m = 2, ord = TRUE)
DescTools::CombN(n = 7, m = 2, ord = FALSE)
DescTools::CombN(n = 3, m = 3, ord = TRUE)
DescTools::CombN(n = 4, m = 4, ord = FALSE)

# CHAPTER REVIEW PROBLEM 21
4^5
1 / 1024

# CHAPTER REVIEW PROBLEM 22
tibble(
  litt =  rep(1:4, each = 6),
  soc_sc = rep(c(1, 1, 2, 2, 3, 3), times = 4),
  phil = rep(1:2, times = 12)
)

# CHAPTER REVIEW PROBLEM 23
10^3

# CHAPTER REVIEW PROBLEM 24
DescTools::CombN(n = 3, m = 3, ord = TRUE)

# 6. THE BINOMIAL PROBABILITY DISTRIBUTION AND RELATED TOPICS -------------

## 6.1. INTRODUCTION TO RANDOM VARIABLES AND PROBABILITY DISTRIBUTIONS ----



# EXAMPLE 2
x <- 1:5
w <- c(0.27, 0.31, 0.18, 0.09, 0.15)

weighted.mean(x, w)

# PROBLEM 7
x <- c(0, 1, 2)
w <- c(0.25, 0.6, 0.15)

weighted.mean(x, w)
sqrt(sum(w * (x - 0.9)^2))

# PROBLEM 8
1 / 1000
500 / 1000

# PROBLEM 10
x <- seq(23, 67, 11)
w <- c(0.07, 0.44, 0.24, 0.14, 0.11)

weighted.mean(x, w)
sqrt(sum(w * (x - 42.58)^2))

# PROBLEM 11
x <- seq(10, 60, 10)
w <- c(0.21, 0.14, 0.22, 0.15, 0.20, 0.08)

weighted.mean(x, w)
sqrt(sum(w * (x - 32.3)^2))

# PROBLEM 12
x <- seq(24.5, 84.5, 10)
w <- c(0.057, 0.097, 0.195, 0.292, 0.25, 0.091, 0.018)

0.25 + 0.091 + 0.018

weighted.mean(x, w)
sqrt(sum(w * (x - 32.3)^2))

# PROBLEM 13
x <- 0:4
w <- c(0.44, 0.36, 0.15, 0.04, 0.01)

0.36 + 0.15 + 0.04 + 0.01
0.15 + 0.04 + 0.01

weighted.mean(x, w)
sqrt(sum(w * (x - 0.82)^2))

# PROBLEM 14
x <- 0:5
w <- c(0.237, 0.396, 0.264, 0.088, 0.015, 0.001)

1 - 0.237
0.264 + 0.088 + 0.015 + 0.001
0.015 + 0.001

weighted.mean(x, w)
sqrt(sum(w * (x - weighted.mean(x, w))^2))

# PROBLEM 15
15 / 719
704 / 719 
(15 / 719) * 35

# PROBLEM 16
6 / 2852
(2852 - 6) / 2852
(6 / 2852) * 2000
6 * 5

# PROBLEM 17
0.01191 * 50000

x <- 60:64
w <- c(0.01191, 0.01292, 0.01396, 0.01503, 0.01613)
sum(w * 50000)
sum(w * 50000) + 700
5000 - sum(w * 50000)

# PROBLEM 18
0.00756 * 50000

x <- 60:64
w <- c(0.00756, 0.00825, 0.00896, 0.00965, 0.01035)
sum(w * 50000)
sum(w * 50000) + 700
5000 - sum(w * 50000)

# PROBLEM 19
115 - 100
12^2 - 8^2
sqrt(80)

0.5 * 115 + 0.5 * 100
0.5^2 * 12^2 + 0.5^2 * 8^2
sqrt(52)

0.8 * 115 - 2
0.8^2 * 12^2
sqrt(92.16)

0.95 * 110 - 5
0.95^2 * 8^2
sqrt(57.76)

# PROBLEM 20
mu_1 <- 28.1
sd_1 <- 8.2
mu_2 <- 90.5
sd_2 <- 15.2

(mu_1 + mu_2)
(sd_1^2 + sd_2^2)
sqrt(sd_1^2 + sd_2^2)

1.5 * mu_1 + 2.75 * mu_2
1.5^2 * sd_1^2 + 2.75^2 * mu_2^2
sqrt(1.5^2 * sd_1^2 + 2.75^2 * mu_2^2)

50 + 1.5 * mu_1
50 + 1.5^2 * sd_1^2
sqrt(201.29)

# PROBLEM 21
0.5*50.2 + 0.5*50.2
0.5^2*11.5^2 + 0.5^2*11.5^2
sqrt(66.125)

## 6.2. BINOMIAL PROBABILITIES --------------------------------------------



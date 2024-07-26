# UNDERSTANDING BASIC STATISTICS
# BRASE & BRASE
# 2024-07-24

# PACKAGES ----------------------------------------------------------------

library(plyr)
library(tidyverse)

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

# 2. ORGANIZING DATA ------------------------------------------------------
## 2.1. FREQUENCY DISTRIBUTIONS, HISTOGRAMS, AND RELATED TOPICS -----------

miles <- c(13, 47, 10, 3, 16, 20, 17, 40, 4, 2,
           7, 25, 8, 21, 19, 15, 3, 17, 14, 6,
           12, 45, 1, 8, 4, 16, 11, 18, 23, 12,
           6, 2, 14, 13, 7, 15, 46, 12, 9, 18,
           34, 13, 41, 28, 36, 17, 24, 27, 29, 9,
           14, 26, 10, 24, 37, 31, 8, 16, 12, 16)

class_width <- plyr::round_any(
  x = (max(miles) - min(miles)) / 6, 
  accuracy = 1,
  f = ceiling
  )

class_limits <- tibble(
  lower_cl = seq(from = 1, by = class_width, length.out = 6),
  upper_cl = seq(from = class_width, by = class_width, length.out = 6)
)

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
  
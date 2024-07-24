# UNDERSTANDING BASIC STATISTICS
# BRASE & BRASE
# 2024-07-24

# PACKAGES ----------------------------------------------------------------

library(plyr)
library(tidyverse)

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

# TIDY MODELING WITH R
# INTRODUCTION
# 2. A TIDYVERSE PRIMER
# https://www.tmwr.org/tidyverse
# 2024-05-03

# 2.1 TIDYVERSE PRINCIPLES ------------------------------------------------

library(dplyr)
arrange(mtcars, gear, mpg)

boot_samp <- rsample::bootstraps(data = mtcars, times = 3)
boot_samp
class(boot_samp)

small_mtcars <- arrange(mtcars, gear)
small_mtcars <- slice(small_mtcars, 1:10)

small_mtcars <- slice(arrange(mtcars, gear), 1:10)

small_mtcars <- 
  mtcars |> 
  arrange(gear) |> 
  slice(1:10)

library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = lm)

n <- nrow(mtcars)
ratios <- rep(NA_real_, n)
for (car in 1:n) {
  ratios[car] <- log(mtcars$mpg[car]/mtcars$wt[car])
}
head(ratios)

ratios <- log(mtcars$mpg/mtcars$wt)

compute_log_ratio <- function(mpg, wt) {
  log_base <- getOption("log_base", default = exp(1))  # gets external data
  results <- log(mpg/wt, base = log_base)
  print(mean(results))  # prints to the console
  done <<- TRUE  # sets external data
  results
}

compute_log_ratio <- function(mpg, wt, log_base = exp(1)) {
  log(mpg/wt, base = log_base)
}

purrr::map(head(mtcars$mpg, 3), sqrt)

purrr::map_dbl(head(mtcars$mpg, 3), sqrt)

log_ratios <- purrr::map2_dbl(mtcars$mpg, mtcars$wt, compute_log_ratio)
head(log_ratios)

# 2.2 EXAMPLES OF TIDYVERSE SYNTAX ----------------------------------------

data.frame(`variable 1` = 1:2, two = 3:4)

df <- data.frame(`variable 1` = 1:2,
                 two = 3:4,
                 check.names = FALSE)
df

tbbl <- tibble::tibble(`variable 1` = 1:2,
               two = 3:4)
tbbl

df$tw
tbbl$tw

df[, "two"]
tbbl[, "two"]

url <- "https://data.cityofchicago.org/api/views/5neh-572f/rows.csv?accessType=DOWNLOAD&bom=true&format=true"

all_stations <- 
  # Step 1: Read in the data
  readr::read_csv(url) |> 
  # Step 2: filter columns and rename stationname
  dplyr::select(station = stationname, date, rides) |> 
  # Step 3: convert the character date field to a date encoding,
  # and put the data in units of 1K rides
  dplyr::mutate(date = lubridate::mdy(date),
                rides = rides / 1000) |> 
  # Step 4: summarize the multiple records using the maximum
  dplyr::group_by(station) |> 
  dplyr::summarise(rides = max(rides),
                   .groups = "drop")
# INTRODUCTION TO MODERN STATISTICS
# MINE CETINKAYA-RUNDEL & JOHANNA HARDIN
# https://openintro-ims.netlify.app/
# STARTED READING : 2024-10-03

# LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(openintro)

# INTRODUCTION TO DATA ----------------------------------------------------

## 1. HELLO DATA ----------------------------------------------------------

### 1.1 CASE STUDY: USING STENTS TO PREVENT STROKES -----------------------

stent30_count <- stent30 |> 
  mutate(time = "30 days", .before = group) |> 
  count(time, group, outcome)

stent365_count <- stent365 |> 
  mutate(time = "365 days", .before = group) |> 
  count(time, group, outcome)

stent30_count |> 
  bind_rows(stent365_count) |> 
  pivot_wider(id_cols = c(time, group),
              names_from = outcome,
              values_from = n)

45/224
28/(199+28)

### 1.2 DATA BASICS -------------------------------------------------------



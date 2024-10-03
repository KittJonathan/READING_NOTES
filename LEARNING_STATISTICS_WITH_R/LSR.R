# LEARNING STATISTICS WITH R
# A TUTORIAL FOR PSYCHOLOGY STUDENTS AND OTHER BEGINNERS
# VERSION 0.6
# DANIELLE NAVARRO
# https://learningstatisticswithr.com/lsr-0.6.pdf
# STARTED READING : 2024-10-03

# LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)

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

head(loan50)

loan50 |> 
  select(loan_amount, interest_rate, term, grade,
         state, total_income, homeownership) |> 
  head()

head(county)

county |> 
  select(unemployment_rate, pop2017, state, median_edu) |> 
  glimpse()

county |> 
  distinct(state)

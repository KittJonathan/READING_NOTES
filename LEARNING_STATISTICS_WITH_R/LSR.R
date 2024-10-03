# LEARNING STATISTICS WITH R
# A TUTORIAL FOR PSYCHOLOGY STUDENTS AND OTHER BEGINNERS
# VERSION 0.6
# DANIELLE NAVARRO
# https://learningstatisticswithr.com/lsr-0.6.pdf
# STARTED READING : 2024-10-03

# LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(lsr)

# WORKING WITH DATA -------------------------------------------------------

## 5 DESCRIPTIVE STATISTICS -----------------------------------------------

load("LEARNING_STATISTICS_WITH_R/DATA/aflsmall.Rdata")
who()

print(afl.margins)
hist(afl.margins)

### 5.1 MEASURES OF CENTRAL TENDENCY --------------------------------------

# MEAN

(56 + 31 + 56 + 8 + 32) / 5

sum(afl.margins)

sum(afl.margins[1:5])

sum(afl.margins[1:5]) / 5

mean(x = afl.margins)

mean(afl.margins[1:5])

# MEDIAN

sort(x = afl.margins)

(sort(x = afl.margins)[88] + sort(x = afl.margins)[89]) / 2

median(x = afl.margins)

# PRACTICAL STATISTICS IN MEDICINE WITH R
# KONSTANTINOS I. BOUGIOUKAS
# 2024-09-03
# https://practical-stats-med-r.netlify.app/

# 1. R VIA RSTUDIO --------------------------------------------------------

# https://practical-stats-med-r.netlify.app/intro_rstudio

## 1.1. INSTALLING R AND RSTUDIO ------------------------------------------

## 1.2. STARTING R & RSTUDIO ----------------------------------------------

14 + 16

## 1.3. ERRORS, WARNINGS, AND MESSAGES IN R -------------------------------

hello
# 14 + 16 - 

# 1.4. R HELP RESOURCES ---------------------------------------------------

help(median)
?median

help.search("geometric median")
??"geometric median"

apropos("med")

example(median)

# 2. RSTUDIO PROJECTS -----------------------------------------------------

## 2.1. WORKING WITH RSTUDIO PROJECTS -------------------------------------

### 2.1.1. CREATING AN RSTUDIO PROJECT ------------------------------------

### 2.1.2. ORGANIZING OUR RSTUDIO PROJECT FOLDER --------------------------

# data/
# figures/

## 2.2. OPENING A NEW R SCRIPT --------------------------------------------

## 2.3. ADDING COMMENTS TO A SCRIPT ---------------------------------------

14 + 16  # add two integers
# 45.3 + 20.5  # add two decimals

# 3. R AS CALCULATOR ------------------------------------------------------

## 3.1. ARITHMETIC OPERATIONS IN R ----------------------------------------

1 + 100
3 + 5 * 2
3 / 5 * 2

(3 + 5) * 2

(3 + (5 * (2 ^ 2)))  # hard to read
3 + 5 * 2 ^ 2        # clear, if we remember the rules
3 + 5 * (2 ^ 2)      # if we forget some rules, this may help

## 3.2. RELATIONAL OPERATORS IN R -----------------------------------------

2 < 1
1 > 0
1 == 1
1 <= 1
-9 >= -3
1 != 2

## 3.3. SCIENTIFIC NOTATION -----------------------------------------------

0.05
0.0005

## 3.4. SPECIAL VALUES IN R -----------------------------------------------

### 3.4.1. MISSING VALUES (NA) --------------------------------------------

1 + NA
(3 + 5) / NA

### 3.4.2. INF ------------------------------------------------------------

1 / 0
Inf + 1000

### 3.4.3. NOT A NUMBER (NAN) ---------------------------------------------

Inf / Inf
0 / 0
-Inf + Inf

### 3.4.4. NULL -----------------------------------------------------------

NULL

# 4. R FUNCTIONS ----------------------------------------------------------

## 4.1. CHARACTERISTICS OF R FUNCTIONS ------------------------------------

seq(from = 5, to = 8, by = 0.5)
seq(5, 8, 0.5)
seq(5, 8, length.out = 26)

seq(5, 8)

args(log)
log(base = 10)
log(15)
log(15, base = 10)

log(x = 3)
log(x = 3, exp(1))
log(x = 3, base = exp(1))
log(3, exp(1))
log(3, base = exp(1))
log(base = exp(1), 3)
log(base = exp(1), x = 3)

log(exp(1), 3)

date()

## 4.2. MATHEMATICAL FUNCTIONS --------------------------------------------

### 4.2.1. LOGARITHMIC AND EXPONENTIAL FUNCTIONS --------------------------

log(100)
log(0.05)
log2(100)
log10(100)

exp(5)
exp(0.5)

### 4.2.2. TRIGONOMETRIC FUNCTIONS (ANGLES IN RADIANS) --------------------

sin(pi/2)
cos(pi)
tan(pi/3)

### 4.2.3. OTHER MATHEMATICAL FUNCTIONS -----------------------------------

sqrt(9)
abs(-9)
sign(-9)
factorial(3)
choose(6, 2)

### 4.2.4. THE ROUND() FUNCTION -------------------------------------------

round(7 / 3)
round(7 / 3, digits = 2)

round(1.5)
round(2.5)
round(4.5)
round(5.5)

ceiling(16.2)
floor(16.2)
trunc(125.2395)
signif(2718214, 3)

## 4.3. THE SESSIONINFO() AND OPTION() FUNCTIONS --------------------------

sessionInfo()
help(options)

## 4.4. USER-DEFINED FUNCTIONS --------------------------------------------

log7 <- function(x) {
  log(x, base = 7)
  }

log7(5)

# 5. R PACKAGES -----------------------------------------------------------

## 5.1. WHAT ARE R PACKAGES? ----------------------------------------------

# Standard (base) R packages
# Add-on packages (CRAN, Github, Bioconductor)

## 5.2. PACKAGE INSTALLATION ----------------------------------------------

# install.packages("rstatix")
# install.packages(c("rstatix", "dplyr", "ggplot2"))

# 5.3. PACKAGE LOADING ----------------------------------------------------

library(rstatix)

rstatix::t_test

## 5.4. THE {TIDYVERSE} PACKAGE -------------------------------------------

library(tidyverse)

## 5.5. THE {HERE} PACKAGE ------------------------------------------------

# library(readxl)
# dat <- read_excel(here("data", "covid19.xlsx"))

# 6. R OBJECTS ------------------------------------------------------------



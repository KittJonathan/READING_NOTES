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

## 6.1. WHAT ARE THE OBJECTS IN R -----------------------------------------

class(iris); dim(iris)
attributes(iris)

## 6.2. NAMED STORAGE OF OBJECTS ------------------------------------------

x <- 1/40
x
(x <- 1/40)
log(x)

x = 1/40
x

1/40 -> x
x

## 6.3. REASSIGNING AN OBJECT ---------------------------------------------

x
x <- 100
x
x <- x + 1
x

## 6.4. LEGAL OBJECT NAMES ------------------------------------------------

??make.names
??clean_names

Y <- 50
Y
y

## 6.5. WE ARE NOT LIMITED TO STORE NUMBERS IN OBJECTS --------------------

sentence <- "the cat sat on the mat"
sentence
sentence + 1

# 7. ATOMIC VECTORS -------------------------------------------------------

## 7.1. INTRODUCTION TO VECTORS IN R --------------------------------------

## 7.2. ATOMIC VECTORS ----------------------------------------------------

### 7.2.1. ONE-ELEMENT VECTORS --------------------------------------------

oev_a <- TRUE
oev_a

oev_b <- FALSE
oev_b

oev_c <- T
oev_c

oev_d <- F
oev_d

oev_e <- 3L
oev_e

oev_f <- 100L
oev_f

oev_g <- 0.000017
oev_g

oev_scientific <- 1.7e-05
oev_scientific

oev_h <- "hello"
oev_h

oev_i <- 'Covid-19'
oev_i

oev_j <- "I love data analysis"
oev_j

h <- "1"
k <- "2"
h + k

# 7.2.2. LONGER ATOMIC VECTORS --------------------------------------------

1:5
x_seq <- 1:5
x_seq
typeof(x_seq)
length(x_seq)

5:1
2.5:8.5
-3:4

seq(1, 5)

c(2, 4.5, -1)
c(TRUE, FALSE, TRUE, FALSE)
c(T, F, T, F)
c("male", "female", "male", "female")

y_seq <- 3:7
c(y_seq, 2, 4.5, -1)

rep(1:4, times = 5)
rep(c(0, 4, 7), times = 3)
rep(c("a", "b", "c"), times = 2)

rep(1:4, each = 5)
rep(c(0, 4, 7), each = 3)
rep(c("a", "b", "c"), each = 2)

LETTERS
letters
month.name
month.abb

## 7.3. MIXING THINGS IN A VECTOR - COERCION ------------------------------

### 7.3.1. IMPLICIT COERCION ----------------------------------------------

my_vector <- c(1, 4, "hello", TRUE)
my_vector

# The hierarchy for coercion is: logical < integer < numeric < character

a <- c(10.5, 3.2, "I am a character string")
a
typeof(a)

b <- c(TRUE, FALSE, "Hello")
b
typeof(b)

d <- c(FALSE, TRUE, 2)
d
typeof(d)

num_TF <- c(4, FALSE, TRUE, 2, -1, TRUE, FALSE, 0)
num_TF
sum(num_TF)
mean(num_TF)

### 7.3.2. EXPLICIT COERCION ----------------------------------------------

f <- 1:5
g <- as.character(f)
g
as.numeric(g)

q <- c("1", "2", "3", "d", "5")
as.numeric(q)

x_abcde <- c("a", "b", "c", "d", "e")
as.numeric(x_abcde)

## 7.4. OPERATORS APPLIED BETWEEN TWO VECTORS -----------------------------

### 7.4.1. ARITHMETIC OPERATORS -------------------------------------------

v <- c(1, 2, 3)
v + 3
v * 3

v <- c(1, 2, 3)
t <- c(8, 3, 2)
t + v
t * v
t^v
t + 3 * v / 2

(1:5) * 2
2^(1:5)

z_seq <- 3:9
sqrt(z_seq)
round(sqrt(z_seq), digits = 2)

### 7.4.2. DOT (INNER) PRODUCT OPERATOR -----------------------------------

v %*% t

### 7.4.3. RELATIONAL OPERATORS -------------------------------------------

m <- c(4, 2, 3, 8)
m > 3
m >= 3
m == 3
m != 3

w <- c(2, 5.5, 6, 9)
z <- c(8, 2.5, 14, 9)

w > z
w == z
w >= z
w != z

### 7.4.4. LOGICAL OPERATORS APPLIED TO VECTORS ---------------------------

s <- c(1, 0, -1, 0, TRUE, TRUE, FALSE)
s

u <- c(2, 0, -2, 2, TRUE, FALSE, FALSE)
u

s & u
s[1] && u[1]
s && u

s | u
s[1] || u[1]
s || u

!s
!u

## 7.5. STATISTICAL FUNCTIONS APPLIED TO VECTORS --------------------------



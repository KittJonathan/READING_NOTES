# APPLIED STATISTICS WITH R
# DAVID DALPIAZ
# 2024-08-26
# https://book.stat420.org/

# 1. INTRODUCTION ---------------------------------------------------------

# 2. INTRODUCTION TO R ----------------------------------------------------

## 2.1. GETTING STARTED ---------------------------------------------------

# Alt + Shift + K -> keyboard shortcuts

## 2.2. BASIC CALCULATIONS ------------------------------------------------

# Addition, substraction, multiplication, and division
3 + 2
3 - 2
3 * 2
3 / 2

# Exponents
3 ^ 2
2 ^ (-3)
100 ^ (1 / 2)
sqrt(100)

# Mathematical constants
pi
exp(1)

# Logarithms
log(exp(1))
log10(1000)
log2(8)
log(16, base = 4)

# Trigonometry
sin(pi / 2)
cos(0)

## 2.3. GETTING HELP ------------------------------------------------------

?log
?sin
?paste
?lm

# 2.4. INSTALLING PACKAGES ------------------------------------------------

# install.packages("ggplot2")
# library(ggplot2)

# 3. DATA AND PROGRAMMING -------------------------------------------------

## 3.1. DATA TYPES --------------------------------------------------------

# Numeric (double)
typeof(1)
typeof(1.0)
typeof(42.5)

# Integer
typeof(1L)
typeof(2L)
typeof(45L)

# Complex
typeof(4 + 2i)

# Logical
typeof(TRUE)
typeof(FALSE)
typeof(T)
typeof(F)
typeof(NA)

# Character
typeof("a")
typeof("Statistics")
typeof("1 plus 2.")

## 3.2. DATA STRUCTURES ---------------------------------------------------

### 3.2.1. VECTORS --------------------------------------------------------

c(1, 3, 5, 7, 8, 9)

x <- c(1, 3, 5, 7, 8, 9)
x

c(42, "Statistics", TRUE)
c(42, TRUE)

(y <- 1:100)

2

seq(from = 1.5, to = 4.2, by = 0.1)
seq(1.5, 4.2, 0.1)

rep("A", times = 10)
rep(x, times = 3)

c(x, rep(seq(1, 9, 2), 3), c(1, 2, 3), 42, 2:4)

length(x)
length(y)

x
x[1]
x[3]
x[-2]
x[1:3]
x[c(1, 3, 4)]

z <- c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
z
x[z]

### 3.2.2. VECTORIZATION --------------------------------------------------

x <- 1:10
x + 1
2 * x
2 ^ x
sqrt(x)
log(x)

### 3.2.3. LOGICAL OPERATORS ----------------------------------------------

3 < 42
3 > 42
3 <= 42
3 >= 42
3 == 42
3 != 42
!(3 > 42)
(3 > 42) | TRUE
(3 & 42) & (42 > 13)

x <- c(1, 3, 5, 7, 8, 9)
x > 3
x < 3
x == 3
x != 3
x == 3 & x != 3
x == 3 | x != 3
x[x > 3]
x[x != 3]
sum(x > 3)
as.numeric(x > 3)
which(x > 3)
x[which(x > 3)]
max(x)
which(x == max(x))
which.max(x)

### 3.2.4. MORE VECTORIZATION ---------------------------------------------

x <- c(1, 3, 5, 7, 8, 9)
y <- 1:100

x + 2
x + rep(2, 6)
x > 3
x > rep(3, 6)
x + y
length(x)
length(y)
length(y) / length(x)
(x + y) - y

y <- 1:60
x + y
length(y) / length(x)
rep(x, 10) + y

all(x + y == rep(x, 10) + y)
identical(x + y, rep(x, 10) + y)

?any
?all.equal

x <- c(1, 3, 5)
y <- c(1, 2, 4)

x == y
all(x == y)
any(x == y)

x <- c(10 ^ (-8))
y <- c(10 ^ (-9))
all(x == y)
all.equal(x, y)

### 3.2.5. MATRICES -------------------------------------------------------



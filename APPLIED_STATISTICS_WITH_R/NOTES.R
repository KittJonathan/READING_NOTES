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

x <- 1:9
x

X <- matrix(x, nrow = 3, ncol = 3)
X

Y <- matrix(x, nrow = 3, ncol = 3, byrow = TRUE)
Y

Z <- matrix(0, 2, 4)
Z

X
X[1, 2]
X[1, ]
X[, 2]

X[2, c(1, 3)]

x <- 1:9
rev(x)
rep(1, 9)
rbind(x, rev(x), rep(1, 9))
cbind(col_1 = x, col_2 = rev(x), col_3 = rep(1, 9))

x <- 1:9
y <- 9:1
X <- matrix(x, 3, 3)
Y <- matrix(y, 3, 3)
X
Y
X + Y
X - Y
X * Y
X / Y

X %*% Y  # matrix multiplication
t(X)  # transpose matrix

Z <- matrix(c(9, 2, -3, 2, 4, -2, -3, -2, 16), 3, byrow = TRUE)
Z
solve(Z)  # inverse of a square matrix (if it is invertible)
solve(Z) %*% Z
diag(3)
all.equal(solve(Z) %*% Z, diag(3))

X <- matrix(1:6, 2, 3)
X
dim(X)
rowSums(X)
colSums(X)
rowMeans(X)
colMeans(X)

diag(Z)  # extract the diagonal of a matrix
diag(1:5)  # create a matrix with specified elements on the diagonal
diag(5)

a_vec <- c(1, 2, 3)
b_vec <- c(2, 2, 2)
c(is.vector(a_vec), is.vector(b_vec))
c(is.matrix(a_vec), is.matrix(b_vec))

a_vec %*% b_vec  # inner product
a_vec %o% b_vec  # outer product

as.matrix(a_vec)
as.matrix(a_vec) %*% b_vec
as.matrix(a_vec) %*% as.matrix(b_vec)

crossprod(a_vec, b_vec)
tcrossprod(a_vec, b_vec)

C_mat <- matrix(c(1, 2, 3, 4, 5, 6), 2, 3)
D_mat <- matrix(c(2, 2, 2, 2, 2, 2), 2, 3)
C_mat
D_mat

crossprod(C_mat, D_mat)
t(C_mat) %*% D_mat
all.equal(crossprod(C_mat, D_mat), t(C_mat) %*% D_mat)

crossprod(C_mat, C_mat)
t(C_mat) %*% C_mat
all.equal(crossprod(C_mat, C_mat), t(C_mat) %*% C_mat)

### 3.2.6. LISTS ----------------------------------------------------------

list(42, "Hello", TRUE)

ex_list <- list(
  a = c(1, 2, 3, 4),
  b = TRUE,
  c = "Hello!",
  d = function(arg = 42) {print("Hello World!")},
  e = diag(5)
)

ex_list$e
ex_list[1:2]
ex_list[1]
ex_list[[1]]
ex_list[c("e", "a")]
ex_list["e"]
ex_list[["e"]]
ex_list$d
ex_list$d(arg = 1)

### 3.2.7. DATA FRAMES ----------------------------------------------------

example_data <- data.frame(x = c(1, 3, 5, 7, 9, 1, 3, 5, 7, 9),
                           y = c(rep("Hello", 9), "Goodbye"),
                           z = rep(c(TRUE, FALSE), 5))
example_data

example_data$x

all.equal(length(example_data$x),
          length(example_data$y),
          length(example_data$z))

str(example_data)
nrow(example_data)
ncol(example_data)
dim(example_data)

example_data_from_csv <- readr::read_csv("APPLIED_STATISTICS_WITH_R/example-data.csv")
example_data_from_csv

example_data <- tibble::as_tibble(example_data)
example_data

library(ggplot2)

head(mpg, n = 10)
mpg
str(mpg)
?mpg
names(mpg)
mpg$year
mpg$hwy
dim(mpg)
nrow(mpg)
ncol(mpg)

mpg[mpg$hwy > 35, c("manufacturer", "model", "year")]
subset(mpg, subset = hwy > 35, select = c("manufacturer", "model", "year"))

library(dplyr)
mpg |> filter(hwy > 35) |> select(manufacturer, model, year)

## 3.3. PROGRAMMING BASICS ------------------------------------------------

### 3.3.1. CONTROL FLOW ---------------------------------------------------

x <- 1
y <- 3

if (x > y) {
  z = x * y
  print("x is larger than y")
} else {
  z = x + 5 * y
  print("x is less than or equal to y")
}

z

ifelse(4 > 3, 1, 0)

fib <- c(1, 1, 2, 3, 5, 8, 13, 21)
ifelse(fib > 6, "Foo", "Bar")

x <- 11:15
for (i in 1:5) {
  x[i] = x[i] * 2
}
x

### 3.3.2. FUNCTIONS ------------------------------------------------------

standardize <- function(x) {
  m = mean(x)
  std = sd(x)
  result = (x - m) / std
  result
}

(test_sample <- rnorm(n = 10, mean = 2, sd = 5))
standardize(x = test_sample)

standardize <- function(x) {
  (x - mean(x)) / sd(x)
}
standardize(test_sample)

power_of_sum <- function(num, power = 2) {
  num ^ power
}

power_of_sum(10)
power_of_sum(10, 2)
power_of_sum(num = 10, power = 2)
power_of_sum(2, 10)
power_of_sum(power = 5)

get_var <- function(x, biased = FALSE) {
  n = length(x) - 1 * !biased
  (1 / n) * sum((x - mean(x))^2)
}

get_var(test_sample)
get_var(test_sample, biased = FALSE)
var(test_sample)
get_var(test_sample, biased = TRUE)

# 4. SUMMARIZING DATA -----------------------------------------------------

## 4.1. SUMMARY STATISTICS ------------------------------------------------

library(ggplot2)

# Central tendency
mean(mpg$cty)
median(mpg$cty)

# Spread
var(mpg$cty)
sd(mpg$cty)
IQR(mpg$cty)
min(mpg$cty)
max(mpg$cty)
range(mpg$cty)

# Categorical
table(mpg$drv)
table(mpg$drv) / nrow(mpg)

## 4.2. PLOTTING ----------------------------------------------------------

### 4.2.1. HISTOGRAMS -----------------------------------------------------

hist(mpg$cty)
hist(mpg$cty,
     xlab = "Miles Per Gallon (City)",
     main = "Histogram of MPG (City)",
     breaks = 12,
     col = "dodgerblue",
     border = "darkorange")

### 4.2.2. BARPLOTS -------------------------------------------------------

barplot(table(mpg$drv))
barplot(table(mpg$drv),
        xlab = "Drivetrain (f = FWD, r = RWD, 4 = 4WD)",
        ylab = "Frequency",
        main = "Drivetrains",
        col = "dodgerblue",
        border = "darkorange")

### 4.2.3. BOXPLOTS -------------------------------------------------------

unique(mpg$drv)
boxplot(mpg$hwy)
boxplot(hwy ~ drv, data = mpg)
boxplot(hwy ~ drv, data = mpg,
        xlab = "Drivetrain (f = FWD, r = RWD, 4 = 4WD)",
        ylab = "Miles Per Gallon (Highway)",
        main = "MPG (Highway) vs Drivetrain",
        pch = 20,
        cex = 2,
        col = "darkorange",
        border = "dodgerblue")

### 4.2.4. SCATTERPLOTS ---------------------------------------------------

plot(hwy ~ displ, data = mpg)
plot(hwy ~ displ, data = mpg,
     xlab = "Engine Displacement (in Liters)",
     ylab = "Miles Per Gallon (Highway)",
     main = "MPG (Highway) vs Engine Displacement",
     pch = 20,
     cex = 2,
     col = "dodgerblue")

# 5. PROBABILITY AND STATISTICS IN R --------------------------------------

## 5.1. PROBABILITY IN R --------------------------------------------------



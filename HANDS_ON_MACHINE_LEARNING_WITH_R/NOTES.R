# HANDS-ON MACHINE LEARNING WITH R
# BRADLEY BOEHMKE & BRANDON GREENWELL
# 2024-08-21

# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(rsample)
library(caret)
library(h2o)
library(splines)

# 1. INTRODUCTION TO MACHINE LEARNING -------------------------------------

# THE DATA SETS

# Property sales information
ames <- AmesHousing::make_ames()
dim(ames)
head(ames$Sale_Price)

# Employee attrition
attrition <- modeldata::attrition
dim(attrition)
head(attrition$Attrition)

# Image information for handwritten numbers
mnist <- dslabs::read_mnist()
names(mnist)
dim(mnist$train$images)
head(mnist$train$labels)

# Grocery items and quantities purchased
my_basket <- readr::read_csv("https://koalaverse.github.io/homlr/data/my_basket.csv")
dim(my_basket)
my_basket

# 2. MODELING PROCESS -----------------------------------------------------

## 2.1. PREREQUISITIES ----------------------------------------------------

# h2o setup
h2o.no_progress()
h2o.init()

# Ames housing data
ames <- AmesHousing::make_ames()
ames.h2o <- as.h2o(ames)

# Job attrition data
churn <- modeldata::attrition
glimpse(churn)

churn <- churn |> 
  mutate(Education = factor(Education, ordered = F),
         EnvironmentSatisfaction = factor(EnvironmentSatisfaction, ordered = F),
         JobInvolvement = factor(JobInvolvement, ordered = F),
         JobSatisfaction = factor(JobSatisfaction, ordered = F),
         PerformanceRating = factor(PerformanceRating, ordered = F),
         RelationshipSatisfaction = factor(RelationshipSatisfaction, ordered = F),
         WorkLifeBalance = factor(WorkLifeBalance, ordered = F))
churn.h2o <- as.h2o(churn)

## 2.2. DATA SPLITTING ----------------------------------------------------

### 2.2.1. SIMPLE RANDOM SAMPLING -----------------------------------------

# Using base R
set.seed(123)
index_1 <- sample(x = 1:nrow(ames), round(nrow(ames) * 0.7))
train_1 <- ames[index_1, ]
test_1 <- ames[-index_1, ]

# Using caret package
set.seed(123)
index_2 <- createDataPartition(ames$Sale_Price, p = 0.7, list = FALSE)
train_2 <- ames[index_2, ]
test_2 <- ames[-index_2, ]

# Using rsample package
set.seed(123)
split_1 <- initial_split(data = ames, prop = 0.7)
train_3 <- training(split_1)
test_3 <- testing(split_1)

# Using h2o package
split_2 <- h2o.splitFrame(ames.h2o, ratios = 0.7, seed = 123)
train_4 <- split_2[[1]]
test_4 <- split_2[[2]]

### 2.2.2. STRATIFIED SAMPLING --------------------------------------------

# Original response distribution
table(churn$Attrition) |> prop.table()

# Stratified sampling with the rsample package
set.seed(123)
split_strat <- initial_split(churn, prop = 0.7, strata = "Attrition")
train_strat <- training(split_strat)
test_strat <- testing(split_strat)

# Consistent response ratio between train & test
table(train_strat$Attrition) |> prop.table()
table(test_strat$Attrition) |> prop.table()

## 2.3. CREATING MODELS IN R ----------------------------------------------

### 2.3.1. MANY FORMULA INTERFACES ----------------------------------------

# Sale price as function of neighborhood and year sold
lm(Sale_Price ~ Neighborhood + Year_Sold,
   data = ames)

# Variables + interactions
lm(Sale_Price ~ Neighborhood + Year_Sold +
     Neighborhood:Year_Sold,
   data = ames)

# Shorthand for all predictors
lm(Sale_Price ~ ., data = ames)

# Inline functions / transformations
lm(log10(Sale_Price) ~ ns(Longitude, df = 3) +
     ns(Latitude, df = 3), data = ames)

### 2.3.2. MANY ENGINES ---------------------------------------------------

lm_lm <- lm(Sale_Price ~ ., data = ames)
lm_glm <- glm(Sale_Price ~ ., data = ames, family = gaussian)
lm_caret <- train(Sale_Price ~ ., data = ames, method = "lm")

## 2.4. RESAMPLING METHODS ------------------------------------------------

### 2.4.1. K-FOLD CROSS VALIDATION ----------------------------------------

h2o.cv <- h2o.glm(
  x = x,
  y = y,
  training_frame = ames.h2o,
  nfolds = 10
  )

vfold_cv(data = ames, v = 10)

### 2.4.2. BOOTSTRAPPING --------------------------------------------------

bootstraps(ames, times = 10)

## 2.6. MODEL EVALUATION --------------------------------------------------



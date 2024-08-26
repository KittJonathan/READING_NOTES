# HANDS-ON MACHINE LEARNING WITH R
# BRADLEY BOEHMKE & BRANDON GREENWELL
# 2024-08-21

# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(rsample)
library(caret)
library(h2o)
library(splines)
library(visdat)
library(recipes)

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

## 2.1. PREREQUISITES -----------------------------------------------------

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

### 2.6.1. REGRESSION MODELS ----------------------------------------------

# MSE : Mean Squared Error
# RMSE : Root Mean Squared Error
# Deviance
# MAE : Mean Absolute Error
# RMSLE : Root Mean Squared Logarithmic Error

### 2.6.2. CLASSIFICATION MODELS ------------------------------------------

# Misclassification
# Mean per class error
# Mean Squared Error
# Cross-entropy (aka Log Loss or Deviance)
# Gini index

# Confusion matrix : 
# Accuracy = (TP + TN) / total
# Precision = TP / (TP + FP)
# Sensitivity = TP / (TP + FN)
# Specificity = TN / (TN + FP)

# AUC : Area Under the Curve

## 2.7. PUTTING THE PROCESSES TOGETHER ------------------------------------

# Stratified sampling with the rsample package
set.seed(123)
split <- initial_split(ames, prop = 0.7, strata = "Sale_Price")
ames_train <- training(split)
ames_test <- testing(split)

# Specify resampling strategy
cv <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5
)

# Create grid of hyperparameter values
hyper_grid <- expand.grid(k = seq(2, 25, by = 1))

# Tune a knn model using grid search
knn_fit <- train(
  Sale_Price ~ .,
  data = ames_train,
  method = "knn",
  trControl = cv,
  tuneGrid = hyper_grid,
  metroc = "RMSE"
)

# Print and plot the CV results
knn_fit
ggplot(knn_fit)

# 3. FEATURE & TARGET ENGINEERING -----------------------------------------

## 3.1. PREREQUISITES -----------------------------------------------------

library(dplyr)
library(ggplot2)
library(visdat)

library(caret)
library(recipes)

## 3.2. TARGET ENGINEERING ------------------------------------------------

# Help correct for positively skewed target variables

# Log transformation :

transformed_response <- log(ames_train$Sale_Price)

ames_recipe <- recipe(Sale_Price ~ ., data = ames_train) |> 
  step_log(all_outcomes())
ames_recipe

log(-0.5)
log1p(-0.5)

# Box-Cox transformation

# Log transform a value
y <- log(10)

# Undo log-transformation
exp(y)

# Box Cox transform a value
y <- forecast::BoxCox(10, lambda = 1)

# Inverse Box Cox function
inv_box_cox <- function(x, lambda) {
  
  if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda)
  
  }

inv_box_cox(y, lambda = 1)

## 3.3. DEALING WITH MISSINGNESS ------------------------------------------

### 3.3.1. VISUALIZING MISSING VALUES -------------------------------------

sum(is.na(AmesHousing::ames_raw))

AmesHousing::ames_raw |> 
  is.na() |> 
  reshape2::melt() |> 
  ggplot(aes(Var2, Var1, fill = value)) +
  geom_raster() +
  coord_flip() +
  scale_y_continuous(NULL, expand = c(0, 0)) +
  scale_fill_grey(name = "",
                  labels = c("Present", "Missing")) +
  xlab("Observation") +
  theme(axis.text.y = element_text(size = 4))

AmesHousing::ames_raw |> 
  filter(is.na(`Garage Type`)) |> 
  select(`Garage Type`, `Garage Cars`, `Garage Area`)

vis_miss(AmesHousing::ames_raw, cluster = TRUE)

### 3.3.2. IMPUTATION -----------------------------------------------------

#### 3.3.2.1. ESTIMATED STATISTIC -----------------------------------------

ames_recipe |> 
  step_impute_median(Gr_Liv_Area)

#### 3.3.2.2. K-NEAREST NEIGHBOR ------------------------------------------

ames_recipe |> 
  step_impute_knn(all_predictors(), neighbors = 6)

#### 3.3.2.3. TREE-BASED --------------------------------------------------

ames_recipe |> 
  step_impute_bag(all_predictors())

## 3.4. FEATURE FILTERING -------------------------------------------------

caret::nearZeroVar(ames_train, saveMetrics = TRUE) |> 
  tibble::rowid_to_column() |> 
  filter(nzv)

## 3.5. NUMERIC FEATURE ENGINEERING ---------------------------------------

### 3.5.1. SKEWNESS -------------------------------------------------------

# Normalize all numerical columns
recipe(Sale_Price ~ ., data = ames_train) |> 
  step_YeoJohnson(all_numeric())

### 3.5.2. STANDARDIZATION ------------------------------------------------

ames_recipe |> 
  step_center(all_numeric(), -all_outcomes()) |> 
  step_scale(all_numeric(), -all_outcomes())

## 3.6. CATEGORICAL FEATURE ENGINEERING -----------------------------------

### 3.6.1. LUMPING --------------------------------------------------------

count(ames_train, Neighborhood) |> 
  arrange(n)

count(ames_train, Screen_Porch) |> 
  arrange(n)

# Lump levels for two features
lumping <- recipe(Sale_Price ~ ., data = ames_train) |> 
  step_other(Neighborhood, threshold = 0.01, other = "other") |> 
  step_other(Screen_Porch, threshold = 0.1, other = ">0")

# Apply this blueprint
apply_2_training <- prep(lumping, training = ames_train) |> 
  bake(ames_train)

# New distribution of Neighborhood
count(apply_2_training, Neighborhood) |> arrange(n)

# New distribution of Screen_Porch
count(apply_2_training, Screen_Porch) |> arrange(n)

# 3.6.2. ONE-HOT AND DUMMY ENCODING ---------------------------------------



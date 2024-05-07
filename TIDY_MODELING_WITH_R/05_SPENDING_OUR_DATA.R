# TIDY MODELING WITH R
# MODELING BASICS
# 5. SPENDING OUR DATA
# https://www.tmwr.org/splitting
# 2024-05-07

# PACKAGES ----------------------------------------------------------------

library(modeldata)
library(tidymodels)
tidymodels_prefer()

# 5.1 COMMON METHODS FOR SPLITTING DATA -----------------------------------

data(ames)
ames <- ames |> mutate(Sale_Price = log10(Sale_Price))

# Set the random number stream using `set.seed()` so that the 
# results can be reproduced later.
set.seed(501)

# Save the split information for a 80/20 split of the data
ames_split <- initial_split(ames, prop = 0.80)
ames_split

ames_train <- training(ames_split)
ames_test <- testing(ames_split)

dim(ames_train)
dim(ames_test)

# Stratified sampling
set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

dim(ames_train)

# 5.2 WHAT ABOUT A VALIDATION SET? ----------------------------------------

set.seed(52)

# To put 60% into training, 20% in validation, and 20% in testing:

ames_val_split <- initial_validation_split(ames, prop = c(0.6, 0.2))

ames_val_split

ames_train <- training(ames_val_split)
ames_test <- testing(ames_val_split)
ames_val <- validation(ames_val_split)

# 5.5 CHAPTER SUMMARY -----------------------------------------------------

library(tidymodels)
data(ames)
ames <- ames |> mutate(Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

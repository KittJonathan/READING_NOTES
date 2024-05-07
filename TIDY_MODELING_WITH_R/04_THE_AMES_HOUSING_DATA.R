# TIDY MODELING WITH R
# MODELING BASICS
# 4. THE AMES HOUSING DATA
# https://www.tmwr.org/ames
# 2024-05-07

# PACKAGES ----------------------------------------------------------------

library(modeldata)
library(tidymodels)
tidymodels_prefer()

# 4.1 EXPLORING FEATURES OF HOMES IN AMES ---------------------------------

data(ames)
dim(ames)

ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 50, col = "white")

ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 50, col = "white") +
  scale_x_log10()

ames <- ames |> 
  mutate(Sale_Price = log10(Sale_Price))

ggplot(ames, aes(x = Longitude, y = Latitude)) +
  geom_point(aes(col = Neighborhood)) +
  theme(legend.position = "bottom")

# 4.2 CHAPTER SUMMARY -----------------------------------------------------

library(tidymodels)
data(ames)
ames <- ames |> mutate(Sale_Price = log10(Sale_Price))

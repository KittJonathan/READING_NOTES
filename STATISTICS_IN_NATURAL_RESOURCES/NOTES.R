# STATISTICS IN NATURAL RESOURCES
# MATTHEW RUSSELL
# https://stats4nr.com/
# 2024-07-22

# PACKAGES ----------------------------------------------------------------

library(stats4nr)
library(tidyverse)
library(hexbin)
library(modeest)

# 1. VISUALIZING DATA -----------------------------------------------------

elm
dim(elm)
head(elm)
tail(elm)

summary(elm)

n_Crowns <- table(elm$CROWN_CLASS_CD)
n_Crowns
prop.table(n_Crowns)

CO2 <- tibble(CO2)
?CO2
min(CO2$uptake)
dim(CO2)
table(CO2$Treatment)

elm |> 
  mutate(DIA_cm = DIA * 2.54)

CO2 |> 
  mutate(category = ifelse(conc >= 500, "HIGH", "LOW"))

ggplot(data = elm,
       aes(x = DIA, y = HT)) + 
  geom_point()

elm |> 
  count(CROWN_CLASS_CD)

elm_summ <- elm |> 
  summarise(n_trees = n(), .by = CROWN_CLASS_CD) |> 
  mutate(Pct = n_trees / sum(n_trees) * 100) |> 
  arrange(CROWN_CLASS_CD) |> 
  mutate(CROWN_CLASS_NM = c("Open grown", "Dominant", "Co-dominant",
                            "Intermediate", "Suppressed"),
         .after = CROWN_CLASS_CD)

ggplot(data = elm_summ,
       aes(x = "", y = n_trees,
           fill = CROWN_CLASS_CD)) +
  geom_bar(stat = "identity") +
  coord_polar("y")

ggplot(data = elm,
       aes(x = CROWN_CLASS_CD)) +
  geom_bar() +
  coord_polar()

stem(elm$HT, width = 50)

ggplot(data = elm,
       aes(HT)) +
  geom_histogram()

ggplot(data = elm,
       aes(HT)) +
  geom_histogram(bins = 10)

ggplot(data = elm,
       aes(HT)) +
  geom_density(color = "blue")

ggplot(data = elm,
       aes(x = 1, y = HT)) +
  geom_boxplot()

ggplot(data = elm,
       aes(x = 1, y = HT)) +
  geom_violin()

ggplot(data = elm,
       aes(x = factor(CROWN_CLASS_CD), y = HT)) +
  geom_boxplot()

ggplot(data = elm,
       aes(x = factor(CROWN_CLASS_CD), y = HT)) +
  geom_violin()

ggplot(data = elm,
       aes(x = DIA, y = HT,
           color = factor(CROWN_CLASS_CD))) +
  geom_point()

ggplot(data = elm,
       aes(x = DIA, y = HT)) +
  geom_point() +
  geom_smooth()

ggplot(data = elm,
       aes(x = DIA, y = HT)) +
  geom_point() +
  facet_wrap(~CROWN_CLASS_CD)

ggplot(data = elm,
       aes(x = DIA, y = HT)) +
  geom_hex(bins = 20)

ggplot(data = elm,
       aes(x = DIA, y = HT))

ggplot(data = CO2,
       aes(x = Plant)) +
  geom_bar()

ggplot(data = CO2,
       aes(x = conc, y = uptake)) +
  # geom_hex(bins = 5)
  # geom_hex(bins = 20)
  # geom_hex(bins = 40)
  geom_hex(bins = 60)

ggplot(data = CO2,
       aes(x = conc, y = uptake)) +
  geom_point() +
  facet_grid(Type ~ Treatment)

ggplot(data = elm,
       aes(x = DIA, y = HT)) +
  geom_point() +
  labs(x = "Tree diameter (inches)",
       y = "Tree height (feet)",
       title = "Cedar elm trees in Austin, Texas",
       subtitle = "Trees that have a larger diameter are taller.",
       caption = "Source: USDA Forest Inventory and Analysis")

ggplot(data = elm,
       aes(x = fct_infreq(factor(CROWN_CLASS_CD)))) +
  geom_bar()

ggplot(data = elm,
       aes(x = fct_rev(fct_infreq(factor(CROWN_CLASS_CD))))) +
  geom_bar()

ggplot(data = elm,
       aes(x = DIA, y = HT)) +
  geom_point() +
  labs(title = "theme_bw()") +
  theme_bw()

ggplot(data = elm,
       aes(x = DIA, y = HT)) +
  geom_point() +
  labs(title = "theme_classic()") +
  theme_classic()

ggplot(data = elm,
       aes(x = DIA, y = HT)) +
  geom_point() +
  labs(title = "theme_void()") +
  theme_void()

ggplot(data = elm, 
       aes(x = DIA, y = HT)) +
  geom_point()

# ggsave("STATISTICS_IN_NATURAL_RESOURCES/elm_scatter.jpg", height = 3, width = 5, units = "in")

ggplot(data = CO2,
       aes(x = Type, y = uptake)) +
  geom_boxplot() +
  labs(x = "Location", y = "Uptake")

ggplot(data = elm,
       aes(x = CROWN_CLASS_CD)) +
  geom_bar()

ggplot(data = elm, 
       aes(x = DIA, y = HT)) +
  geom_point() +
  scale_x_continuous(limits = c(10, 15)) +
  scale_y_continuous(limits = c(30, 40))

# 2. SUMMARY STATISTICS AND DISTRIBUTIONS ---------------------------------

head(chirps)

chirps |> 
  arrange(chirps)

# Measures of central tendency
mean(chirps$cps)
median(chirps$cps)
mfv(chirps$cps)

# Measures of spread
var(chirps$cps)
sd(chirps$cps)

(sd(chirps$cps) / mean(chirps$cps)) * 100  # coeff of variation (CV)

# Measures of position (z scores)
(16 - mean(chirps$cps)) / sd(chirps$cps)
(20 - mean(chirps$cps)) / sd(chirps$cps)

ggplot(data = chirps, 
       aes(x = 1, y = cps)) +
  geom_boxplot() +
  scale_x_continuous(breaks = NULL) +
  labs(y = "Chirps per second")

head(ant)

ant |> 
  summarise(spprich_mean = mean(spprich),
            .by = ecotype)

ant |> 
  ggplot(aes(x = ecotype, y = spprich)) +
  geom_violin()

ant |> 
  reframe(spprich_range = range(spprich),
          .by = ecotype)

ant |> 
  summarise(spprich_iqr = IQR(spprich),
            .by = ecotype)

ant |> 
  summarise(spprich_sd = sd(spprich),
            spprich_cv = 100 * sd(spprich) / mean(spprich),
            .by = ecotype)

ant |> 
  summarise(spprich_sd = sd(spprich),
            spprich_mean = mean(spprich),
            .by = ecotype) |> 
  mutate(x = 7,
         z = (x - spprich_mean) / spprich_sd)

z_score <- function(x_i, pop_mean, pop_sd) {
  
  z <- (x_i - pop_mean) / pop_sd
  return(z)

  }

z_score(x_i = 38, pop_mean = 42, pop_sd = 3)

deer <- tibble(
  deer = c(0, 1, 2),
  prob = c(0.7, 0.1, 0.2))

deer_mu <- sum(deer$deer * deer$prob)
deer_sd <- sqrt(sum((deer$deer - deer_mu)^2 * deer$prob))

wolves <- tibble(
  age = 0:9,
  num_wolves = c(10, 9, 21, 8, 9, 5, 2, 2, 1, 2))

wolves |> 
  summarise(age_mean = mean(age),
            age_sd = sd(age))

summary(wolves$age)

z_score(x_i = 6, pop_mean = 4.5, pop_sd = 3.03)

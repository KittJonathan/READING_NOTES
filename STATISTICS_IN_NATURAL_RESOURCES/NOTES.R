# STATISTICS IN NATURAL RESOURCES
# MATTHEW RUSSELL
# https://stats4nr.com/
# 2024-07-22

# PACKAGES ----------------------------------------------------------------

library(stats4nr)
library(tidyverse)
library(hexbin)
library(modeest)
library(plotrix)

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

c(6.3 - 1 * 0.8, 6.3 + 1 * 0.8)
c(6.3 - 2 * 0.8, 6.3 + 2 * 0.8)
c(6.3 - 3 * 0.8, 6.3 + 3 * 0.8)

z_score(x_i = 8, pop_mean = 6.3, pop_sd = 0.8)

# 3. PROBABILITY ----------------------------------------------------------

# angiosperm 5/12
# Quercus 2/12
# not deciduous 5/12

# Pinus 3/12 * 2/11 * 1/10
# gymnno 7/12 * 6/11 * 5/10
# gymno, gymno, angio 7/12 * 6/11 * 5/10
# angio | Pinus (5/12 * 4/11 * 3/10) + (3/12 * 2/11 * 1/10)

# 0.3 damage & disease
# 0.65 of those trees damage only
# 0.3 / 0.65

birthday <- function(n) {
  q <- 1 - (1:(n-1)) / 365
  print(p <- 1 - prod(q))
}

birthday(5)
birthday(20)
birthday(40)

(1/12) * (1/11)
1 / prod(12:11)

choose(12, 2)
prod(12:1) / (prod(2:1) * prod(10:1))

sample(1:20, 5)
sample(1:20, 5, replace = TRUE)
choose(20, 4)
choose(20, 4)
1 / (choose(20, 1) * choose(19, 1) * choose(18, 1) * choose(17, 1))
1 / prod(20:17)

p.binomial <- function(n, p, title, ymax, xmax) {
  x <- dbinom(0:n, size = n, prob = p)
  barplot(x, ylim = c(0, ymax), xlim = c(0, xmax),
          names.arg = 0:n,
          xlab = "Number of grouse with West Nile",
          ylab = "Probability",
          main = sprintf(paste(title)))
}

p.binomial(n = 6, p = 0.13, 
           title = "Binomial distribution (n = 6, p = 0.13)",
           ymax = 0.5, xmax = 7)

p.binomial(n = 50, p = 0.29, 
           title = "Binomial distribution (n = 50, p = 0.29)",
           ymax = 0.15, xmax = 51)

dbinom(x = 0, size = 6, prob = 0.13)

dbinom(x = 0, size = 6, prob = 0.13) + dbinom(x = 1, size = 6, prob = 0.13) + dbinom(x = 2, size = 6, prob = 0.13)
pbinom(q = 2, size = 6, prob = 0.13)

p.binomial(n = 25, p = 0.13, 
           title = "Binomial distribution (n = 25, p = 0.13)",
           ymax = 0.3, xmax = 26)

dbinom(x = 5, size = 25, prob = 0.13)
pbinom(q = 5, size = 25, prob = 0.13)
1 - pbinom(q = 5, size = 25, prob = 0.13)

dnorm(x = 38, mean = 42, sd = 3)
pnorm(q = 38, mean = 42, sd = 3)

z_score(x_i = 8, pop_mean = 6.3, pop_sd = 0.8)
x <- tibble(x = rnorm(n = 100, mean = 6.3, sd = 0.8))
ggplot(data = x, aes(x)) + geom_histogram()

dnorm(x = 2, mean = 4.8, sd = 1.5)
pnorm(q = 7, mean = 4.8, sd = 1.5) - pnorm(q = 4, mean = 4.8, sd = 1.5)

# 4. HYPOTHESIS TESTS FOR MEANS AND VARIANCES -----------------------------

qt(p = 0.95, df = 9)

head(chirps)

mean_cps <- mean(chirps$cps)
sd_cps <- sd(chirps$cps)
n_cps <- length(chirps$cps)

low_bound <- mean_cps + (qt(0.025, df = n_cps - 1) * (sd_cps / sqrt(n_cps)))
high_bound <- mean_cps + (qt(0.975, df = n_cps - 1) * (sd_cps / sqrt(n_cps)))

low_bound; high_bound

low_bound_90 <- mean_cps + (qt(0.05, df = n_cps - 1) * (sd_cps / sqrt(n_cps)))
high_bound_90 <- mean_cps + (qt(0.95, df = n_cps - 1) * (sd_cps / sqrt(n_cps)))

low_bound_90; high_bound_90

qt(p = 0.1, df = 8)
qt(p = 0.1, df = 16)
qt(p = 0.75, df = 10)
qt(p = 0.9, df = 22)

mean_hg <- 0.540
sd_hg <- 0.399
n_hg <- 60

low_bound_95 <- mean_hg + (qt(0.025, df = n_hg - 1) * (sd_hg / sqrt(n_hg)))
high_bound_95 <- mean_hg + (qt(0.975, df = n_hg - 1) * (sd_hg / sqrt(n_hg)))
low_bound_95; high_bound_95

low_bound_80 <- mean_hg + (qt(0.1, df = n_hg - 1) * (sd_hg / sqrt(n_hg)))
high_bound_80 <- mean_hg + (qt(0.9, df = n_hg - 1) * (sd_hg / sqrt(n_hg)))
low_bound_80; high_bound_80

mean_perch <- 260
sd_perch <- sqrt(10.2)
n_perch <- 18

low_bound_90 <- mean_perch + (qt(0.05, df = n_perch - 1) * (sd_perch / sqrt(n_perch)))
high_bound_90 <- mean_perch + (qt(0.95, df = n_perch - 1) * (sd_perch / sqrt(n_perch)))
low_bound_90; high_bound_90

t.test(x = chirps$cps, mu = 18)
qt(p = 0.025, df = 14)

t.test(x = chirps$cps, mu = 18, conf.level = 0.90)

t.test(x = chirps$cps, mu = 18, alternative = "greater")
t.test(x = chirps$cps, mu = 18, alternative = "less")

wilcox.test(chirps$cps, mu = 18)

chirps <- chirps |> 
  mutate(cps_2 = c(20.5, 16.3, 20.9, 18.6, 17.5, 15.7, 
                   14.9, 17.5, 15.9, 16.5, 15.3, 17.7, 
                   16.2, 17.6, 14.6))

chirps|> 
  pivot_longer(cols = !temp_F, names_to = "dataset", values_to = "cps") |> 
  ggplot(aes(dataset, cps)) +
  geom_boxplot() +
  labs(x = "Data set", y = "Chirps per second")

t.test(chirps$cps, chirps$cps_2, conf.level = 0.95, var.equal = TRUE)

t.test(chirps$cps, chirps$cps_2, conf.level = 0.95, var.equal = FALSE)

hunt <- tibble(
  hunter_id = rep(1:8, 2),
  time = rep(c("Pre", "Post"), each = 8),
  days = c(5, 7, 3, 9, 4, 1, 6, 2, 8, 7, 4, 11, 9, 3, 10, 5)
)

hunt |> 
  ggplot(aes(time, days)) +
  geom_boxplot() +
  geom_point() +
  labs(x = "Time period",
       y = "Estimated days to hunt")

pre <- hunt |> 
  filter(time == "Pre")

post <- hunt |> 
  filter(time == "Post")

t.test(pre$days, post$days, paired = TRUE)

mean_ht <- mean(elm$HT)
sd_ht <- sd(elm$HT)
n_ht <- length(elm$HT)
se_ht <- sd(elm$HT) / sqrt(length(elm$HT))
plotrix::std.error(elm$HT)

low_bound_95 <- mean_ht + (qt(0.025, df = n_ht - 1) * se_ht)
high_bound_95 <- mean_ht + (qt(0.975, df = n_ht - 1) * se_ht)

res <- t.test(elm$HT, conf.level = 0.95)
res$conf.int

t.test(elm$HT, mu = 30)
t.test(elm$HT, mu = 30, alternative = "less", conf.level = 0.9)

elm_intermediate <- elm |> 
  filter(CROWN_CLASS_CD == 4)

elm_suppressed <- elm |> 
  filter(CROWN_CLASS_CD == 5)

t.test(elm_intermediate$HT, elm_suppressed$HT, 
       conf.level = 0.95, var.equal = FALSE)

t.test(elm_intermediate$DIA, elm_suppressed$DIA, 
       conf.level = 0.9, var.equal = TRUE)

hunt <- tibble(
  hunter_id = rep(1:8, 2),
  time = rep(c("Pre", "Post"), each = 8),
  days = c(5, 7, 3, 9, 4, 1, 6, 2, 8, 7, 4, 11, 9, 3, 10, 5)
)

pre <- hunt |> 
  filter(time == "Pre")

post <- hunt |> 
  filter(time == "Post")

t.test(pre$days, post$days, paired = TRUE)
t.test(pre$days, post$days, var.equal = TRUE)

qf(p = 0.90, df1 = 4, df2 = 9)

chirps <- chirps |> 
  mutate(cps_2 = c(20.5, 16.3, 20.9, 18.6, 17.5, 15.7, 
                   14.9, 17.5, 15.9, 16.5, 15.3, 17.7, 
                   16.2, 17.6, 14.6))

var.test(chirps$cps, chirps$cps_2, conf.level = 0.90)

qf(p = 0.90, df1 = 14, df2 = 14)

qf(p = 0.80, df1 = 4, df2 = 6)
qf(p = 0.95, df1 = 4, df2 = 6)

qf(p = 0.90, df1 = 5, df2 = 10)
qf(p = 0.90, df1 = 10, df2 = 20)

var.test(elm_intermediate$HT, elm_suppressed$HT, conf.level = 0.95)
var.test(elm_intermediate$DIA, elm_suppressed$DIA, conf.level = 0.90)

# 5. INFERENCE FOR COUNTS AND PROPORTIONS ---------------------------------



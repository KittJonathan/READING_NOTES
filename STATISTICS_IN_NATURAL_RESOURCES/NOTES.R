# STATISTICS IN NATURAL RESOURCES
# MATTHEW RUSSELL
# https://stats4nr.com/
# 2024-07-22

# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(hexbin)

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

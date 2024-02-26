# Data and Packages -------------------------------------------------------

library(tidyverse)
library(marginaleffects)
library(broom)

countries <- readr::read_rds("https://github.com/Sociology-FA-CU/applied-regression-r/raw/main/data/countries.rds")
un <- readr::read_rds("https://github.com/Sociology-FA-CU/applied-regression-r/raw/main/data/united-nations.rds")
turnout <- readr::read_rds("https://github.com/Sociology-FA-CU/applied-regression-r/raw/main/data/vote-2017-parliament.rds")


# Data preparation --------------------------------------------------------

countries = countries %>% 
  mutate(gdp_pc = gdp / population*1000,
         poverty_risk=poverty_risk*100,
         hdi=hdi*100)
#glimpse(countries)

# Multiple regression -----------------------------------------------------
m1 = lm(life_exp ~ poverty_risk, data=countries)
summary(m1)
plot_predictions(m1, condition = "poverty_risk")

m2 = lm(life_exp ~ poverty_risk + hdi, data=countries)
summary(m2)
plot_predictions(m2, condition = "poverty_risk")
plot_predictions(m2, condition = "hdi")

tidy(m2, conf.int = 0.95) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x=estimate,
             xmin=conf.low,
             xmax=conf.high,
             y=term)) +
  geom_pointrange()+
  geom_vline(xintercept = 0, linetype = "dashed")

# Categorical predictor ---------------------------------------------------
m3 = lm(life_exp ~ di_cat, data=countries)
summary(m3)

m4 = lm(life_exp ~ 0 + di_cat, data=countries)
summary(m4)

plot_predictions(m3, condition="di_cat")
plot_predictions(m4, condition="di_cat")#same

levels(countries$di_cat)
fct_rev(countries$di_cat) %>% levels()

## Prevadeni mezi kodovanim
avg_predictions(m3, variables = "di_cat")

avg_comparisons(m4, variables = list(di_cat = "reference"))
avg_comparisons(m4, variables = list(di_cat = "sequential"))
avg_comparisons(m4, variables = list(di_cat = "pairwise"))

# Individualni prace ------------------------------------------------------
un %>% select(infantMortality, region, contraception)
un_m1 = lm(infantMortality ~ region, data=un)
summary(un_m1)
avg_predictions(un_m1, variables="region")
plot_predictions(un_m1, condition="region")

avg_comparisons(un_m1, variables = list(region = "pairwise")) %>% 
  arrange(desc(abs(estimate))) %>% 
  ggplot(aes(x=estimate,
             xmin=conf.low,
             xmax=conf.high,
             y=contrast)) +
  geom_pointrange()

un_m2 = lm(infantMortality ~ region + contraception, data=un)
summary(un_m2)
avg_comparisons(un_m2, variables = list(region="pairwise")) %>% 
  arrange(desc(abs(estimate)))

#Bonus
un %>% 
  ggplot(aes(x=region,
             y=contraception))+
  geom_boxplot()+
  geom_jitter(height = 0, width = 0.2)

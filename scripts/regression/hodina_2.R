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

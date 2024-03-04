library(tidyverse)
library(marginaleffects)

countries <- readr::read_rds("https://github.com/Sociology-FA-CU/applied-regression-r/raw/main/data/countries.rds")
teachers <- readr::read_csv("https://github.com/Sociology-FA-CU/applied-regression-r/raw/main/data/teacher-ratings.csv")


# Interakce ---------------------------------------------------------------

m1 = lm(life_exp ~ poverty_risk + di_cat, data=countries)
summary(m1)

m2 = lm(life_exp ~ poverty_risk * di_cat, data = countries)
summary(m2)

levels(countries$di_cat)
countries$di_cat = fct_relevel(countries$di_cat, "Hybrid regime", "Flawed democracy", "Full democracy")

plot_predictions(m2, condition = c("life_exp", "di_cat"), conf_level = 0.0001)
avg_slopes(m2, variables = "poverty_risk", by="di_cat")


# Marginalni efekty -------------------------------------------------------

countries_filtered = countries %>% 
  mutate(gdp_pc = gdp / population * 1000) %>% 
  filter(!is.na(gdp_pc))

m3 = lm(life_exp ~ gdp_pc, data = countries_filtered)
plot_predictions(m3, condition = "gdp_pc", points=1)

m4 = lm(life_exp ~ poly(gdp_pc, 2), data=countries_filtered)
plot_predictions(m4, condition = "gdp_pc", points=1)
summary(m4)


##Marinalni efekty pro konktretni hodnotu
newdata_gdp_pc = data.frame(gdp_pc = c(25,50,75))
slopes(m4, variables = "gdp_pc", newdata = newdata_gdp_pc)

##Marginalni efekt pro zemi s prumernym HDP
slopes(m4, variables = "gdp_pc", newdata = "mean")

##Prumerny marginalni efekt napric vsemi zememi
avg_slopes(m4, variables = "gdp_pc")


# Vlastni prace -----------------------------------------------------------

qplot(x=gender,data=teachers)
qplot(x=eval,data=teachers)
qplot(x=beauty,data=teachers)

m5 = lm(eval ~ beauty, data=teachers)
summary(m5)

m6 = lm(eval ~ beauty + gender, data=teachers)
summary(m6)
plot_predictions(m6, condition = c('beauty', "gender"))

m8 = lm(eval ~ beauty * gender, data=teachers)
plot_predictions(m8, condition = c('beauty', "gender"))
avg_slopes(m8, variables = "beauty", by="gender")
avg_slopes(m8, variables = "gender") #prumerny marginalni efekt na hodnoceni

newgadata_teachers = expand_grid(gender=c('male', "female"),
            beauty = c(-1,0,1))

avg_slopes(m8, variables = "gender", by="beauty", newdata = newgadata_teachers)


m7 = lm(eval ~ poly(beauty, 2)*gender, data=teachers)
plot_predictions(m7, condition = c('beauty', "gender"), points = 1)
avg_slopes(m7, variables = "beauty", by="gender")

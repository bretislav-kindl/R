library(tidyverse)
library(marginaleffects)
library(splines)
library(lspline)

turnout <- readr::read_rds("https://github.com/Sociology-FA-CU/applied-regression-r/raw/main/data/vote-2017-parliament.rds")
teachers <- readr::read_csv("https://github.com/Sociology-FA-CU/applied-regression-r/raw/main/data/teacher-ratings.csv")

# Robustni standartni chyba -----------------------------------------------
m1 = lm(infantMortality ~ lspline(GDPperCapita, knots = c(5000,20000,30000)), data=un)
m2 = lm(infantMortality ~ ns(GDPperCapita, df = 3), data=un)

plot(m1, which = 1)
#check_model(m1)

avg_slopes(m2, variables = 'GDPperCapita')
avg_slopes(m2, variables = 'GDPperCapita', vcov = "HC3")

plot_predictions(m2, condition = "GDPperCapita", points = 1)
plot_predictions(m2, condition = "GDPperCapita", points = 1, vcov = "HC3")

#procvic
#1
m3 = lm(vote ~ ns(agea, df = 3), data=turnout)
m4 = lm(vote ~ agea, data=turnout)
plot(m4, which = 1)
plot(m3, which = 1)
plot(m4, which = 2)
plot(m4, which = 3)
plot_predictions(m3, condition = "agea", points = 1)
plot_predictions(m3, condition = "agea", points = 1, vcov = "HC3")
avg_slopes(m3, variables = 'agea')
avg_slopes(m3, variables = 'agea', vcov = "HC3")


# Clustered robust errors -------------------------------------------------
teachers %>% 
  count(prof)
m5 = lm(eval ~ beauty + gender, data= teachers)
avg_slopes(m5, variables = "beauty", vcov = ~prof)
plot_predictions(m5, condition = "beauty", points = 1, vcov = ~prof)

# Bootstrap ---------------------------------------------------------------
avg_slopes(m5, variables = "beauty") %>% 
  inferences(method = "boot", R=500)


library(tidyverse)
library(marginaleffects)
library(splines)

install.packages("lspline")
library(lspline)

turnout <- readr::read_rds("https://github.com/Sociology-FA-CU/applied-regression-r/raw/main/data/vote-2017-parliament.rds")
un <- readr::read_rds("https://github.com/Sociology-FA-CU/applied-regression-r/raw/main/data/united-nations.rds")

# Kategorizace ------------------------------------------------------------
turnout = turnout %>% 
  mutate(age_width = cut_width(agea, width = 15, boundary = 16),
         age_number = cut_number(agea, n=5),
         age_interval = cut_interval(agea, n=5),
         age_manual = case_when(agea <= 30 ~ "30 and younger",
                                agea <= 50 ~ "31 to 50",
                                TRUE ~ "51 and older"))
m1 = lm(vote ~ age_manual, data=turnout)
summary(m1)
plot_predictions(m1, condition = "age_manual")

# Jednoduche polynomy -----------------------------------------------------
m2 = lm(vote ~ poly(agea, 2, raw = TRUE), data=turnout) #klasicky polynom
m2 = lm(vote ~ poly(agea, 2, raw = FALSE), data=turnout) #ortogonalni polynom
summary(m2)
avg_slopes(m2, variables = "agea")
plot_predictions(m2, condition = "agea", points=1)

# Ukoly -------------------------------------------------------------------
#1
un = un %>% mutate(GDPperCapita_interval = cut_interval(GDPperCapita, n=5))
#2
m3 = lm(infantMortality ~ GDPperCapita_interval ,data= un)
plot_predictions(m3, condition = "GDPperCapita_interval")
#3
m4 = lm(infantMortality ~ poly(GDPperCapita, 2, raw = TRUE) ,data= un)
plot_predictions(m4, condition = "GDPperCapita", points=1)
#m5 = lm(infantMortality ~ GDPperCapita ,data= un)
#plot_predictions(m5, condition = "GDPperCapita", points=1)

# Linearni spliny ---------------------------------------------------------
m5 = lm(vote ~ lspline(agea, knots = c(25,50,75)), data=turnout)
summary(m5)
plot_predictions(m5, condition = "agea", points = 1)

# Prirozene spliny --------------------------------------------------------

m6 = lm(vote ~ ns(agea, df = 3), data=turnout)#Pocet uzlu je df-1
plot_predictions(m6, condition = "agea", points = 1)
avg_slopes(m6, variables = "agea")

ns(turnout$agea, df = 3) %>% attr("knots")

#procvic 2
#1
m7 = lm(infantMortality ~ lspline(GDPperCapita, knots = c(5000,20000,30000)), data=un)
plot_predictions(m7, condition = "GDPperCapita", points = 1)
m8 = lm(infantMortality ~ ns(GDPperCapita, df = 3), data=un)
plot_predictions(m8, condition = "GDPperCapita", points = 1)

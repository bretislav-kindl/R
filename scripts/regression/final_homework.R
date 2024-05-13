library(tidyverse)
library(marginaleffects)
library(splines)
library(lspline)
library(see)
library(performance)

polls =  read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/raw_polls.csv")
polls =  subset(polls, methodology %in%  c("IVR", "Live Phone", "Online Panel"))

names(polls)
polls = polls %>% mutate(error = abs(margin_actual - margin_poll))
#error ~ methodology
qplot(x=error,data=polls)
qplot(x=methodology,data=polls)
summary(polls$error)

m1 = lm(error ~ 0+methodology, data=polls)
summary(m1)
plot_predictions(m1, condition = "methodology")
avg_comparisons(m3, variables=list("methodology"="pairwise"))

unique(polls$cycle)
m2 = lm(error ~ methodology * cycle, data=polls)
plot_predictions(m2, condition = c("methodology","cycle"))

avg_comparisons(m2, variables=list("methodology"="pairwise"), newdata = datagrid(cycle = 2020:2024))

#---
unique(polls$time_to_election)
qplot(x=time_to_election,data=polls)

polls %>% select(methodology, time_to_election, cycle) %>% group_by(time_to_election) %>% summarise(n=n())
polls %>% count(methodology, time_to_election, cycle)

m3 = lm(error ~ methodology * poly(time_to_election, 2, raw = TRUE), data=polls)
m3 = lm(error ~ 0 + methodology * ns(time_to_election, df = 3), data=polls)
plot_predictions(m3, condition = c("methodology", "time_to_election"))

plot_predictions(m3, condition = c("time_to_election", "methodology"), vcov = "HC3")

avg_comparisons(m3, variables=list("methodology"="pairwise"))
avg_comparisons(m3, variables=list("methodology"="pairwise"), newdata = datagrid(time_to_election = 50:61))

#---

m5 = lm(error ~ methodology * ns(cycle, df=3) + ns(time_to_election, df=3) + aapor_roper, data=polls)
m5 = lm(error ~ methodology * ns(cycle, df=3) * ns(time_to_election, df=3) * aapor_roper, data=polls)
plot_predictions(m5, condition = c("cycle", "methodology"), vcov = "HC3")
plot_predictions(m5, condition = c("methodology", "cycle"))

avg_comparisons(m5, variables = list("methodology"), newdata = datagrid(cycle = 2024), vcov=~race)
avg_comparisons(m5, variables = list("methodology"="pairwise"), newdata = datagrid(cycle = 2024))

#ve spravnou validitu muzem nanejvyse doufat
check_model(m5, check = c("linearity", "homogeneity", "normality"))
                
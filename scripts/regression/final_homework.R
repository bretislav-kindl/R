library(tidyverse)
library(marginaleffects)
library(splines)
library(lspline)

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
avg_comparisons(m1, variables=list("methodology"="pairwise"))

unique(polls$cycle)
m2 = lm(error ~ methodology * cycle, data=polls)
plot_predictions(m2, condition = c("methodology","cycle"))

#---
unique(polls$time_to_election)
qplot(x=time_to_election,data=polls)

polls %>% select(methodology, time_to_election, cycle) %>% group_by(time_to_election) %>% summarise(n=n())
polls %>% count(methodology, time_to_election, cycle)
#---
m3 = lm(error ~ methodology * time_to_election, data=polls)
plot_predictions(m3, condition = c("methodology","time_to_election"))

m4 = lm(error ~ methodology * time_to_election * cycle, data=polls)
plot_predictions(m4, condition = c("methodology","time_to_election","cycle"))

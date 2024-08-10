#library includes
install.packages(c("tidyverse", "marginaleffects", "lspline", "splines", "see", "performance", "sandwich"))

library(tidyverse)
library(marginaleffects)
library(splines)
library(lspline)
library(see)
library(performance)

#data downloads
#https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-08
ipf_lifts <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")
#VO: zmena prumeru vysledku u lidi, kteri se umistili, ku casu
#VO2: rozdil v prumer_muz - prumer_zena podle veku/vahovky

unique(ipf_lifts$place)
unique(ipf_lifts$equipment)
qplot(x=equipment,data=ipf_lifts)
unique(ipf_lifts$age_class)
qplot(x=age_class,data=ipf_lifts)
unique(ipf_lifts$weight_class_kg)
qplot(x=weight_class_kg,data=ipf_lifts)
unique(ipf_lifts$division)
unique(ipf_lifts$federation)
unique(ipf_lifts$meet_name)
unique(ipf_lifts$event)



#Data cleaning
ipf_lifts$date = as.numeric(as.Date(ipf_lifts$date, "%Y-%m-%d")) 
class(ipf_lifts$date)
is.numeric(ipf_lifts$date)
ipf_lifts = ipf_lifts %>%  filter(!place %in% c("DQ", "DD", "NS", "G") & !is.na(age_class))
ipf_lifts_male = ipf_lifts %>% filter(sex == 'M')
ipf_lifts_female = ipf_lifts %>% filter(sex == 'F')

unique(ipf_lifts_male$weight_class_kg)
qplot(x=weight_class_kg,data=ipf_lifts_male)
unique(ipf_lifts_female$weight_class_kg)
qplot(x=weight_class_kg,data=ipf_lifts_female)

unique(ipf_lifts_male$age_class)
qplot(x=age_class,data=ipf_lifts_male)
unique(ipf_lifts_female$age_class)
qplot(x=age_class,data=ipf_lifts_female)

ipf_lifts = ipf_lifts %>% 
  mutate(weight_class_kg_stand = case_when(
    bodyweight_kg < 52 ~ "52-",
    bodyweight_kg >= 52 & bodyweight_kg <= 60 ~ "52-60",
    bodyweight_kg > 60 & bodyweight_kg <= 68 ~ "61-68",
    bodyweight_kg > 68 & bodyweight_kg <= 76 ~ "69-76",
    bodyweight_kg > 76 & bodyweight_kg <= 84 ~ "77-84",
    bodyweight_kg > 84 & bodyweight_kg <= 90 ~ "85-90",
    bodyweight_kg > 90 ~ "90+",
  ),age_class_stand = case_when(
    age < 18 ~ "18-",
    age >= 18 & age <= 19 ~ "18-19",
    age > 20 & age <= 23 ~ "20-23",
    age > 23 & age <= 29 ~ "24-29",
    age > 29 & age <= 34 ~ "30-34",
    age > 34 & age <= 39 ~ "35-39",
    age > 39 & age <= 44 ~ "40-44",
    age > 44 & age <= 49 ~ "45-49",
    age > 49 & age <= 54 ~ "50-54",
    age > 54 & age <= 59 ~ "55-59",
    age > 59 & age <= 64 ~ "60-64",
    age > 64 & age <= 69 ~ "65-69",
    age > 70 ~ "70+",
  )
)

qplot(x=weight_class_kg_stand,data=ipf_lifts)
qplot(x=age_class_stand,data=ipf_lifts)


ipf_lifts_male_test = ipf_lifts_male %>% filter(date > "2000-01-01")
ipf_lifts_male_test_2 = ipf_lifts_male %>% filter(date > "2018-01-01" & meet_name != "World Games")

weird_data = ipf_lifts_male %>% filter(date == "2018-07-09")
weird_data_2 = ipf_lifts_male %>% filter(date == "2019-07-22")
weird_normal = ipf_lifts_male %>% filter(date == "2018-09-03")

m1 = lm(best3squat_kg ~ ns(date, df=5)+sex+age_class_stand+weight_class_kg_stand+equipment+meet_name, data=ipf_lifts)
plot_predictions(m1, condition = c("date", "sex","weight_class_kg_stand", "age_class_stand"), vcov = "HC3")
plot_predictions(m1, condition = c("date", "sex"), vcov = "HC3")
avg_comparisons(m1, variables = list("sex"="pairwise"))

m2 = lm(best3bench_kg ~ ns(date, df=5)+sex+age_class_stand+weight_class_kg_stand+equipment+meet_name, data=ipf_lifts)
plot_predictions(m2, condition = c("date", "sex","weight_class_kg_stand", "age_class_stand"), vcov = "HC3")
plot_predictions(m2, condition = c("date", "sex"), vcov = "HC3")
avg_comparisons(m2, variables = list("sex"="pairwise"))

m3 = lm(best3deadlift_kg ~ ns(date, df=5)+sex+age_class_stand+weight_class_kg_stand+equipment+meet_name, data=ipf_lifts)
plot_predictions(m3, condition = c("date", "sex","weight_class_kg_stand", "age_class_stand"), vcov = "HC3")
plot_predictions(m3, condition = c("date", "sex"), vcov = "HC3")
avg_comparisons(m3, variables = list("sex"="pairwise"))

m3_slow = lm(best3deadlift_kg ~ 0+ns(date, df=5)*sex*age_class*weight_class_kg, data=ipf_lifts)
plot_predictions(m3, condition = c("date", "date"), vcov = "HC3")

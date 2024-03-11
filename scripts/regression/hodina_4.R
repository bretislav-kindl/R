library(tidyverse)
library(marginaleffects)
library(bayestestR)

teachers <- readr::read_csv("https://github.com/Sociology-FA-CU/applied-regression-r/raw/main/data/teacher-ratings.csv")


# Intervalove odhady ------------------------------------------------------

m1 = lm(eval~beauty+gender,data=teachers)
summary(m1)
avg_slopes(m1, conf_level = 0.95)

m2 = lm(eval~beauty*gender,data=teachers)
avg_slopes(m2, variables = "beauty", by = "gender")
avg_comparisons(m2, variables = "beauty", by= "gender", hypothesis = "pairwise")


# Testovani hypotez -------------------------------------------------------

##Frekventisticke
summary(m1) #Pr(>|t|) hodnota
avg_slopes(m1)


##Bayesian pristup
m0=lm(eval~beauty, data=teachers)
bayesfactor(m0,m1)


# Specificka hypoteza -----------------------------------------------------

m3 = lm(eval~0+gender+beauty, data=teachers)
hypotheses(m3, hypothesis = "gendermale - genderfemale = 0.5")

# Ekvivalencni testovani --------------------------------------------------

#Nulova hypoteza, ze rozdil mezi muzi a zenami v hodnoceni vyuky je vetsi nez 0.5 bodu.
hypotheses(m1, equivalence = c(-0.5, 0.5)) %>% 
  filter(term == "gendermale")#p (Equiv)

#p (Equiv) je mensi nez 0.05, takze zamitame hypotezu, ze rozdil mezi muzi a zenami je vetsi nez
# 0.5 bodu. Tzn. skupiny jsou prakticky ekvivalentni

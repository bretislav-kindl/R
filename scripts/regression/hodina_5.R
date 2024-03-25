library(tidyverse)
library(performance)
library(effectsize)
#installed.packages(c("tidyverse", "performance", "effectsize"))

teachers <- readr::read_csv("https://github.com/Sociology-FA-CU/applied-regression-r/raw/main/data/teacher-ratings.csv")


# Model s jednim prediktorem ----------------------------------------------

m1=lm(eval~beauty, data=teachers)
summary(m1)
anova(m1)
# R-squared Sum Sq:
5.083/(5.083+137.156)
r2(m1, ci=0.95)

# Model s vice prediktory -------------------------------------------------

m2=lm(eval ~ beauty+gender, data=teachers)
summary(m2)
r2(m2, ci=0.95)

anova(m2)
eta_squared(m2)

m2up=lm(eval ~ beauty*gender, data=teachers)
eta_squared(m2up)#zde i vzajemna iterakce mezi znaky


# Porovnavani modelu ------------------------------------------------------

anova(m1,m2)

# Ostatni metriky ---------------------------------------------------------

model_performance(m2)

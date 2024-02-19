install.packages(c("tidyverse", "marginaleffects", "broom"))


# Data and Packages -------------------------------------------------------

library(tidyverse)
library(marginaleffects)
library(broom)

countries <- read.csv("https://github.com/Sociology-FA-CU/applied-regression-r/raw/main/data/countries.csv")
turnout <- read.csv("https://github.com/Sociology-FA-CU/applied-regression-r/raw/main/data/vote-2017-parliament.csv")


# Vytvoreni linerarni regrese ---------------------------------------------
#Predikovat nadeji na doziti pomoci lidi v mateialni deprivaci
m1 = lm(life_exp ~ material_dep, data=countries)

# Sumarizace modelu -------------------------------------------------------
summary(m1)
print(m1)

tidy(m1, conf.int=TRUE) 

# Vizualizace regresnich modelu -------------------------------------------
## Rucne
ggplot(data=countries,
       mapping = aes(x=material_dep,
                     y=life_exp))+
  geom_point()+
  geom_abline(intercept = 82.7, slope=-16.4, color="red")

##marginaleffects
plot_predictions(m1,condition = "material_dep", points = 0.4)


# Predikce ----------------------------------------------------------------
## Jaka je ocekavana nadeje na doziti, u zemi kde 20% lidi zije v materialni deprivaci

##Rucne
82.7+(-16.42)*0.2

##Funkce predict
mat_derpivation1 = data.frame(material_dep=0.2)
predict(m1, newdata = mat_derpivation1)

mat_derpivation2 = data.frame(material_dep = c(0.1,0.2,0.3))
predict(m1, newdata = mat_derpivation2)

## Residua
residuals(m1)


# Cviceni -----------------------------------------------------------------
lm_vote = lm(vote ~ agea, data=turnout)
summary(lm_vote)
print(lm_vote)
residuals(lm_vote)
plot_predictions(lm_vote,condition = "agea", points = 0.3)
predict(lm_vote, newdata = data.frame(agea = c(25,2024-1960,2024-1969)))

turnout = turnout %>% mutate(agea_centered = agea - 42.7)
lm(vote~agea_centered, data = turnout)

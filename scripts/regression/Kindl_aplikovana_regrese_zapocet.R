#library includes
install.packages(c("tidyverse", "marginaleffects", "lspline", "splines", "see", "performance", "sandwich"))

library(tidyverse)
library(marginaleffects)
library(splines)
library(lspline)
library(see)
library(performance)

#data downloads
#https://github.com/rfordatascience/tidytuesday/blob/master/data/2024/2024-05-21/readme.md
emissions <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-21/emissions.csv')

unique(emissions$commodity)
unique(emissions$production_unit)

#VO: Improvement in CO2 production efficiency per commodity (and by entity)

#Commodities by their CO2 production though entire history
emissions %>% group_by(commodity) %>% summarise(n=n(), total_emission = sum(total_emissions_MtCO2e)) %>% arrange(desc(total_emission))


qplot(x=total_emissions_MtCO2e,data=emissions)
qplot(x=commodity,data=emissions)
summary(emissions$total_emissions_MtCO2e)

m1 = lm(total_emissions_MtCO2e ~ 0+commodity*ns(year,df=10), data=emissions)
summary(m1)
plot_predictions(m1, condition = c("year","commodity"), vcov = "HC3")


emissions_bbl = emissions %>% filter(production_unit == "Million bbl/yr")
emissions_bcf = emissions %>% filter(production_unit == "Bcf/yr")
emissions_tonnes = emissions %>% filter(production_unit == "Million tonnes/yr")
emissions_tonnes_co2 = emissions %>% filter(production_unit == "Million Tonnes CO2")



yemission_per_commodity_per_year = emissions %>% group_by(year, commodity) %>% summarise(emission = sum(total_emissions_MtCO2e))
qplot(x=year,data=emission_per_commodity_per_year)

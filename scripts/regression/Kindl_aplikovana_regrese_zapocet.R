#library includes
install.packages(c("tidyverse", "marginaleffects", "lspline"))

library(tidyverse)
library(marginaleffects)
library(lspline)

#data downloads
emissions <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-21/emissions.csv')

unique(emissions$commodity)

#VO: Improvement in CO2 production efficiency per commodity (and by entity)

#Commodities by their CO2 production though entire history
emissions %>% group_by(commodity) %>% summarise(n=n(), total_emission = sum(total_emissions_MtCO2e)) %>% arrange(desc(total_emission))

emission_per_commodity_per_year = emissions %>% group_by(year, commodity) %>% summarise(emission = sum(total_emissions_MtCO2e))
qplot(x=year,data=emission_per_commodity_per_year)

library(tidyverse)

countries = read_csv("data/countries.gnumeric")

# Transformace promennych -------------------------------------------------
## Base R
countries$gdp_milliards = countries$gdp / 1000
countries$gdp_zscore = scale(countries$gdp)

##Mutate()
countries %>% 
  mutate(gdp_milliards = gdp / 1000,
         gdp_zscore = scale(gdp_zscore)) %>% 
  select(country, gdp, gdp_milliards, gdp_zscore) %>% 
  filter(gdp >= mean(gdp, na.rm = TRUE))

## Transformace po skupinach
countries %>% 
  group_by(postsoviet) %>% 
  mutate(gdp_zscore = scale(gdp))#z skory pro zapadani a postsovetske zeme zvlast

## Podminene transformace
countries %>% 
  mutate(gdp_binary = if_else(gdp > mean(gdp, na.rm = TRUE),
                              true = "Above average",
                              false = "Below average")) %>% 
  select(country, gdp, gdp_binary)

countries %>% 
  mutate(gdp_categorical = case_when(gdp_zscore < -1 ~ "Below average",
                                     gdp_zscore <= 0 ~ "Slightly bellow average",
                                     gdp_zscore <= 1 ~ "Slightly above average",
                                     gdp_zscore > 1 ~ "Above average",
                                     .default = "Unknown")) %>% 
  select(country, gdp_zscore, gdp_categorical)

# Radkove operace
# Chceme zjistit maximalni pocet lidi v zemi, kteri jsou ohrozeni chudobou nebo trpi materialni deprivaci
countries %>% 
  rowwise() %>% 
  mutate(poverty_or_dep = sum(poverty_risk, material_dep, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(country, poverty_risk, material_dep, poverty_or_dep) %>% 
  print(n = Inf)

# Sumarizace --------------------------------------------------------------
##Summarise()

countries %>% 
  summarise(life_exp_mean = mean(life_exp, na.rm = TRUE),
            life_exp_median = median(life_exp, na.rm = TRUE),
            life_exp_sd = sd(life_exp, na.rm = TRUE))

countries %>% 
  group_by(postsoviet) %>% 
  summarise(life_exp_mean = mean(life_exp, na.rm = TRUE),
            life_exp_median = median(life_exp, na.rm = TRUE),
            life_exp_sd = sd(life_exp, na.rm = TRUE))


# Procvic -----------------------------------------------------------------

view(countries)

countries %>% 
  mutate(gdp_per_capita = gdp / population) %>% 
  group_by(postsoviet) %>% 
  summarise(gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE)) %>% 
  pivot_wider(names_from = postsoviet,
              values_from = gdp_per_capita) %>% 
  mutate(difference = yes - no)

#Ukol
#1 spocitat prumernou pozici kazde psich plemen v datasetu breed_tanks
#2 kterych 5 plemen je hostoricky nejpopularnejsich? kterych 5 je nejmene popularni

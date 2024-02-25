library(tidyverse)


# Import a export dat -----------------------------------------------------
#pracovni adresar
getwd()
setwd()#nepouzivat

#import dat
countries = read_csv("data/countries.gnumeric")
countries = read_rds("data/countries.rds")

#import z internetu
countries = read_csv("https://raw.githubusercontent.com/Sociology-FA-CU/uvod-do-r/master/data/countries.csv")

#export dat
write_csv(countries, "data/countries-new.csv")


# Prvni pohled na dataset -------------------------------------------------
view(countries)

head(countries)
tail(countries)

summary(countries)
glimpse(countries)

names(countries)
ncol(countries)


# Vyber sloupecku ---------------------------------------------------------
# select()

select(countries, country, life_exp, postsoviet)
countries %>% select(country, life_exp, postsoviet)

countries %>% select(-c(country, life_exp, postsoviet)) #vsechny krome 3 vyjmenovanych

# Pomocne selektory
countries %>% select(country:life_exp) #vsechny on country po life_exp

countries %>% select(where(is.numeric))
countries %>% select(where(is.character))

countries %>% select(contains("_"))
countries %>% select(starts_with("_"))
countries %>% select(ends_with("_"))


# Prejmenovani sloupcu ----------------------------------------------------

countries %>% rename(country_code = code)
countries = countries %>% rename(country_code = code, id = X)


# Poradi sloupcu ----------------------------------------------------------

countries %>% relocate(country, .after = population)
countries %>% relocate(country, .before = population)

countries %>% relocate(where(is.numeric), .after = last_col())


# Ukol --------------------------------------------------------------------

countries_filtred = countries %>% select(code, where(is.numeric), -X)
countries_filtred %>% 
  relocate(contains("_"), .after = last_col()) %>% 
  rename(uni_educated = uni_prc)

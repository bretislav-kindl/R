library(tidyverse)

countries = read_csv("data/countries.gnumeric")


# Filtrovani radku --------------------------------------------------------

#sloupce vybirani - select()
#radky filtruji - filter()
# ==
# <
# <=
# !=
# &


countries %>% filter(postsoviet == "yes")

countries %>% filter(countries %in% c("Germany", "France", "Italy"))


# Retezeni datasetu -------------------------------------------------------

countries %>% slice(1:10)
countries %>% slice_head(n=5)
countries %>% slice_head(prop = 0.1)
countries %>% slice_tail(n=1)

countries %>% slice_max(order_by = life_exp, n=3)
countries %>% slice_min(order_by = life_exp, n=3)


# Poradi radku ------------------------------------------------------------

countries %>% 
  arrange(life_exp) %>% #od nejnizsi po nejvyssi
  select(country, life_exp)

countries %>% 
  arrange(-life_exp) %>% #od nejvyssi po nejnizsi
  select(country, life_exp)

# procvic
countries %>% 
  select(country, life_exp, di_cat) %>% 
  filter(country != "Liechtenstein") %>% 
  group_by(di_cat) %>% 
  slice_max(order_by = life_exp, n=2) %>% 
  write_csv("data/life_exp_democracy.csv")


# Siroky a dlouhy format --------------------------------------------------
## Dlouhy format
countries %>% 
  select(country, where(is.numeric)) %>% 
  pivot_longer(cols = -country,
               names_to = "variable",
               values_to = "max_value")

## Siroky format
countries %>% 
  select(eu_member, maj_belief, poverty_risk) %>% 
  group_by(eu_member, maj_belief) %>% 
  slice_min(poverty_risk) %>% 
  pivot_wider(names_from = maj_belief, values_from = poverty_risk)
         

# Spojovani datasetu ------------------------------------------------------

breed_traits <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
breed_ranks <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

view(breed_traits)


breed_left = breed_traits %>% left_join(breed_ranks, by="Breed")
#right_join
#inner_join
#full_join
view(breed_left)

head(breed_ranks)
head(breed_traits)

names(breed_ranks)
names(breed_traits)
Encoding(breed_ranks$Breed)
Encoding(breed_traits$Breed)
Encoding(breed_ranks$Breed) = "UTF-8"
Encoding(breed_traits$Breed) = "UTF-8"

#kontrola encoding
iconv(breed_ranks$Breed[[1]], toRaw = TRUE)
iconv(breed_traits$Breed[[1]], toRaw = TRUE)

#sjednoceni encoding
breed_ranks$Breed_ascii = iconv(breed_ranks$Breed, to = "ASCII")
breed_traits$Breed_ascii = iconv(breed_traits$Breed, to = "ASCII")

breed_ranks %>% select(Breed, Breed_ascii)
breed_traits %>% select(Breed, Breed_ascii)

left_join(breed_ranks, breed_traits, by= "Breed_ascii") %>% view()
#funkcni fix pro kodovani
library(stringi)
breed_ranks$Breed = stri_trans_general(breed_ranks$Breed, "ASCII")
breed_traits$Breed = stri_trans_general(breed_traits$Breed, "ASCII")

breed_ranks %>% 
  left_join(breed_traits, by = "Breed") %>% 
  view()

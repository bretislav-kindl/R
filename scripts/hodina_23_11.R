install.packages("colourpicker")
library(tidyverse)
library(RColorBrewer)
library(scales)

countries = read_csv("data/countries.gnumeric")


#scales jsou dimenze grafu - osy, barva, velikost
#geom jsou objekty reprezentující data v grafu - sloupce ve slouocovém grafu, body v bodovém 
# themes kontrolují estetickou stránku grafu - velikost, font písma


# logika balicku ggplot2 --------------------------------------------------

qplot() #rychle, utilitární grafy 

ggplot(data = countries, 
       mapping = aes(x = hdi,
                     y = life_exp)) + 
  geom_point() + 
  theme(panel.background = element_rect(fill = "orange"))


# Vizualizace kategorialnich promennych -----------------------------------

#Sloupcovy graf
countries %>% 
  group_by(postsoviet) %>% 
  summarise(n=n())

countries %>% 
  count(postsoviet) %>% 
  ggplot(mapping = aes(x=postsoviet, y=n))+
  geom_col()

countries %>% 
  count(postsoviet) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(mapping = aes(x=postsoviet, y=prop))+
  geom_col()

countries %>% 
  count(postsoviet) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(mapping = aes(x=postsoviet,
                       y=prop,
                       label=percent(prop, accuracy=1)))+
  geom_col()+
  geom_text(nudge_y = 0.01)

## Vice kategorialnich promennych
countries %>% 
  count(postsoviet, eu_member) %>% 
  ggplot(mapping = aes(x = postsoviet,
                       y = n,
                       fill = eu_member))+
  geom_col(position = "fill")#position={"stack"|"dodge"|"fill"}

countries %>% 
  count(postsoviet, eu_member) %>% 
  ggplot(mapping = aes(x = postsoviet,
                       y = n,
                       fill = eu_member,
                       label = n)) +
  geom_col(position = "dodge") +
  geom_text(position=position_dodge(width = 1), vjust=-0.05)


# Vizualizace numerickych promennych --------------------------------------
ggplot(data = countries,
       mapping = aes(x=life_exp))+
  geom_histogram()

# pocet binu/pocet sloupecku
ggplot(data = countries,
       mapping = aes(x=life_exp))+
  geom_histogram(bins = 15)

ggplot(data = countries,
       mapping = aes(x=life_exp))+
  geom_histogram(binwidth = 2, color="white", fill="purple")

#boxplot
ggplot(data = countries,
       mapping = aes(x=life_exp)) +
  geom_boxplot()

ggplot(data = countries,
       mapping = aes(y=life_exp)) +
  geom_boxplot()

#Density plot
ggplot(data = countries,
       mapping = aes(x=life_exp)) + 
  geom_density(fill="orange", bw=0.25)

## Scatterplot

ggplot(data = countries,
       mapping = aes(x=hdi,
                     y=life_exp)) +
  geom_point()

## Regression live/curve
ggplot(data = countries,
       mapping = aes(x=hdi,
                     y=life_exp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Procvic
#1)
countries %>% 
  count(maj_belief) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(mapping = aes(x=maj_belief,
                       y=prop,
                       label=percent(prop, accuracy=1)))+
  geom_col()+
  geom_text(nudge_y = 0.01)
#2)
ggplot(data = countries,
       mapping = aes(x=uni_prc,
                     y=material_dep)) +
  geom_point()
#3)
ggplot(data = countries,
       mapping = aes(x=uni_prc,
                     y=material_dep)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#4)
countries %>% 
  count(postsoviet, eu_member) %>% 
  ggplot(mapping = aes(x = postsoviet,
                       y = n,
                       fill = eu_member,
                       label = n)) +
  geom_col(position = "dodge") +
  geom_text(position=position_dodge(width = 1), vjust=-0.05)


# Vizualizace kategorickych a numerickych promennych ----------------------
ggplot(data=countries,
       mapping = aes(y=life_exp, x=postsoviet))+
  geom_boxplot() +
  geom_jitter(height = 0, width = 0.1)
#geom_hex()

ggplot(data=countries,
       mapping = aes(x=hdi, y=life_exp, color=postsoviet))+
  geom_point()

ggplot(data=countries,
       mapping = aes(x=hdi, y=life_exp, color=postsoviet, 
                     shape=eu_member, size=gdp, alpha=poverty_risk))+
  geom_point()

ggplot(data=countries,
       mapping = aes(x=hdi, y=life_exp, color=postsoviet))+
  geom_point() +
  geom_smooth(method="lm")

ggplot(data=countries,
       mapping = aes(x=hdi, y=life_exp, color=postsoviet, group=1))+
  geom_point() +
  geom_smooth(method="lm")


ggplot(data=countries,
       mapping = aes(x=life_exp, fill=postsoviet)) +
  geom_histogram()

ggplot(data=countries,
       mapping = aes(x=life_exp, fill=postsoviet))+
  geom_density(alpha=0.5)

# Facety ------------------------------------------------------------------
#(small multiples)
ggplot(data=countries, 
       mapping = aes(x=hdi,y=life_exp))+
  geom_point() +
  facet_wrap(~postsoviet)

ggplot(data=countries, 
       mapping = aes(x=hdi,y=life_exp))+
  geom_point()+
  facet_wrap(~postsoviet + maj_belief)

ggplot(data=countries, 
       mapping = aes(x=hdi,y=life_exp))+
  geom_point()+
  facet_grid(postsoviet ~ maj_belief)

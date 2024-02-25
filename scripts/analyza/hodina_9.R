library(tidyverse)
countries = read_csv("data/countries.gnumeric")

systemfonts::system_fonts() %>% view()

ggplot(x = maj_belief, data = countries)+
  theme(text = element_text(family = "Times New Roman"))

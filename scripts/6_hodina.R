library(tidyverse)
countries = read_csv("data/countries.gnumeric")

##Transformace urovni
countries$maj_belief = fct_recode(countries$maj_belief,
                                  "protestant" = "protestantism")
qplot(x = maj_belief, data=countries)

##Transformace programticky
countries$maj_belief = fct_relabel(countries$maj_belief, str_to_upper)
qplot(x = maj_belief, data=countries)

##Slucovani katerogii
countries$maj_belief = fct_collapse(countries$maj_belief,
                                    "Christian" = c("CATHOLIC", "PROTESTANT", "ORTHODOX"))
qplot(x = maj_belief, data=countries)


##Slucovani kategorii podle poctu
countries$maj_belief = fct_lump(countries$maj_belief,
                                n = 3,
                                other_level = "Other")
#nebo podle relativni cetnosti
countries$maj_belief = fct_lump(countries$maj_belief,
                                prop = 0.2,
                                other_level = "Other")
qplot(x = maj_belief, data=countries)


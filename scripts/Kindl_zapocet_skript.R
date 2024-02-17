#Břetislav Kindl - zápočet R

# Knihovny ----------------------------------------------------------------

library(tidyverse)
library(scales)
library(RColorBrewer)

# Data --------------------------------------------------------------------

cvvm <- read_rds("data/cvvm_cerven_2019.rds")
names(cvvm)

# Sestavovani df ------------------------------------------------------

# Df hrozeb ----------------------------------------
#vyber odpovedi z otazek - hrozeb
cvvm_danger = cvvm %>% select(matches("PO_1((0\\d)|(11))."))
#kontrola vybranych sloupcu
cvvm_danger %>% names()
#kontrola tabulkou hodnoty
cvvm_danger %>% select("PO_109A")  %>% table()
#rekodovani hodnot
cvvm_danger = cvvm_danger %>%
  mutate(across(
    everything(),
    ~ recode(
      .x,
      'naprosto žádná hrozba' = "0",
      "naprosto zásadní hrozba" = "10"
    )
  )) %>%
  mutate(across(everything(),
                ~ strtoi(.x)))
#vypocet mean, sd a poctu "nevi" pro hrozby
cvvm_danger_index = cvvm_danger %>% pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    dont_know = sum(is.na(value)),
    n = n(),
    dont_know_per = dont_know / n
  ) 
#serazeni df podle prumeru
cvvm_danger_index = cvvm_danger_index %>% arrange(desc(mean))
#pripadni popisu pro jednotlive promenne
cvvm_danger_index$label = c('Dlouhodobé výkyvy počasí, např. dlouhodobé sucho, dlouhodobě extrémně vysoké nebo nízké teploty apod.',
                                    'Stárnutí populace',
                                    'Prohlubování ekonomických rozdílů mezi skupinami obyvatel',
                                    'Přírodní katastrofy, např. povodeň, větrná smršť, rozsáhlé požáry atd.',
                                    'Manipulace s informacemi v soukromých médiích',
                                    'Prohlubování názorových rozdílů mezi skupinami obyvatel',
                                    'Manipulace s informacemi ve veřejnoprávních médiích, tedy v České televizi nebo Českém rozhlase',
                                    'Nárůst chudoby ',
                                    'Technologická závislost státu na nadnárodních společnostech jako jsou Huawei, Facebook, Google apod.',
                                    'Šíření konspiračních teorií a dezinformací po internetu',
                                    'Teroristický útok na místě s vysokým počtem osob',
                                    'Kybernetický, počítačový útok',
                                    'Masová migrace',
                                    'Účast extremistických politických stran ve vládě',
                                    'Energetická či jiná hospodářská závislost na nepřátelském státu',
                                    'Únik nebezpečných chemických či radioaktivních látek do prostředí',
                                    'Uchvácení státní moci ze strany úzké skupiny osob ',
                                    'Účast politických stran prosazujících zájmy nepřátelského státu ve vládě ',
                                    'Dlouhodobý nedostatek potravin či pitné vody ',
                                    'Dlouhodobý nedostatek ropy či plynu',
                                    'Epidemie',
                                    'Krach bankovního sektoru',
                                    'Rozsáhlý a dlouhodobý výpadek dodávek elektrické energie',
                                    'Dlouhodobý výpadek internetu, mobilních sítí nebo telefonu',
                                    'Rabování a výtržnosti',
                                    'Válečný konflikt')
#finalni select podstatnych sloupcu
cvvm_danger_index = cvvm_danger_index %>% select(name, mean, sd, dont_know_per, label)

#finalni vypis sestaveneho df
print(cvvm_danger_index, n=count(cvvm_danger_index))


# Df pripravenost ---------------------------------------------------------
#vyber odpovedi z otazek - pripravenost
cvvm_readiness = cvvm %>% select(matches("PO_1((12)|(10))."))
#kontrola vybranych sloupcu
cvvm_readiness %>% names()
#kontrola tabulkou frekvenci hodnoty
cvvm_readiness %>% select("PO_110A")  %>% table()
#rekodovani hodnot
cvvm_readiness = cvvm_readiness %>%
  mutate(across(
    everything(),
    ~ recode(
      .x,
      'nejsme vůbec připraveni' = "0",
      "jsme výborně připraveni" = "10"
    )
  )) %>%
  mutate(across(everything(),
                ~ strtoi(.x)))
#vypocet mean, sd a poctu "nevi" pro hrozby
cvvm_readiness_index = cvvm_readiness %>% pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    dont_know = sum(is.na(value)),
    n = n(),
    dont_know_per = dont_know / n
  ) 
#serazeni df podle prumeru
cvvm_readiness_index = cvvm_readiness_index %>% arrange(desc(mean))
#pripadni popisu pro jednotlive promenne
cvvm_readiness_index$label = c('Epidemie',
                                    'Přírodní katastrofy, např. povodeň, větrná smršť, rozsáhlé požáry atd.',
                                    'Únik nebezpečných chemických či radioaktivních látek do prostředí',
                                    'Rabování a výtržnosti',
                                    'Účast extremistických politických stran ve vládě',
                                    'Účast politických stran prosazujících zájmy nepřátelského státu ve vládě ',
                                    'Rozsáhlý a dlouhodobý výpadek dodávek elektrické energie',
                                    'Energetická či jiná hospodářská závislost na nepřátelském státu',
                                    'Dlouhodobý nedostatek ropy či plynu',
                                    'Dlouhodobý nedostatek potravin či pitné vody ',
                                    'Uchvácení státní moci ze strany úzké skupiny osob ',
                                    'Kybernetický, počítačový útok',
                                    'Technologická závislost státu na nadnárodních společnostech jako jsou Huawei, Facebook, Google apod.',
                                    'Krach bankovního sektoru',
                                    'Šíření konspiračních teorií a dezinformací po internetu',
                                    'Prohlubování názorových rozdílů mezi skupinami obyvatel',
                                    'Dlouhodobý výpadek internetu, mobilních sítí nebo telefonu',
                                    'Teroristický útok na místě s vysokým počtem osob',
                                    'Manipulace s informacemi ve veřejnoprávních médiích, tedy v České televizi nebo Českém rozhlase',
                                    'Manipulace s informacemi v soukromých médiích',
                                    'Nárůst chudoby',
                                    'Prohlubování ekonomických rozdílů mezi skupinami obyvatel',
                                    'Masová migrace',
                                    'Stárnutí populace',
                                    'Dlouhodobé výkyvy počasí, např. dlouhodobé sucho, dlouhodobě extrémně vysoké nebo nízké teploty apod.',
                                    'Válečný konflikt')
#finalni select podstatnych sloupcu
cvvm_readiness_index = cvvm_readiness_index %>% select(name, mean, sd, dont_know_per, label)
#finalni vypis sestaveneho df
print(cvvm_readiness_index, n=count(cvvm_readiness_index))


# Generovani grafu --------------------------------------------------------

#Filtrace dat

#Generovani souboru

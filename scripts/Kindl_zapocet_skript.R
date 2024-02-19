#Břetislav Kindl - zápočet R

# Knihovny ----------------------------------------------------------------

library(tidyverse)
library(scales)
library(RColorBrewer)

# Data --------------------------------------------------------------------

cvvm <- read_rds("data/cvvm_cerven_2019.rds")
names(cvvm)
#dataframe se sloupci, se kterymi budem pracovat
cvvm_filtred = cvvm %>% select(matches("PO_1[10]\\d."))
cvvm_filtred %>% names()

# Sestavovani df ------------------------------------------------------
#definovani a pridani labels
labels = data.frame(name = cvvm_filtred %>% names(), label = 0)
labels %>% pivot_wider(names_from = name, values_from = label)
labels$label[which(labels$name == "PO_109A" | labels$name == "PO_110A")] = 'Přírodní katastrofy, např. povodeň, větrná smršť, rozsáhlé požáry atd.'
labels$label[which(labels$name == "PO_109B" | labels$name == "PO_110B")] = 'Epidemie'
labels$label[which(labels$name == "PO_109C" | labels$name == "PO_110C")] = 'Dlouhodobé výkyvy počasí, např. dlouhodobé sucho, dlouhodobě extrémně vysoké nebo nízké teploty apod.'
labels$label[which(labels$name == "PO_109D" | labels$name == "PO_110D")] = 'Únik nebezpečných chemických či radioaktivních látek do prostředí'
labels$label[which(labels$name == "PO_109E" | labels$name == "PO_110E")] = 'Dlouhodobý nedostatek potravin či pitné vody'
labels$label[which(labels$name == "PO_109F" | labels$name == "PO_110F")] = 'Rozsáhlý a dlouhodobý výpadek dodávek elektrické energie'
labels$label[which(labels$name == "PO_109G" | labels$name == "PO_110G")] = 'Dlouhodobý nedostatek ropy či plynu'
labels$label[which(labels$name == "PO_109H" | labels$name == "PO_110H")] = 'Dlouhodobý výpadek internetu, mobilních sítí nebo telefonu'
labels$label[which(labels$name == "PO_109I" | labels$name == "PO_110I")] = 'Kybernetický, počítačový útok'
labels$label[which(labels$name == "PO_109J" | labels$name == "PO_110J")] = 'Teroristický útok na místě s vysokým počtem osob'
labels$label[which(labels$name == "PO_109K" | labels$name == "PO_110K")] = 'Válečný konflikt'
labels$label[which(labels$name == "PO_109L" | labels$name == "PO_110L")] = 'Rabování a výtržnosti'
labels$label[which(labels$name == "PO_109M" | labels$name == "PO_110M")] = 'Masová migrace'
labels$label[which(labels$name == "PO_109N" | labels$name == "PO_110N")] = 'Stárnutí populace'
labels$label[which(labels$name == "PO_109O" | labels$name == "PO_110O")] = 'Nárůst chudoby'
labels$label[which(labels$name == "PO_109P" | labels$name == "PO_110P")] = 'Krach bankovního sektoru'
labels$label[which(labels$name == "PO_109Q" | labels$name == "PO_110Q")] = 'Prohlubování názorových rozdílů mezi skupinami obyvatel'
labels$label[which(labels$name == "PO_111A" | labels$name == "PO_112A")] = 'Šíření konspiračních teorií a dezinformací po internetu'
labels$label[which(labels$name == "PO_111B" | labels$name == "PO_112B")] = 'Manipulace s informacemi ve veřejnoprávních médiích, tedy v České televizi nebo Českém rozhlase'
labels$label[which(labels$name == "PO_111C" | labels$name == "PO_112C")] = 'Manipulace s informacemi v soukromých médiích'
labels$label[which(labels$name == "PO_111D" | labels$name == "PO_112D")] = 'Prohlubování ekonomických rozdílů mezi skupinami obyvatel'
labels$label[which(labels$name == "PO_111E" | labels$name == "PO_112E")] = 'Uchvácení státní moci ze strany úzké skupiny osob'
labels$label[which(labels$name == "PO_111F" | labels$name == "PO_112F")] = 'Účast extremistických politických stran ve vládě'
labels$label[which(labels$name == "PO_111G" | labels$name == "PO_112G")] = 'Účast politických stran prosazujících zájmy nepřátelského státu ve vládě'
labels$label[which(labels$name == "PO_111H" | labels$name == "PO_112H")] = 'Energetická či jiná hospodářská závislost na nepřátelském státu'
labels$label[which(labels$name == "PO_111I" | labels$name == "PO_112I")] = 'Technologická závislost státu na nadnárodních společnostech jako jsou Huawei, Facebook, Google apod.'
labels
# Df hrozeb ----------------------------------------
#vyber odpovedi z otazek - hrozeb
cvvm_danger = cvvm_filtred %>% select(matches("PO_1((0\\d)|(11))."))
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
cvvm_danger_index %>% names()
labels %>% names()
cvvm_danger_index = cvvm_danger_index %>% left_join(labels, by="name")
cvvm_danger_index
#finalni select podstatnych sloupcu
cvvm_danger_index = cvvm_danger_index %>% select(name, mean, sd, dont_know_per, label)

#finalni vypis sestaveneho df
print(cvvm_danger_index, n=count(cvvm_danger_index))


# Df pripravenost ---------------------------------------------------------
#vyber odpovedi z otazek - pripravenost
cvvm_readiness = cvvm_filtred %>% select(matches("PO_1((12)|(10))."))
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
cvvm_readiness_index %>% names()
labels %>% names()
cvvm_readiness_index = cvvm_readiness_index %>% left_join(labels, by="name")
cvvm_readiness_index
#zpusob pridani labels zavysejici na pridani labels podle serazeni promennych
# cvvm_readiness_index$label = c('Epidemie',
#                                     'Přírodní katastrofy, např. povodeň, větrná smršť, rozsáhlé požáry atd.',
#                                     'Únik nebezpečných chemických či radioaktivních látek do prostředí',
#                                     'Rabování a výtržnosti',
#                                     'Účast extremistických politických stran ve vládě',
#                                     'Účast politických stran prosazujících zájmy nepřátelského státu ve vládě ',
#                                     'Rozsáhlý a dlouhodobý výpadek dodávek elektrické energie',
#                                     'Energetická či jiná hospodářská závislost na nepřátelském státu',
#                                     'Dlouhodobý nedostatek ropy či plynu',
#                                     'Dlouhodobý nedostatek potravin či pitné vody ',
#                                     'Uchvácení státní moci ze strany úzké skupiny osob ',
#                                     'Kybernetický, počítačový útok',
#                                     'Technologická závislost státu na nadnárodních společnostech jako jsou Huawei, Facebook, Google apod.',
#                                     'Krach bankovního sektoru',
#                                     'Šíření konspiračních teorií a dezinformací po internetu',
#                                     'Prohlubování názorových rozdílů mezi skupinami obyvatel',
#                                     'Dlouhodobý výpadek internetu, mobilních sítí nebo telefonu',
#                                     'Teroristický útok na místě s vysokým počtem osob',
#                                     'Manipulace s informacemi ve veřejnoprávních médiích, tedy v České televizi nebo Českém rozhlase',
#                                     'Manipulace s informacemi v soukromých médiích',
#                                     'Nárůst chudoby',
#                                     'Prohlubování ekonomických rozdílů mezi skupinami obyvatel',
#                                     'Masová migrace',
#                                     'Stárnutí populace',
#                                     'Dlouhodobé výkyvy počasí, např. dlouhodobé sucho, dlouhodobě extrémně vysoké nebo nízké teploty apod.',
#                                     'Válečný konflikt')
#finalni select podstatnych sloupcu
cvvm_readiness_index = cvvm_readiness_index %>% select(name, mean, sd, dont_know_per, label)
cvvm_readiness_index %>% mutate(across(c(mean, sd, dont_know_per),round,2))
#finalni vypis sestaveneho df
print(cvvm_readiness_index, n=count(cvvm_readiness_index))


# Generovani grafu --------------------------------------------------------

#Filtrace dat

#Generovani souboru

#Břetislav Kindl - zápočet R

# Knihovny ----------------------------------------------------------------

library(tidyverse)
library(scales)
library(RColorBrewer)
library(ggrepel)

# Data --------------------------------------------------------------------

cvvm <- read_rds("data/cvvm_cerven_2019.rds")
names(cvvm)
#dataframe se sloupci, se kterymi budem pracovat
cvvm_filtred = cvvm %>% select(matches("PO_1[10]\\d."))
#cvvm_filtred %>% names()

# Sestavovani df ------------------------------------------------------
#definovani a pridani labels
labels = data.frame(name = cvvm_filtred %>% names(), label = 0)
#labels %>% pivot_wider(names_from = name, values_from = label)
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
#labels
#vyber odpovedi z otazek
cvvm_danger = cvvm_filtred %>% select(matches("PO_1((0\\d)|(11))."))
cvvm_readiness = cvvm_filtred %>% select(matches("PO_1((12)|(10))."))

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

#spojeni data framu pro spolecne transforamce
df_danger_and_readiness = cbind(cvvm_danger, cvvm_readiness)
df_danger_and_readiness_index = df_danger_and_readiness %>% pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    dont_know = sum(is.na(value)),
    n = n(),
    dont_know_per = dont_know / n
  ) 
#pridani labels
df_danger_and_readiness_index_labeled = df_danger_and_readiness_index %>% left_join(labels, by="name")
#vyber hrozeb
df_danger = df_danger_and_readiness_index_labeled %>% filter(str_detect(name,"PO_1((0\\d)|(11))."))
df_danger_final = df_danger %>% 
  select(name, mean, sd, dont_know_per, label) %>% #vyber pozadovanych sloupcu
  arrange(desc(mean)) %>% #serazeni
  mutate(across(c(mean, sd),round,1), dont_know_per= round(dont_know_per,2)) #zaokrouhleni
#finalni vypis hrozeb
print(df_danger_final, n=count(df_danger_final))
#vyber pripravenosti
df_readiness = df_danger_and_readiness_index_labeled %>% filter(str_detect(name,"PO_1((12)|(10))."))
df_readiness_final = df_readiness %>% 
  select(name, mean, sd, dont_know_per, label) %>% #vyber pozadovanych sloupcu
  arrange(desc(mean)) %>% #serazeni
  mutate(across(c(mean, sd),round,1), dont_know_per= round(dont_know_per,2)) #zaokrouhleni
#finalni vypis pripravenosti
print(df_readiness_final, n=count(df_readiness_final))


# Generovani grafu --------------------------------------------------------
#definovani labels pro grafy
labels_graph = data.frame(name = cvvm_filtred %>% names(), label = 0)
labels_graph %>% pivot_wider(names_from = name, values_from = label)
labels_graph$label[which(labels_graph$name == "PO_109A" | labels_graph$name == "PO_110A")] = 'Přírodní katastrofy'
labels_graph$label[which(labels_graph$name == "PO_109B" | labels_graph$name == "PO_110B")] = 'Epidemie'
labels_graph$label[which(labels_graph$name == "PO_109C" | labels_graph$name == "PO_110C")] = 'Dlouhodobé výkyvy počasí'
labels_graph$label[which(labels_graph$name == "PO_109D" | labels_graph$name == "PO_110D")] = 'Únik nebezpečných chemických/ radioaktivních látek'
labels_graph$label[which(labels_graph$name == "PO_109E" | labels_graph$name == "PO_110E")] = 'Nedostatek potravin/ pitné vody'
labels_graph$label[which(labels_graph$name == "PO_109F" | labels_graph$name == "PO_110F")] = 'Výpadek elektrické energie'
labels_graph$label[which(labels_graph$name == "PO_109G" | labels_graph$name == "PO_110G")] = 'Nedostatek ropy či plynu'
labels_graph$label[which(labels_graph$name == "PO_109H" | labels_graph$name == "PO_110H")] = 'Výpadek internetu, mobilních sítí nebo telefonu'
labels_graph$label[which(labels_graph$name == "PO_109I" | labels_graph$name == "PO_110I")] = 'Kybernetický útok'
labels_graph$label[which(labels_graph$name == "PO_109J" | labels_graph$name == "PO_110J")] = 'Teroristický útok'
labels_graph$label[which(labels_graph$name == "PO_109K" | labels_graph$name == "PO_110K")] = 'Válečný konflikt'
labels_graph$label[which(labels_graph$name == "PO_109L" | labels_graph$name == "PO_110L")] = 'Rabování a výtržnosti'
labels_graph$label[which(labels_graph$name == "PO_109M" | labels_graph$name == "PO_110M")] = 'Masová migrace'
labels_graph$label[which(labels_graph$name == "PO_109N" | labels_graph$name == "PO_110N")] = 'Stárnutí populace'
labels_graph$label[which(labels_graph$name == "PO_109O" | labels_graph$name == "PO_110O")] = 'Nárůst chudoby'
labels_graph$label[which(labels_graph$name == "PO_109P" | labels_graph$name == "PO_110P")] = 'Krach bankovního sektoru'
labels_graph$label[which(labels_graph$name == "PO_109Q" | labels_graph$name == "PO_110Q")] = 'Prohlubování názorových rozdílů'
labels_graph$label[which(labels_graph$name == "PO_111A" | labels_graph$name == "PO_112A")] = 'Šíření dezinformací po internetu'
labels_graph$label[which(labels_graph$name == "PO_111B" | labels_graph$name == "PO_112B")] = 'Manipulace ve veřejnoprávních médiích'
labels_graph$label[which(labels_graph$name == "PO_111C" | labels_graph$name == "PO_112C")] = 'Manipulace v soukromých médiích'
labels_graph$label[which(labels_graph$name == "PO_111D" | labels_graph$name == "PO_112D")] = 'Prohlubování ekonomických rozdílů'
labels_graph$label[which(labels_graph$name == "PO_111E" | labels_graph$name == "PO_112E")] = 'Uchvácení státní moci ze strany úzké skupiny osob'
labels_graph$label[which(labels_graph$name == "PO_111F" | labels_graph$name == "PO_112F")] = 'Účast extremistických politických stran ve vládě'
labels_graph$label[which(labels_graph$name == "PO_111G" | labels_graph$name == "PO_112G")] = 'Účast politických stran prosazujících zájmy nepřátelského státu ve vládě'
labels_graph$label[which(labels_graph$name == "PO_111H" | labels_graph$name == "PO_112H")] = 'Hospodářská/energetická závislost na nepřátelském státu'
labels_graph$label[which(labels_graph$name == "PO_111I" | labels_graph$name == "PO_112I")] = 'Technologická závislost státu na nadnárodních společnostech'
labels_graph

#Priprava data framu
df_danger_and_readiness_index_graph_labels = df_danger_and_readiness_index %>% 
  left_join(labels_graph, by="name") %>% 
  select(name, mean, label)
#rozdeleni na hrozby a pripravenost
df_danger_graph = df_danger_and_readiness_index_graph_labels %>% filter(str_detect(name,"PO_1((0\\d)|(11))."))
df_rediness_graph = df_danger_and_readiness_index_graph_labels %>% filter(str_detect(name,"PO_1((12)|(10))."))
#mergnuti podle labels
df_graph_merge = merge(df_danger_graph, df_rediness_graph, by = "label")
#prirazeni kategorii
df_graph_merge$category = 0
df_graph_category_1 = df_graph_merge %>% filter(str_detect(name.x,"PO_109[ABCDE]")) %>% mutate(category = 'Přírodní hrozby')
df_graph_category_2 = df_graph_merge %>% filter(str_detect(name.x,"PO_109[FGHI]")) %>% mutate(category = 'Infrastrukturní hrozby')
df_graph_category_3 = df_graph_merge %>% filter(str_detect(name.x,"PO_109[JKLM]")) %>% mutate(category = 'Konfliktní hrozby')
df_graph_category_4 = df_graph_merge %>% filter(str_detect(name.x,"PO_111[ABCD]")) %>% mutate(category = 'Informační hrozby')
df_graph_category_5 = df_graph_merge %>% filter(str_detect(name.x,"PO_111[EFGHI]")) %>% mutate(category = 'Politické hrozby')
df_graph_category_6 = df_graph_merge %>% filter(str_detect(name.x,"PO_109[NOPQ]")) %>% mutate(category = 'Sociální hrozby')
df_graph_merge_categories = rbind(df_graph_category_1, df_graph_category_2,df_graph_category_3,df_graph_category_4, df_graph_category_5, df_graph_category_6)
df_graph_merge_categories 

#graf
df_graph_merge_categories$category = factor(df_graph_merge_categories$category, levels = c('Přírodní hrozby', 'Infrastrukturní hrozby', 'Konfliktní hrozby', 'Informační hrozby', 'Politické hrozby', 'Sociální hrozby'))
df_graph_merge_categories %>%
  ggplot(mapping = aes(x = mean.x, y = mean.y, color=category, label = label, group = 1)) +
  geom_point(size = 6) +
  geom_abline(slope=1, color="black", linetype = "dashed", size = 1) + 
  #geom_text(color="black", check_overlap = T)+
  geom_text_repel(color="black", hjust=-0.1, vjust=0.3) +
  #geom_text(color="black", hjust=-0.1, vjust=0.3) +
  scale_y_continuous(limits = c(3.8,4.9), n.breaks = 12, expand = c(0, 0)) +
  scale_x_continuous(limits = c(4,7), n.breaks = 7, expand = c(0, 0)) +
  scale_color_manual(values=c("#538235","#ec7c30","#b6b6b6", "#ffc000", "#89a6da", "#7e5f00")) +
  labs(
    x = "Míra vnímaného ohrožení",
    y = "Míra vnímané připravenosti",
    title = "Graf 1: Vztah míry vnímaného ohrožení a připravenosti na vybrané hrozby (průměrné hodnoty na škále 0 – 10) ",
    caption = "Pozn.: Přerušovaná čára značí přechod, nad kterým je míra vnímané připravenost vyšší, než míra vnímaného ohrožení.\nPozn. 2: Znění položek je pro účely grafu kráceno.\nZdroj: CVVM SOÚ AV ČR, Naše společnost, 8. – 17. 6. 2019, 1024 respondentů starších 15 let, osobní rozhovor.",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0),
        panel.grid.major = element_line(color="#d8d8d8", linetype="solid"),
        panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(nrow = 1))
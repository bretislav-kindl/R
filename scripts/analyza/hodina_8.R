library(tidyverse)
library(scales)
library(RColorBrewer)

countries = read_csv("data/countries.gnumeric")

# vzhled grafu
dem_countries <- countries %>% 
  count(di_cat) %>% 
  mutate(prop = n/sum(n),
         di_cat = fct_relevel(di_cat,
                              "Full democracy",
                              "Flawed democracy",
                              "Hybrid regime"))
ggplot(data = dem_countries,
       mapping = aes(x = di_cat,
                     y = prop)) + 
  geom_col()

c("#7b33ab","#e1eb61","#ffffff")

ggplot(data = dem_countries,
       mapping = aes(x = di_cat,
                     y = prop)) + 
  geom_col(fill="purple")

## Barevne palety
display.brewer.all()

ggplot(data = dem_countries,
       mapping = aes(x = di_cat,
                     y = prop,
                     fill=di_cat)) + 
  geom_col() +
  scale_fill_brewer(palette="RdYlGn",
                    direction = -1,
                    na.value = "grey80")

ggplot(data = dem_countries,
       mapping = aes(x = di_cat,
                     y = prop,
                     fill=di_cat)) + 
  geom_col(color="black") +
  scale_fill_manual(values=c("green","yellow","red","grey80"))


# Barvy pro spojite promenne ----------------------------------------------

ggplot(data = countries,
       mapping = aes(x = life_exp,
                     y = poverty_risk,
                     color = hdi)) + 
  geom_point() +
  scale_color_continuous(type = "viridis")


ggplot(data = countries,
       mapping = aes(x = life_exp,
                     y = poverty_risk,
                     color = hdi)) + 
  geom_point() +
  scale_color_gradient(low="red",high = "green")


ggplot(data = countries,
       mapping = aes(x = life_exp,
                     y = poverty_risk,
                     color = hdi)) + 
  geom_point() +
  scale_color_gradient2(low="red",mid="yellow", high = "green",midpoint = 0.85)


# Tvar geomu --------------------------------------------------------------

ggplot(data = countries,
       mapping = aes(x = life_exp,
                     y = poverty_risk)) + 
  geom_point(shape=21, color="tomato", fill="blue",
             size = 5, alpha = 0.5)


# Formatovani os ----------------------------------------------------------

ggplot(data = countries,
       mapping = aes(x = life_exp,
                     y = poverty_risk)) + 
  geom_point() +
  scale_x_continuous(limits = c(60,90),
                     labels = number_format(suffix = " let")) +
  scale_y_continuous(breaks = c(0.15,0.3,0.4),
                     labels = percent_format(suffix = " %"))

ggplot(data = dem_countries,
       mapping = aes(x = di_cat,
                     y = prop)) +
  geom_col() +
  scale_x_discrete(labels = str_to_title)

#vypnout vedeckou notaci
options(scipen = 999)


# Popisky grafu -----------------------------------------------------------
plot_scatter = ggplot(data = countries,
       mapping = aes(x = life_exp,
                     y = poverty_risk,
                     color = postsoviet)) +
  geom_point() +
  labs(x = "Naděje na dožití",
       y = "Podíl lidí ohrožených chudobou",
       color = "Je země \npostsovětská?",
       title = "Bohatším lidem se žije déle",
       subtitle = "Data z Eurostatu",
       caption = "Zdroj dat: Eurostat 2018")


# Tematiky ----------------------------------------------------------------
## themes

plot_scatter +
  theme_minimal()

## Vlastni tematiky
plot_scatter +
  theme(legend.position = "bottom")
plot_scatter +
  theme(legend.position = c(0.8,0.8),
        text = element_text(size=12, family = "Times New Roman"),
        panel.background = element_rect(fill="tomato"),
        plot.background = element_rect(fill = "grey"),
        legend.background = element_rect(fill="grey"),
        panel.grid.major.x = element_line(color="black", linetype="dashed"),
        panel.grid = element_blank())

## Voditka
## guides

plot_scatter +
  theme(legend.position = "bottom")+
  guides(color=guide_legend(title.position="top",
                            label.position="bottom",
                            keywidth=5))

#prace
belief_count = countries %>% 
  count(maj_belief)

ggplot(data = belief_count,
       mapping = aes(x = maj_belief, y=n)) +
  geom_col(fill="#ad0909") +
  scale_x_discrete(labels = str_to_title) +
  labs(x = "Most Prevalent Religious Group",
       y = "",
       title = "How Religious is Europe?",
       caption = "Pew Research Center 2019") +
  theme(panel.background = element_rect(fill="white"),
        text = element_text(size=12),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill="grey"),
        panel.grid.major.y = element_line(color="black", linetype="longdash"),
        panel.grid = element_blank())


plot_dem_hdi = ggplot(data = countries,
       mapping = aes(x = hdi, y=dem_index, color=postsoviet, label=code)) +
  geom_point(size = 10) +
  geom_text(color="white")+
  scale_color_brewer(palette="Set2", labels = str_to_title)+
  labs(x = "Human Development Index",
       y = "Democracy Index",
       title = "Link Between Democaracy and Wellbeing?",
       subtitle = "Evidence from 2019",
       caption = "Data Source: United Nations, The Economist Intelligence Unit",
       color = "Is country postsoviet?")+
  scale_x_continuous(labels = percent_format(suffix = " %")) +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_dem_hdi

# Export grafu ------------------------------------------------------------
ggsave(plot=plot_dem_hdi, 
       filename = "plot_dem_hdi.png",
       device = "png",
       path = "plots",
       units = "cm",
       width = 17,
       height = 14,
       dpi = 300)

install.packages("svglite")
ggsave(plot=plot_dem_hdi, 
       filename = "plot_dem_hdi.svg",
       device = "svg",
       path = "plots",
       units = "cm",
       width = 17,
       height = 14)

# Kolacovy graf -----------------------------------------------------------
countries %>% 
  count(postsoviet) %>% 
  ggplot(mapping = aes(x = "",
             y= n,
             fill=postsoviet)) +
  geom_col(position = "fill") +
  coord_polar(theta = "y")

# Dumbell ploty -----------------------------------------------------------
countries %>% 
  select(postsoviet, uni_prc, poverty_risk, material_dep) %>% 
  pivot_longer(cols=-postsoviet) %>% 
  group_by(postsoviet, name) %>% 
  summarise(value=mean(value,na.rm=TRUE)) %>% 
  ggplot(mapping = aes(x = value,
                       y= name,
                       color = postsoviet)) +
  geom_line(mapping = aes(group=name), color="black") +
  geom_point(size=7)


# Fancy facety ------------------------------------------------------------

countries2 = countries %>% 
  filter(!is.na(di_cat), di_cat != "") %>% 
  select(-di_cat)

countries %>% 
  filter(!is.na(di_cat), di_cat != "") %>% 
  ggplot(mapping = aes(x=hdi,y=life_exp)) +
  facet_wrap(~di_cat) +
  geom_point(data=countries2, alpha= 0.1) +
  geom_point(mapping = aes(color=di_cat),
             show.legend = FALSE)



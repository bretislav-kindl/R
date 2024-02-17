library(tidyverse)
library(scales)
library(RColorBrewer)

covid <- read_csv("data/covid.csv")


# Nejvice/nejmene zasazene skupiny ----------------------------------------

# geograficky
covid_geog <- covid %>%
  filter(!is.na(continent))

# top 5 zemi s nejvice total cases per million
covid_geog %>%
  arrange(desc(total_cases_per_million)) %>%
  select(location, total_cases_per_million) %>%
  head(n = 5)

# top 5 zemi s nejvice total deaths per million
covid_geog %>%
  arrange(desc(total_deaths_per_million)) %>%
  select(location, total_deaths_per_million) %>%
  head(n = 5)

covid_geog %>%
  ggplot(mapping = aes(x = total_cases_per_million, y = total_deaths_per_million, color = continent)) +
  geom_point()

covid_geog_fancy <- covid_geog %>%
  filter(!is.na(continent), continent != "") %>%
  select(-continent)

covid_geog %>%
  filter(!is.na(continent), continent != "") %>%
  ggplot(mapping = aes(x = total_cases_per_million, y = total_deaths_per_million)) +
  facet_wrap(~continent) +
  geom_point(data = covid_geog_fancy, alpha = 0.1) +
  geom_point(
    mapping = aes(color = continent),
    show.legend = FALSE
  ) +
  scale_x_continuous(labels = number_format(scale = 0.001, suffix = " tis.")) +
  scale_y_continuous(labels = number_format(scale = 0.001, suffix = " tis.")) +
  labs(
    x = "Celkový počet případů na milion",
    y = "Celkový počet úmrtí na milion",
    title = "Dopad pandemie na kontinetech",
    caption = "Data Source: Our World in Data"
  ) +
  theme_minimal()

# socioekonomicke


# Ucitelova verze ---------------------------------------------------------

covid %>% 
  filter(location %in% c("Africa", "Europe", "North America", "South America", "Asia")) %>% 
  mutate(location = fct_reorder(location, total_cases_per_million,
                                .desc = TRUE)) %>% 
  ggplot(aes(x = location,
             y = total_cases_per_million)) +
  geom_col(fill = "dodgerblue") +
  scale_y_continuous(labels = number_format(scale = 0.00001,
                                            suffix = "k cases per million"))+
  labs(x = element_blank(),
       y = "total cases per million",
       title = "cases per continent",
       subtitle = "Europe most affect. fact or fiction?") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())


# Distribuce vakciny efektivita -------------------------------------------

display.brewer.all()
brewer.pal(n = 3, "RdYlGn")

covid_geog %>%
  ggplot(mapping = aes(x = people_vaccinated_per_hundred, y = human_development_index,shape = continent, size = people_fully_vaccinated, color=total_deaths_per_million)) +
  geom_point() +
  scale_color_gradient(low = "#91CF60", high="#FC8D59", name="Umrtí na milion") +
  scale_shape_discrete(name="Kontinent") +
  scale_size(name="Uplná vakcinace na milion") +
  labs(
    x = "Počet vakcinovaných na sto",
    y = "HDI",
    title = "Efektivita distribuce vakciny",
    caption = "Data Source: Our World in Data"
  ) +
  theme_minimal()


# Ucitelova verze ---------------------------------------------------------

covid %>% 
  ggplot(mapping = aes(x=median_age, y=total_vaccinations_per_hundred))+
  geom_point()+
  geom_smooth(method = "lm")

# Nejdulezitejsi faktory s dopadem epidemie -------------------------------

covid %>% 
  select(total_cases_per_million,
         stringency_index,
         human_development_index,
         hospital_beds_per_thousand) %>% 
  pivot_longer(cols=-total_cases_per_million) %>% 
  ggplot(aes(x=value,
             y=total_cases_per_million))+
  facet_wrap(~name, scales="free_x")+
  geom_point()

covid %>% 
  ggplot(aes(x=gdp_per_capita, y=total_cases_per_million)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()



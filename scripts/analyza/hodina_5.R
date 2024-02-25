library(tidyverse)

breed_traits <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
breed_ranks <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')
countries = read_csv("data/countries.gnumeric")

breed_ranks %>% 
  select(Breed, contains("Rank")) %>% 
  pivot_longer(cols=-Breed,
               names_to = "year",
               values_to = "rank") %>% 
  group_by(Breed) %>% 
  summarise(avg_rank=mean(rank,na.rm = TRUE)) %>% 
  slice_max(avg_rank, n=5)

breed_ranks %>% 
  rowwise() %>% 
  mutate(avg_rank = mean(c_across(contains("Rank")),na.rm = TRUE)) %>% 
  select(Breed, avg_rank) %>% 
  ungroup() %>% 
  slice_min(avg_rank, n=5)


# Transformace a sumarizace vice promenych --------------------------------

countries %>% 
  mutate(postsoviet = as.factor(postsoviet),
         eu_member = as.factor(eu_member),
         maj_belief = as.factor(maj_belief),
         di_cat = as.factor(di_cat)) #neefektivni

countries %>% 
  mutate(across(.cols = c(postsoviet, eu_member, maj_belief, di_cat),.fns= as.factor))#kompaktnejsi

#selection helpers
countries %>% 
  mutate(across(.cols=where(is.numeric),
                .fns=round))

#Tilda notace
countries %>% 
  mutate(across(.cols=where(is.numeric),
                .fns = ~round(., digits = 2)))

#summarizace vice promenych
countries %>% 
  summarise(across(.cols=where(is.numeric),.fns=~mean(., na.rm = TRUE))) %>% 
  pivot_longer(cols=everything(),
               names_to = "variable",
               values_to = "mean")

countries %>% 
  group_by(postsoviet) %>% 
  summarise(across(.cols=where(is.numeric),.fns=~mean(., na.rm = TRUE)))

# Vice nez jedna funkce ---------------------------------------------------
countries %>% 
  summarise(across(.cols=where(is.numeric),
                   .fns=lst(mean, sd, min, max),
                   na.rm = TRUE))

countries %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(cols=everything()) %>% 
  group_by(name) %>% 
  summarise(mean=mean(value, na.rm = TRUE),
            sd=sd(value,na.rm=TRUE),
            min=min(value,na.rm=TRUE),
            max=max(value,na.rm=TRUE))

# Procvic -----------------------------------------------------------------
countries %>% 
  select(where(is.numeric), postsoviet) %>% 
  mutate(across(.cols=where(is.numeric),
                .fns=scale),
         across(.cols=where(is.numeric),
                .fns=as.numeric)) %>% 
  group_by(postsoviet) %>% 
  summarise(across(.cols=where(is.numeric),
                   .fns=~mean(., na.rm = TRUE)))
countries %>% 
  group_by(postsoviet) %>% 
  summarise(across(.cols=where(is.numeric),
                   .fns=mean,
                   na.rm = TRUE)) %>% 
  mutate(across(.cols=where(is.numeric),
                .fns=~round(.,digits=2))) %>% 
  pivot_longer(cols=-postsoviet) %>% 
  pivot_wider(names_from = postsoviet,values_from = value)


# Prace se stringy --------------------------------------------------------
countries %>% 
  select(country, hd_title_name) %>% 
  filter(str_detect(hd_title_name, pattern="King"))

countries %>% 
  select(country, hd_title_name) %>% 
  filter(str_detect(hd_title_name, pattern="King|Queen"))

#Separace stringu do novych sloupcu
countries = countries %>% 
  separate(hd_title_name,
           into = c("title", "name"),
           sep="-",
           extra = "merge") %>% 
  select(country, title, name)

# Transformace stringu ----------------------------------------------------
countries$name
str_squish(countries$name) #zbavovani se prebytecnych mezer
str_to_lower(countries$name)
str_to_upper(countries$name)
str_to_sentence(countries$name)
str_to_title(countries$name)

# Prace s faktory ---------------------------------------------------------
countries = read_csv("data/countries.gnumeric")

qplot(x=maj_belief, data=countries)

countries %>% 
  mutate(maj_belief=fct_relevel(maj_belief, 
                                "catholic",
                                "orthodox",
                                "protestantism",
                                "nonbelief",
                                "islam")) %>% 
  qplot(x=maj_belief, data=.)

# Razeni podle poctu vyskytu
countries %>% 
  mutate(maj_belief=fct_infreq(maj_belief)) %>% 
  qplot(x=maj_belief, data=.)

#obraceni poradi
countries %>% 
  mutate(maj_belief=fct_infreq(maj_belief),
         maj_belief=fct_rev(maj_belief)) %>% 
  qplot(x=maj_belief, data=.)

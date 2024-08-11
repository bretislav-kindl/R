# Library includes --------------------------------------------------------
install.packages(c("tidyverse", "marginaleffects", "lspline", "splines", "see", "performance", "sandwich", "svglite"))

library(tidyverse)
library(marginaleffects)
library(splines)
library(lspline)
library(see)
library(performance)


# Data downloads ----------------------------------------------------------
#https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-08
ipf_lifts <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

#VO: How does squat strength differentiate between man and woman across age and weight?

# Data cleaning and formatting --------------------------------------------
ipf_lifts$date_ms = as.numeric(as.POSIXct(ipf_lifts$date, format="%Y-%m-%d")) #converting date to ms
ipf_lifts = ipf_lifts %>%  filter(!place %in% c("DQ", "DD", "NS", "G") & !is.na(age_class)) #removing records of lifters that did not qualify
ipf_lifts = ipf_lifts %>% 
  mutate(weight_class_kg_stand = case_when( #creating custom weight brackets
    bodyweight_kg < 52 ~ "52-",
    bodyweight_kg >= 52 & bodyweight_kg <= 60 ~ "52-60",
    bodyweight_kg > 60 & bodyweight_kg <= 68 ~ "61-68",
    bodyweight_kg > 68 & bodyweight_kg <= 76 ~ "69-76",
    bodyweight_kg > 76 & bodyweight_kg <= 84 ~ "77-84",
    bodyweight_kg > 84 & bodyweight_kg <= 90 ~ "85-90",
    bodyweight_kg > 90 ~ "90+",
  ),age_class_stand = case_when( #creating custom age brackets
    age < 18 ~ "18-",
    age >= 18 & age <= 19 ~ "18-19",
    age > 20 & age <= 23 ~ "20-23",
    age > 23 & age <= 29 ~ "24-29",
    age > 29 & age <= 34 ~ "30-34",
    age > 34 & age <= 39 ~ "35-39",
    age > 39 & age <= 44 ~ "40-44",
    age > 44 & age <= 49 ~ "45-49",
    age > 49 & age <= 54 ~ "50-54",
    age > 54 & age <= 59 ~ "55-59",
    age > 59 ~ "60+"
  )
  ) %>% 
  filter(!is.na(weight_class_kg_stand) #removing records with missing weight or age data
         & !is.na(age_class_stand)) %>% 
  filter(date_ms >= 327708000) #removing all records older than 1980-05-21, because no woman participated in meets before that


# Frequency table and histograms ------------------------------------------
ipf_lifts_frequency_table = ipf_lifts %>% 
  group_by(weight_class_kg_stand, age_class_stand, sex) %>%
  summarise(frequency = n()) %>% 
  arrange(frequency)
write_csv(ipf_lifts_frequency_table, "data/ipf_lifts_frequency_table.csv")

weight_class_histogram = ggplot(ipf_lifts_frequency_table, aes(x = weight_class_kg_stand, y = frequency, fill = sex)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Weight category (kg)",
    y = "Frequency",
    title = "Histogram of men and woman involment in powerlifting events by weight",
    caption = "Source: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-08",
    color = "Sex"
  ) + 
  theme_minimal()
ggsave(plot=weight_class_histogram, 
       filename = "weight_class_histogram.svg",
       device = "svg",
       path = "plots",
       units = "cm",
       width = 17,
       height = 14)

age_class_histogram = ggplot(ipf_lifts_frequency_table, aes(x = age_class_stand, y = frequency, fill = sex)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Age category",
    y = "Frequency",
    title = "Histogram of men and woman involment in powerlifting events by age",
    caption = "Source: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-08",
    color = "Sex"
  ) + 
  theme_minimal()
ggsave(plot=age_class_histogram, 
       filename = "age_class_histogram.svg",
       device = "svg",
       path = "plots",
       units = "cm",
       width = 17,
       height = 14)

ipf_lifts_by_sex_and_date_data = ipf_lifts %>% 
  group_by(date, sex) %>% 
  summarise(frequency = n()) %>% 
  arrange(frequency)

ipf_lifts_by_sex_and_date_histogram = ggplot(ipf_lifts_by_sex_and_date_data, aes(x = date, y = frequency, fill = sex)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Date",
    y = "Frequency",
    title = "Histogram displaing male and female participation in powerlifting events thoughout time",
    caption = "Source: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-08",
    color = "Sex"
  ) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=1, margin = margin(t = 0, r = 20, b = 0, l = 0)))

ggsave(plot=ipf_lifts_by_sex_and_date_histogram, 
       filename = "ipf_lifts_by_sex_and_date_histogram.svg",
       device = "svg",
       path = "plots",
       units = "cm",
       width = 55,
       height = 14)

# Analysis ----------------------------------------------------------------

#Male and female squad strength comparison throughout time - unused
#m1_time = lm(best3squat_kg ~ ns(date_ms, df=2)*sex*age_class_stand*weight_class_kg_stand+equipment+meet_name, data=ipf_lifts)
#avg_comparisons_date = avg_comparisons(m1, variables = list("sex"="pairwise"), by=c("date"))
#plot_predictions(m1_time, condition = c("date_ms", "sex"), vcov = "HC3") +
# scale_x_continuous(labels = ~ format(as.POSIXct(.x, origin = '1970-01-01'),
#                                       "%Y-%m-%d"))

#Male and female squad strength comparison by age and weight categories
m1 = lm(best3squat_kg ~ sex*age_class_stand*weight_class_kg_stand+equipment+meet_name, data=ipf_lifts)
squad_strength_sex_age_weight_plot = plot_predictions(m1, condition = c("weight_class_kg_stand", "sex", "age_class_stand"), vcov = "HC3") +
  labs(
    x = "Weight categories (kg)",
    y = "Best 3 squat (kg)",
    title = "Graph with data for best squat (out of 3) for men and woman categorized by weight (x axis) and age (graph title)",
    caption = "Source: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-08",
    color = "Sex"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(plot=squad_strength_sex_age_weight_plot, 
       filename = "squad_strength_sex_age_weight_plot.svg",
       device = "svg",
       path = "plots",
       units = "cm",
       width = 17,
       height = 14)

avg_comparisons_best3squat_kg = avg_comparisons(m1, variables = list("sex"="pairwise"), by=c("age_class_stand", "weight_class_kg_stand"))
write_csv(avg_comparisons_best3squat_kg, "data/avg_comparisons_best3squat_kg.csv")



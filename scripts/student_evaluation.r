library(tidyverse)

evaluace = read_rds("data/course-evals/course_evals.rds")

evaluace=evaluace %>% 
  na.omit()#cisteni dat

view(evaluace)

qplot(x=gender, data=evaluace) #graf pomeru mnozstvi predmetu venede muzskymi vs. zenskymi kantory

department_male = evaluace %>%
  filter(gender=="male") %>% 
  select(lecturer, department) %>% 
  group_by(department) %>% 
  distinct() %>% 
  count(department, sort=TRUE) %>% 
  ungroup()#katedry serazene podle mnozstvi muzskych kantoru

department_female = evaluace %>% 
  filter(gender=="female") %>% 
  select(lecturer, department) %>% 
  group_by(department) %>% 
  distinct() %>% 
  count(department, sort=TRUE) %>% 
  #left_join(evaluace, by = 'department') %>% 
  #select(department, n) %>% 
  #distinct() %>% 
  ungroup()#katedry serazene podle mnozstvi zenskych kantoru

departments = evaluace %>% 
  select(lecturer, department) %>% 
  group_by(department) %>% 
  distinct() %>% 
  count(department, sort=TRUE) %>% 
  ungroup()

departments %>% 
  left_join(department_female, by = 'department') %>% 
  mutate(index_female_ratio = n.y/n.x) %>% 
  arrange(desc(index_female_ratio)) %>% 
  select(department, index_female_ratio, n.x) %>% 
  filter(n.x > 5) %>% 
  rename(n = n.x)

departments %>% 
  left_join(department_female, by = 'department') %>% 
  mutate(index_female_ratio = n.y/n.x) %>% 
  arrange(index_female_ratio) %>% 
  select(department, index_female_ratio, n.x) %>% 
  rename(n = n.x)

department_male %>% slice_max(order_by = n, n=5) %>% view()

department_female %>% slice_max(order_by = n, n=5) %>% view()

evaluace %>% select(department) %>% distinct() %>% nrow()#pocet kateder

evaluace %>% 
  select(lecturer, gender, lecturer_rating, department) %>% 
  group_by(lecturer, department , gender) %>% 
  summarise(lecturer_rating = mean(lecturer_rating)) %>% view()

evaluace_mean = evaluace %>% 
  select(lecturer, gender, lecturer_rating) %>% 
  group_by(lecturer, gender) %>% 
  summarise(lecturer_rating = mean(lecturer_rating))#pomer muzskych a zenskych kantoru

qplot(x=gender, data=evaluace_mean)
evaluace_mean %>% filter(gender=='male') %>% nrow() - evaluace_mean %>% filter(gender=='female') %>% nrow()

#pomer vyucovanych predmetu na kontora podle pohlavi
num_of_female_lecturers = evaluace %>% 
  filter(gender=="female") %>% 
  select(lecturer) %>% 
  distinct() %>% 
  nrow()

num_of_courses_with_female_lecturer = evaluace %>% 
  filter(gender=="female") %>% 
  nrow()

num_of_courses_with_male_lecturer = evaluace %>% 
  filter(gender=="male") %>% 
  nrow()

num_of_male_lecturers=evaluace %>% 
  filter(gender=="male") %>% 
  select(lecturer) %>% 
  distinct() %>% 
  nrow()

num_of_courses_with_female_lecturer/num_of_female_lecturers
num_of_courses_with_male_lecturer/num_of_male_lecturers

evaluace %>% 
  filter(gender=="male") %>%
  select(enrolled) %>% 
  sum(evaluace$enrolled)#suma vsech prhlasenych lidi u vsech predmetu vedenych muzskym kantorem

evaluace %>% 
  filter(gender=="female") %>%
  select(enrolled) %>% 
  sum(evaluace$enrolled)#suma vsech prhlasenych lidi u vsech predmetu vedenych zenskym kantorem

evaluace %>% 
  select(semester) %>% 
  distinct() #mnozstvi semestru

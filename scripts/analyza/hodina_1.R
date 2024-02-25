#Uvod do R - 1.hodina

# Typy objektu ------------------------------------------------------------
#Atomove vektory

3
"Adam"

c("Fred", "Velma", "Scooby") #atomovy vektor s vice elementy

#Integer
#Double
#Character (string)
#Logical (boolean)

#NA - chybejici hodnota
#NAN - not a number

#vektor musi mit konzistenti datatyp dat, jinak prevadi na Charakter

#Factor
factor(x=c("Agree","Neutral","Disagree"),
       levels = c("Agree","Neutral","Disagree"))

#Matice a tabulky
matrix(c(1,2,3,4,5,6),nrow=2)

table()

#List
list()

#Dataframe
data.frame()


# Pojmenovavani objektu ---------------------------------------------------

# name <- c("Fred", "Velma", "Scooby")
name = c("Fred", "Velma", "Scooby")
name

gang = data.frame(name = name, age = c(17,15,3),is_dog=c(FALSE,FALSE,TRUE))
View(gang)
print(gang)


# Funkce ------------------------------------------------------------------

age=c(15,13,3)
mean(age)

age=c(15,13,NA)
mean(age)

mean(x=age, na.rm = TRUE)

length(is_dog)#nelze
length(gang$is_dog)
length(gang[["is_dog"]])
length(gang[,3])


# Ukol --------------------------------------------------------------------

heights = data.frame(men = c(175, 183, 191), woman = c(173,181,169))
mean_men = mean(heights$men)
mean_men
mean_woman = mean(heights$woman)
mean_woman
mean_men-mean_woman


# retezeni funkci ---------------------------------------------------------

me = wake_up(me)
me = wash(me)
me = run(me)

me = run(wash(wake_up(me)))

#pipe operatory
library(tidyverse)

me %>% 
  wake_up() %>% 
  wash() %>% 
  run()

heights$men %>% 
  log() %>% 
  mean()


# Balicky -----------------------------------------------------------------
install.packages("tidyverse") #instalace
library(tidyverse) #zapnuti



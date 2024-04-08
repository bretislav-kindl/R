library(marginaleffects)
library(performance)

install.packages("see")
library(see)
turnout <- readr::read_rds("https://github.com/Sociology-FA-CU/applied-regression-r/raw/main/data/vote-2017-parliament.rds")
teachers <- readr::read_csv("https://github.com/Sociology-FA-CU/applied-regression-r/raw/main/data/teacher-ratings.csv")

# Turnout model -----------------------------------------------------------

m1 = lm(vote ~ agea, data=turnout)
plot(m1, which = 1) # linearita/homoskedasticita
plot(m1, which = 3) # homoskedasticita
plot(m1, which = 2) # q-q plot/normalita
hist(m1$residuals) #normalni rozdeleni

check_model(m1, 
            check=c("linearity",
                    "homogeneity",
                    "normality"))

plot_predictions(m1, condition = "agea",
                 points = 1)

m2 = lm(vote ~ poly(agea, 2), data=turnout)
check_model(m2,
            check = c("linearity",
                      "homogeneity",
                      "normality"))
plot_predictions(m2, condition = "agea",
                 points = 1)

# Fit model predicting eval by beauty and gender. Check assumptions -------
m3 = lm(eval~beauty*gender,data=teachers) #interakce mezi beauty a gender
check_model(m3,
            check = c("linearity",
                      "homogeneity",
                      "normality"))

m4 = lm(eval~beauty+gender,data=teachers) #nezavislost mezi beuaty a gender
check_model(m4,
            check = c("linearity",
                      "homogeneity",
                      "normality"))

library(tidyverse)
library(marginaleffects)
library(splines)
library(lspline)
library(haven)
library(gtsummary)

dataset = read_sav("./data/issp2007CR_hiso1.sav", encoding="latin1")

qplot(x=volil, data=dataset)
dataset %>% select(volil) %>% tbl_summary()
qplot(x=qcc, data=dataset)
#qplot(x=W4znamky5t, data=dataset)
mod1 = lm(volil ~ qcc, data=dataset)
plot_predictions(mod1, condition="qcc")
summary(mod1)
tbl_regression(mod1, exponentiate = TRUE)

#novy data
df2 = read_sav("./data/AnonCten12r4pr190227.sav", encoding="latin1")

md2 = lm(W4znamky5t ~ divky * vzdRodW4i_VS, data=df2)
summary(md2)
plot_predictions(md2, condition="W4znamky5t")

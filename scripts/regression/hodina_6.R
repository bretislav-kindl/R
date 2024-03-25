library(tidyverse)
library(dagitty)
library(ggdag)

dag = dagify(student_wellbeing ~ school_budget + parent_ses + school_clubs,
             school_clubs ~ school_budget,
             academic_performace ~ school_budget + student_wellbeing,
             school_budget ~ parent_ses,
             exposure="school_budget",
             outcome="student_wellbeing")

ggdag(dag, text_col = 'red') + theme_dag_blank()

adjustmentSets(dag, effect="total")
ggdag_adjustment_set(dag, effect="total")

adjustmentSets(dag, effect="direct")
ggdag_adjustment_set(dag, effect="direct")

#Collider
is_collider(dag, "academic_performace")
is_collider(dag, "parent_ses")
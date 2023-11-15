# Logistic and poisson回归

library(tidymodels)
library(gtsummary)
library(epiDisplay)
fit1 <- glm(vs~wt+mpg,data = mtcars,family = binomial()) #建模Logistic回归

fit1 %>% tidy(conf.int = T,exponentiate = T) #直接出系数，p值以及可信区间

fit1 %>% tbl_regression(exponentiate = T) #直接整理为表格结果

fit1 %>% logistic.display(crude = T) #单因素分析结果


fit2 <- glm(vs~wt+mpg,data = mtcars,family = poisson()) #建模poisson回归

fit2 %>% tidy(conf.int = T,exponentiate = T) #直接出系数，p值以及可信区间

fit2 %>% tbl_regression(exponentiate = T) #直接整理为表格结果

fit2 %>% idr.display(crude = T) #单因素分析结果

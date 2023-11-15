#Demo for Linear Regression
#普通线性回归的示例代码，且整理为Tidy风格的结果
#示例数据使用R语言自带的mtcars数据

library(tidymodels)
library(epiDisplay)
library(gtsummary)
mtcars

fit1 <- lm(mpg~disp+hp+am,data = mtcars)#建模，纳入协变量

fit1 %>% summary() #输出结果，但需要自己整理系数和可信区间

fit1 %>% tidy(conf.int = T) #直接出系数，p值以及可信区间

fit1 %>% tbl_regression() #直接整理为表格结果

fit1 %>% regress.display(crude = T) #单因素与多因素分析结果


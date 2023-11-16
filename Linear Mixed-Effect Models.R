# load packages
# linear mixed effect models
library(lmerTest)
library(tidymodels)
library(broom.mixed)
library(gtsummary)
# 环境流行病学中，线性混合效应模型主要适用于两种研究设计
# 1.定组研究（重复测量）
# 2. 多地区研究

# 由于重复测量与多地区研究数据，内部存在相关性，需要使用带有随机效应的模型进行建模

# Demo

# 使用自带的sleep数据
# 一般使用随机截距模型，即兴趣变量在不同subject中的效应量一致

# 连续型变量建模
fit1 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy) #建模

fit1 %>% tidy(conf.int=T)

fit1 %>% tbl_regression()

# 二分类变量建模
# 取反对数
fit2 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
              data = cbpp, family = binomial) #建模

fit2 %>% tidy(conf.int=T,exponentiate = T)

fit2 %>% tbl_regression(exponentiate = T)


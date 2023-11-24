
# 分组回归

library(dplyr)
library(broom)
library(jstable) # 分类变量必须改为因子
mtcars <- mtcars %>% mutate(am=factor(am),vs=factor(vs))

# jstable 可按分层变量进行拟合模型，并且可得到交互作用的显著性
# 如下所示分层变量为am，添加vs变量的协方差
TableSubgroupGLM(formula=mpg~hp,var_subgroup = "am",
                 var_cov = "vs", # 协方差 
                 data = mtcars,family = "gaussian") #按分层直接拟合模型




# 不推荐使用，需要配合purrr包循环
mtcars %>% 
  nest_by(am) %>% # <1>
  mutate(model=list(lm(mpg~hp,data=data))) %>% # <2>
  reframe(tidy(model,conf.int=T)) # <3>
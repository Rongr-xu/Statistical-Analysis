# 生存分析

# 考虑时间效应以及失访的存在

library(survival)
library(survminer)
library(broom)
#lung数据集模拟建模
lung

#KM生存曲线绘制

fit1 <- survfit(Surv(time,status)~sex,data = lung)

plot(fit1)

ggsurvplot(fit1,
           conf.int = T, #可信区间
           surv.median.line = "hv" #刻画中位生存时间
           )


#构建coxph模型
fit2 <- coxph(Surv(time,status)~age+sex+inst,data = lung)

summary(fit2)

fit2 %>% tidy(exponentiate = T,conf.int = T)

fit2 %>% tbl_regression(exponentiate = T,conf.int = T)

cox.zph(fit2) #等比例风险检验

# 数据存在聚类情况, 需要使用cluster 参数或 frailty 
# example






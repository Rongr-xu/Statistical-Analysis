
# GAM--描述暴露变量与结局变量之间非线性关系

library(mgcv)
library(mgcViz)

# 模型拟合方式与普通回归一致，主要是以图形式

fit1 <- gam(mpg~s(wt)+s(hp),data = mtcars) # 设定为薄板样条回归

summary(fit1)

# 结果主要以图的形式显示

plot(fit1,shade = T,shade.col = "lightblue",
     main = "Relationship",select = 1,col="darkblue",
     lwd=2,rug = F,xlab = "",ylab = "") #select 选择画图的变量


# 拟合二分类变量

fit2 <- gam(am~wt+s(hp),data = mtcars,family = binomial())

summary(fit2)

plot(fit2,shade = T,shade.col = "lightblue",
     main = "Relationship",select = 1,col="darkblue",
     lwd=2,rug = F,xlab = "",ylab = "")

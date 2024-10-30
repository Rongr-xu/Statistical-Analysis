library(tidyverse)
library(pifpaf)

# Quick Start 

require(datasets)
ozone_exposure  <- na.omit(airquality$Ozone) 
ozone_exposure  <- as.data.frame(ozone_exposure) 

sampling_weights <- c(rep(1/232, 58), rep(0.75/58, 58)) # 采样权重

thetahat <- 0.009709 # 风险
thetavar <- 0.00025 # RR的方差

rr <- function(X, theta){ exp(theta*X/5)}

# PAF 计算
paf(X = ozone_exposure, thetahat = thetahat, 
    rr = rr)
cft <- function(X)(rep(0.5,nrow(ozone_exposure)))

# 可信区间
paf.confidence(X = ozone_exposure, thetahat = thetahat, 
               thetavar = thetavar, rr = rr, 
               weights = sampling_weights, nsim = 200)

pif.confidence(X = ozone_exposure, thetahat = thetahat, thetavar = thetavar, 
               rr = rr, cft = cft, 
               weights = sampling_weights, nsim = 200)

counterfactual.plot(X = ozone_exposure, cft = cft, 
                    weights = sampling_weights, n=250)


# Discrete RR
require(datasets)
tobacco_consumption <- as.data.frame(esoph$tobgp)
#Thetas
thetahat <- c(1, 1.59, 2.57, 4.11)

#Relative Risk
rr       <- function(X, theta){
  
  #Create empty vector to fill with RR's
  r_risk <- rep(NA, nrow(X))
  
  #Select by cases
  r_risk[which(X == "0-9g/day")] <- theta[1]
  r_risk[which(X == "10-19")]    <- theta[2]
  r_risk[which(X == "20-29")]    <- theta[3]
  r_risk[which(X == "30+")]      <- theta[4]
  
  return(r_risk)
}
paf(tobacco_consumption, thetahat, rr)
## [1] 0.55047

cft <- function(X){
  
  #Create empty matrix to fill with RR's
  new_tobacco <- matrix(NA, nrow = nrow(X), ncol = 1)
  
  #Select by cases
  new_tobacco[which(X == "0-9g/day")] <- "0-9g/day"  #These remain
  new_tobacco[which(X == "10-19")]    <- "10-19"     #the same
  new_tobacco[which(X == "20-29")]    <- "10-19"
  new_tobacco[which(X == "30+")]      <- "10-19"
  
  # X in relative risk is received as a data.frame
  new_tobacco <- as.data.frame(new_tobacco)
  return(new_tobacco)
}

pif(tobacco_consumption, thetahat, rr, cft)

thetavar <- diag(c(0.119, 0.041, 0.001, 0.093))
paf.confidence(X = tobacco_consumption, thetahat = thetahat, 
               thetavar = thetavar, rr = rr,  
               confidence_method = "bootstrap", nsim = 200)
paf.sensitivity(tobacco_consumption, thetahat=thetahat, rr, 
                nsim = 10, mremove = 20)

# Incomplete data Continous RR example
sbp <- data.frame("Region"   = c("Afr D", "Afr E", "Amr A", "Amr B", "Amr D",
                                 "Emr B", "Emr D", "Eur A", "Eur B", "Eur C",
                                 "Sear B", "Sear D", "Wpr A", "Wpr B"), 
                  "SBP_mean" = c(123, 121, 114, 115, 117, 126, 121, 
                                 122, 122, 125, 120, 117, 120, 115),
                  "SBP_sd"   = c(20, 13, 14, 15, 15, 15, 15, 
                                 15, 16, 17, 15, 14, 15, 16))

thetahat <- 0.71
thetavar <- 0.002

#Notice that the theoretical minimum risk value is 115 and not 0
rr       <- function(X, theta){ theta*(X - 115)^2/121 + 1} 
afr_mean <- as.data.frame(subset(sbp, Region == "Afr E")$SBP_mean)
afr_var  <- subset(sbp, Region == "Afr E")$SBP_sd^2 

#Calculate paf using approximate method
paf(X = afr_mean, thetahat = thetahat, rr = rr, method = "approximate",
    Xvar = afr_var, check_rr = FALSE)

paf.confidence(X = afr_mean, thetahat = thetahat,  thetavar = thetavar, rr = rr, method = "approximate", 
               Xvar = afr_var, check_rr = FALSE, nsim = 200)

# Incomplete data Categorical RR example: Body Mass Index



















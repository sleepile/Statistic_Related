#2-Recognition and source memory
#随便下载几个包
require('pacman')
p_load('tidyverse','bruceR','papaja', 'psych')

#import_data
library(haven)
Full_Data <- read_sav("Full Data.sav")

R_and_SM_data <- Full_Data %>% select(age)

#描述性统计
bruceR::Describe(R_and_SM_data)

#数据处理

age <- as.numeric(unlist( c(Full_Data %>% select(age))))

#1
p_ignJold <- as.numeric(unlist( c(Full_Data %>% select(p_ignJold))))
p_newJold <- as.numeric(unlist( c(Full_Data %>% select(p_newJold))))
p_attJold <- as.numeric(unlist( c(Full_Data %>% select(p_attJold))))
n_new <- as.numeric(unlist( c(Full_Data %>% select(n_new))))
n_IG<- as.numeric(unlist( c(Full_Data %>% select(n_IG))))
n_AT<- as.numeric(unlist( c(Full_Data %>% select(n_AT))))

#2
CIDPerCorrATT <- as.numeric(unlist( c(Full_Data %>% select(CIDPerCorrATT))))
CIDPerCorrIGN <- as.numeric(unlist( c(Full_Data %>% select(CIDPerCorrIGN))))
CIDPerCorrNEW <- as.numeric(unlist( c(Full_Data %>% select(CIDPerCorrIGN))))


#计算指标
hit_rate <- ((20*p_attJold+n_IG*p_ignJold+0.5)/(n_IG+n_AT+1))
FA_rate <- ((n_new*p_newJold+0.5)/(n_new+1))

hit_rate1 <- ((20*p_attJold+20*p_ignJold+0.5)/(40+1))
FA_rate1 <- ((n_new*p_newJold+0.5)/(40+1))


#标准化
z_hit_rate <-scale(hit_rate)
z_FA_rate <- scale(FA_rate)
d1 <- (z_hit_rate - z_FA_rate)

#将d1添加到数据集中
R_and_SM_data$d1 <- d1

#正态性检验-QQ图
qqnorm(d1)
qqline(d1)

#线性假设检验：散点图
plot(age,d1,"p")

#回归1：age
linear_regression1 <- lm(d1~age)
summary(linear_regression1)

#残差方差齐性
plot(linear_regression1,which=3)

#残差正态分布
plot(linear_regression1,which=2)

#结果图：回归1
library(tidyverse)

ggplot(,aes(age,d1))+
  geom_point(size=2,shape=21)+
  geom_smooth(method = "lm")

#线性回归2：在方程中添加age平方
age2 <- age*age
linear_regression2_model <- lm(formula =d1 ~ age+age2)

#在数据集中添加age平方
R_and_SM_data$age2 <-age2

#线性回归前提检验:
#残差方差齐性：残差的方差不受X变量取值的影响
linear_regression_model_residuals <-residuals(linear_regression2_model)
linear_regression_model_predictors <- predict(linear_regression2_model)
scatter.smooth(linear_regression_model_predictors ,linear_regression_model_residuals)

#残差正态性
qqnorm(linear_regression_model_residuals)
qqline(linear_regression_model_residuals)

#残差方差齐性
plot(linear_regression2_model,which=3)

#变量之间不存在多元共线性：检验变量之间有无相关
bruceR::Corr(R_and_SM_data)
vif(linear_regression2_model)

#独立性：回归的误差项和因变量Y不相关

#线性检验-散点图
plot(age2,d1,"p")

#线性回归2
linear_regression2 <- lm(d1 ~ age+age2)
summary(linear_regression2)

#画图

ggplot(,aes(age,d1,color = age2))+
  geom_point(size=2,shape=21)+
  scale_color_distiller(palette = "GnBu")+
  geom_smooth(method = "lm")

  
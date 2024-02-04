#文献结果复现-priming

require('pacman')
p_load('tidyverse','bruceR','papaja', 'psych','car')

#import_data
library(haven)
Full_Data <- read_sav("Full Data.sav")

#select_data_needed
priming_data <- Full_Data %>% select(age)
priming_data_des <- Full_Data %>% select(age,mIDat,mIDn)
#descriptive
bruceR::Describe(priming_data_des)

age <- as.numeric(unlist( c(Full_Data %>% select(age))))
mIDat <- as.numeric(unlist( c(Full_Data %>% select(mIDat))))
mIDn <- as.numeric(unlist( c(Full_Data %>% select(mIDn))))

class(age)
priming_performance_attedend <- (1- mIDat/mIDn)

#线性回归前提检验
#线性检验-散点图
plot(age,priming_performance_attedend,"p")

#正态检验-QQ图
qqnorm(priming_performance_attedend)
qqline(priming_performance_attedend)



#线性回归1：在方程中添加age
linear_regression1 <- lm(priming_performance_attedend~age)
summary(linear_regression1)

#残差正态性
plot(linear_regression1,which=2)

#残差方差齐性
plot(linear_regression1,which=3)

#结果图：线性回归1
library(tidyverse)

ggplot(,aes(age,priming_performance_attedend))+
       geom_point(size=2,shape=21)+
       geom_smooth(method = "lm")

#线性回归2：在方程中添加age平方
age2 <- age*age
priming_data$age2 <-age2
priming_data$priming_performance_attedend <- priming_performance_attedend

#多元线性回归前提检验
#线性-散点图
plot(age2,priming_performance_attedend,"p")
linear_regression2_model <- lm(priming_performance_attedend ~ age+age2)

#线性回归前提检验:
#残差方差齐性：
linear_regression_model_residuals <-residuals(linear_regression2_model)
linear_regression_model_predictors <- predict(linear_regression2_model)
scatter.smooth(linear_regression_model_predictors,linear_regression_model_residuals)

#残差正态性
qqnorm(linear_regression_model_residuals)
qqline(linear_regression_model_residuals)

#残差方差齐性
plot(linear_regression2_model,which=3)


#变量之间不存在多元共线性：检验变量之间相关
library(car)
bruceR::Corr(priming_data)
vif(linear_regression2_model)

#独立性：回归的误差项和因变量Y不相关
#线性回归2
linear_regression2 <- lm(priming_performance_attedend ~ age+age2)
summary(linear_regression2)

ggplot(,aes(age,priming_performance_attedend,color = age2))+
  geom_point(size=2,shape=21)+
  scale_color_distiller(palette = "GnBu")+
  geom_smooth(method = "lm")

#线性回归3：探究分数和智力分数之间的关系

WTAR_score<- as.numeric(unlist( c(Full_Data %>% select(WTAR))))

#回归4：智力分数~年龄
#4.1 描述
bruceR::Describe(WTAR_score)

#4.2 线性回归前提检验
#线性检验-散点图
plot(age,WTAR_score,"p")

#正态检验-QQ图
qqnorm(WTAR_score)
qqline(WTAR_score)

#线性回归
linear_regression4 <- lm(WTAR_score~age)


#残差方差齐性
plot(linear_regression4,which=3)

#残差正态分布
plot(linear_regression4,which=2)

summary(linear_regression4)
ggplot(,aes(age,WTAR_score,color = age))+
  geom_point(size=2,shape=21)+
  scale_color_distiller(palette = "BuPu")+
  geom_smooth(method = "lm")


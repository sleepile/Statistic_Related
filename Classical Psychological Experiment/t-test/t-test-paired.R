#t-test
library(readxl)
library(bruceR)

#import full data
Full_Data <- import("C:/Users/ASUS/Desktop/经典心理实验数据分析/1_RT_total_20240226.xlsx")

#分析整体的缺失值情况:可视化
library(VIM)
aggr(Full_Data,prop=TRUE,numbers=TRUE)

matrixplot(Full_Data)

Full_Data2 <- na.omit(Full_Data)

matrixplot(Full_Data2)

#delete NA
data1 <- Full_Data2$Visual_RT_average
data2 <- Full_Data2$Audiological_RT_Average

#detect outliers
detection1 <- boxplot(Full_Data2$Visual_RT_average, main = "Boxplot1 for detecting outliers")
detection2 <- boxplot(Full_Data2$Audiological_RT_Average, main = "Boxplot2 for detecting outliers")

outliers1 <- detection1$out
outliers2 <- detection2$out

#delete outliers
names(Full_Data2)

Full_Data2_clean <- Full_Data2[!Full_Data2$Visual_RT_average %in% outliers1,]
Full_Data2_clean <- Full_Data2[!Full_Data2$Audiological_RT_Average %in% outliers2,]

#descriptive_analysis
summary(Full_Data2_clean$Visual_RT_average)
summary(Full_Data2_clean$Audiological_RT_Average)

data1_clean <- Full_Data2_clean$Visual_RT_average
data2_clean <- Full_Data2_clean$Audiological_RT_Average

#Analysis Requirement Examination
#graph_normality_test

qqnorm(data1_clean)
qqline(data1_clean)

qqnorm(data2_clean)
qqline(data2_clean)

#SW_normality_test(n<50)
shapiro.test(data1_clean)
shapiro.test(data2_clean)

#F-test
var.test(data1_clean,data2_clean)

cor(data1_clean,data2_clean)

#t-test
t.test(data1_clean,data2_clean,paired = TRUE)

#for one-sample t-test:
#t.test(data1,mu = average)



# 随便下几个包
require('pacman')
p_load('tidyverse','bruceR','papaja', 'psych')

#导入excel数据，默认只获取第一页的数据
Full_Data <- import("C:/Users/ASUS/Desktop/经典心理实验数据分析/1_RT_total_20240226.xlsx")


#detect NA
library(VIM)
aggr(Full_Data,prop=TRUE,numbers=TRUE)
matrixplot(Full_Data)

#delete NA
Full_Data2 <- na.omit(Full_Data)
matrixplot(Full_Data2)

#Visual
  #detect outliers
  detection1 <- boxplot(Full_Data2$Visual_RT_500ms, main = "Boxplot1 for detecting outliers")
  detection2 <- boxplot(Full_Data2$Visual_RT_1000ms, main = "Boxplot2 for detecting outliers")
  detection3 <- boxplot(Full_Data2$Visual_RT_1500ms, main = "Boxplot3 for detecting outliers")

  outliers1 <- detection1$out
  outliers2 <- detection2$out
  outliers3 <- detection3$out

  #delete outliers
  Full_Data2 <- Full_Data2[!Full_Data2$Visual_RT_500ms %in% outliers1,]
  Full_Data2 <- Full_Data2[!Full_Data2$Visual_RT_1000ms %in% outliers2,]
  Full_Data2 <- Full_Data2[!Full_Data2$Visual_RT_1500ms %in% outliers3,]

  #Descriptive
  summary(Full_Data2$Visual_RT_500ms)
  summary(Full_Data2$Visual_RT_1000ms)
  summary(Full_Data2$Visual_RT_1500ms)


#前提检验
  #正态分布-Visual
  shapiro.test(Full_Data2$Visual_RT_500ms)
  shapiro.test(Full_Data2$Visual_RT_1000ms)
  shapiro.test(Full_Data2$Visual_RT_1500ms)

  #正态分布-Audiological
  shapiro.test(Full_Data2$Audiological_RT_500ms)
  shapiro.test(Full_Data2$Audiological_RT_1000ms)
  shapiro.test(Full_Data2$Audiological_RT_1500ms)
  
  #方差齐性
    #if normality passed
      #barlett.test(var1,var2)
      #或：bartlett.test(var1 ~ groupvar) # group可以是多分组
    #if not passed
      #leveneTest(var1,var2)

var.test(Full_Data2$Visual_RT_500ms,Full_Data2$Visual_RT_1000ms)
var.test(Full_Data2$Visual_RT_500ms,Full_Data2$Visual_RT_1500ms)
var.test(Full_Data2$Visual_RT_1000ms,Full_Data2$Visual_RT_1500ms)

var.test(Full_Data2$Audiological_RT_500ms,Full_Data2$Audiological_RT_1000ms)
var.test(Full_Data2$Audiological_RT_500ms,Full_Data2$Audiological_RT_1500ms)
var.test(Full_Data2$Audiological_RT_1000ms,Full_Data2$Audiological_RT_1500ms)

#方差分析

##新建数据框
###计算被试个数
n <- nrow(Full_Data2)

###长数据DataFrame
###a1 = Visual/Audiological
values <- c(Full_Data2$Visual_RT_500ms, Full_Data2$Visual_RT_1000ms, Full_Data2$Visual_RT_1500ms,
            Full_Data2$Audiological_RT_500ms,Full_Data2$Audiological_RT_1000ms,Full_Data2$Audiological_RT_1500ms)
group_RT <- factor(rep(c("500", "1000", "1500",
                         "500", "1000", "1500"), 
                       each = n))
groups_V_A <- factor(rep(c("Visual_RT","Audiological_RT"), each = n*3))

condition_code <- factor(rep(c("A1B1","A1B2","A1B3","A2B1","A2B2","A2B3"), each = n))
print(condition_code)

df <- data.frame(
  RT = values,
  Group_RT = group_RT,
  Groups_V_A = groups_V_A,
  Condition = condition_code
)

##宽数据data frame
a1b1 <- c(Full_Data2$Visual_RT_500ms)
a1b2 <- c(Full_Data2$Visual_RT_1000ms)
a1b3 <- c(Full_Data2$Visual_RT_1500ms)
a2b1 <- c(Full_Data2$Audiological_RT_500ms)
a2b2 <- c(Full_Data2$Audiological_RT_1000ms)
a2b3 <- c(Full_Data2$Audiological_RT_1500ms)

a2b3

df2 <- data.frame(
  A1B1 = a1b1,
  A1B2 = a1b2,
  A1B3 = a1b3,
  A2B1 = a2b1,
  A2B2 = a2b2,
  A2B3 = a2b3
)

##数据分析
###重复设计
library(bruceR)
MANOVA(df2,dvs="A1B1:A2B3",dvs.pattern = "A(.)B(.)",
       within = c("A","B"),
       sph.correction="GG")

###bruceR参考：https://github.com/psychbruce/bruceR

sph.correctiion="GG"

####如果交互作用显著，则忽视主效应，进行简单效应分析

MANOVA(df2,dvs="A1B1:A2B3",dvs.pattern = "A(.)B(.)",
       within = c("A","B"),
       sph.correction="GG") %>%
  EMMEANS("A",by="B") %>%
  EMMEANS("B",by="A")

####如果交互作用不显著，则查看主效应是否显著，
####如果主效应显著，则进行事后多重比较；如果主效应不显著则停止检验


#清洗后数据输出：excel/csv
install.packages("openxlsx")
library(openxlsx)
write.xlsx(df_long_visual, file = "clean_data.xlsx")
write.csv(df_long_visual,file = "clean_data.csv")



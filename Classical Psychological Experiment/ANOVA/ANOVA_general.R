# download packages
require('pacman')
p_load('tidyverse','bruceR','papaja', 'psych')

#import excel data(only 1st page if not assigned)
Full_Data <- import("C:/Users/ASUS/Desktop/经典心理实验数据分析/mental_rotation.xlsx")

#detect NA
library(VIM)
aggr(Full_Data,prop=TRUE,numbers=TRUE)
matrixplot(Full_Data)

#delete NA
#create 2 data frame for further operation
Full_Data1 <- na.omit(Full_Data)
Full_Data2 <- na.omit(Full_Data)

matrixplot(Full_Data1)

#change column name for convenience
setnames(Full_Data1,
         c("R_0_RT","R_60_RT","R_120_RT","R_180_RT","R_240_RT","R_300_RT",
           "R_reversed_0_RT","R_reversed_60_RT","R_reversed_120_RT",
           "R_reversed_180_RT","R_reversed_240_RT","R_reversed_300_RT"),
         c("A1B1","A1B2","A1B3","A1B4","A1B5","A1B6",
           "A2B1","A2B2","A2B3","A2B4","A2B5","A2B6"))

setnames(Full_Data2,
         c("R_0_RT","R_60_RT","R_120_RT","R_180_RT","R_240_RT","R_300_RT",
           "R_reversed_0_RT","R_reversed_60_RT","R_reversed_120_RT",
           "R_reversed_180_RT","R_reversed_240_RT","R_reversed_300_RT"),
         c("A1B1","A1B2","A1B3","A1B4","A1B5","A1B6",
           "A2B1","A2B2","A2B3","A2B4","A2B5","A2B6"))

#to change outliers into mean
 #detect outliers
  #这里能不能用循环啊（复制粘贴好累
  detection1 <- boxplot(Full_Data1$A1B1, main = "Boxplot1 for detecting outliers")
  detection2 <- boxplot(Full_Data1$A1B2, main = "Boxplot2 for detecting outliers")
  detection3 <- boxplot(Full_Data1$A1B3, main = "Boxplot3 for detecting outliers")
  detection4 <- boxplot(Full_Data1$A1B4, main = "Boxplot4 for detecting outliers")
  detection5 <- boxplot(Full_Data1$A1B5, main = "Boxplot5 for detecting outliers")
  detection6 <- boxplot(Full_Data1$A1B6, main = "Boxplot6 for detecting outliers")
  
  detection7 <- boxplot(Full_Data1$A2B1, main = "Boxplot7 for detecting outliers")
  detection8 <- boxplot(Full_Data1$A2B2, main = "Boxplot8 for detecting outliers")
  detection9 <- boxplot(Full_Data1$A2B3, main = "Boxplot9 for detecting outliers")
  detection10 <- boxplot(Full_Data1$A2B4, main = "Boxplot10 for detecting outliers")
  detection11 <- boxplot(Full_Data1$A2B5, main = "Boxplot11 for detecting outliers")
  detection12 <- boxplot(Full_Data1$A2B6, main = "Boxplot12 for detecting outliers")
  
  outliers1 <- detection1$out
  outliers2 <- detection2$out
  outliers3 <- detection3$out
  outliers4 <- detection4$out
  outliers5 <- detection5$out
  outliers6 <- detection6$out
  
  outliers7 <- detection7$out
  outliers8 <- detection8$out
  outliers9 <- detection9$out
  outliers10 <- detection7$out
  outliers11 <- detection8$out
  outliers12 <- detection9$out

  #delete outliers
  Full_Data1 <- Full_Data1[!Full_Data1$A1B1 %in% outliers1,]
  Full_Data1 <- Full_Data1[!Full_Data1$A1B2 %in% outliers2,]
  Full_Data1 <- Full_Data1[!Full_Data1$A1B3 %in% outliers3,]
  Full_Data1 <- Full_Data1[!Full_Data1$A1B4 %in% outliers4,]
  Full_Data1 <- Full_Data1[!Full_Data1$A1B5 %in% outliers5,]
  Full_Data1 <- Full_Data1[!Full_Data1$A1B6 %in% outliers6,]
  
  Full_Data1 <- Full_Data1[!Full_Data1$A2B1 %in% outliers7,]
  Full_Data1 <- Full_Data1[!Full_Data1$A2B2 %in% outliers8,]
  Full_Data1 <- Full_Data1[!Full_Data1$A2B3 %in% outliers9,]
  Full_Data1 <- Full_Data1[!Full_Data1$A2B4 %in% outliers10,]
  Full_Data1 <- Full_Data1[!Full_Data1$A2B5 %in% outliers11,]
  Full_Data1 <- Full_Data1[!Full_Data1$A2B6 %in% outliers12,]
  
  #Descriptive (if needed)
  describe(Full_Data1)
  
  #calculate mean
  mean1 <- mean(Full_Data1$A1B1)
  mean2 <- mean(Full_Data1$A1B2)
  mean3 <- mean(Full_Data1$A1B3)
  mean4 <- mean(Full_Data1$A1B4)
  mean5 <- mean(Full_Data1$A1B5)
  mean6 <- mean(Full_Data1$A1B6)
  
  mean7 <- mean(Full_Data1$A2B1)
  mean8 <- mean(Full_Data1$A2B2)
  mean9 <- mean(Full_Data1$A2B3)
  mean10 <- mean(Full_Data1$A2B4)
  mean11 <- mean(Full_Data1$A2B5)
  mean12 <- mean(Full_Data1$A2B6)
  
  
  #change outliers into mean
   #test
   detection_test <- boxplot(Full_Data2$A1B1, main = "Boxplot3 for detecting outliers")
   
   Full_Data2$A1B1[Full_Data2$A1B1 %in% outliers1] <- mean1
  
   detection_test2 <-boxplot(Full_Data2$A1B1, main = "Boxplot3 after replacing outliers")
  
  Full_Data2$A1B2[Full_Data2$A1B2 %in% outliers2] <- mean2
  Full_Data2$A1B3[Full_Data2$A1B3 %in% outliers3] <- mean3
  Full_Data2$A1B4[Full_Data2$A1B4 %in% outliers4] <- mean4
  Full_Data2$A1B5[Full_Data2$A1B5 %in% outliers5] <- mean5
  Full_Data2$A1B6[Full_Data2$A1B6 %in% outliers6] <- mean6
  
  Full_Data2$A2B1[Full_Data2$A2B1 %in% outliers7] <- mean7
  Full_Data2$A2B2[Full_Data2$A2B2 %in% outliers8] <- mean8
  Full_Data2$A2B3[Full_Data2$A2B3 %in% outliers9] <- mean9
  Full_Data2$A2B4[Full_Data2$A2B4 %in% outliers10] <- mean10
  Full_Data2$A2B5[Full_Data2$A2B5 %in% outliers11] <- mean11
  Full_Data2$A2B6[Full_Data2$A2B6 %in% outliers12] <- mean12
  
#前提检验
  # A1 = R
  #normality test-A
  shapiro.test(Full_Data2$A1B1)
  shapiro.test(Full_Data2$A1B2)
  shapiro.test(Full_Data2$A1B3)

  shapiro.test(Full_Data2$A1B4)
  shapiro.test(Full_Data2$A1B5)
  shapiro.test(Full_Data2$A1B6)
  
  #B1 = 0 degree
  #normality test-B
  shapiro.test(Full_Data2$A2B1)
  shapiro.test(Full_Data2$A2B2)
  shapiro.test(Full_Data2$A2B3)
  
  shapiro.test(Full_Data2$A2B4)
  shapiro.test(Full_Data2$A2B5)
  shapiro.test(Full_Data2$A2B6)
  
  #variance test: 
  #var.test(data1$c1,data1$c2)
  #or:
    #if normality test passed
      #barlett.test(var1,var2)
      #或：bartlett.test(var1 ~ groupvar) # group可以是多分组
    #if not passed
      #leveneTest(var1,var2)
    #within-subject experiment design：no need for variance test


#ANOVA
###calculate the number of subject
n <- nrow(Full_Data2)

##DA
### ANOVA(within subject)
library(bruceR)
MANOVA(df2,dvs="A1B1:A2B6",dvs.pattern = "A(.)B(.)",
       within = c("A","B"),
       sph.correction="GG")

###bruceR：https://github.com/psychbruce/bruceR

####如果交互作用显著，则忽视主效应，进行简单效应分析

MANOVA(df2,dvs="A1B1:A2B6",dvs.pattern = "A(.)B(.)",
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



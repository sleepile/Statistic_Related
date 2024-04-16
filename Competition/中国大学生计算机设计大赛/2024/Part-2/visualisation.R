require('pacman')
p_load('tidyverse','bruceR','papaja', 'psych','car','ggplot2')


#数据分析与初步可视化
##人口流动情况
population <- import("C:/Users/ASUS/Desktop/2024007076-作品主文件夹/作品代码文件夹：2024007076-02素材与源码/Part-2/population.xlsx",sheet = 1)
head(population)

###城乡间人口转移情况
ggplot(data = population,aes(x=year,y=population_in_100000000,fill=type))+
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "城乡人口变化情况（2018-2021）",
       x = "Year",
       y = "population in 100,000,000",
       fill = "type") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = population,aes(x=year,y=percentage_in_total_population,fill=type))+
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "城乡人口总人口之比变化（2018-2021）",
       x = "Year",
       y = "Population in 10000",
       fill = "type") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

###城乡间劳动力转移情况：导入数据
off_farm_workers <- import("C:/Users/ASUS/Desktop/2024007076-作品主文件夹/作品代码文件夹：2024007076-02素材与源码/Part-2/population.xlsx",sheet = 2)
workers <- import("C:/Users/ASUS/Desktop/2024007076-作品主文件夹/作品代码文件夹：2024007076-02素材与源码/Part-2/population.xlsx",sheet = 3)

####劳动力情况
head(workers)
ggplot(data =workers,aes(x=year,y=amount_in_100000000,fill=type))+
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "就业人口情况变化（2018-2021）",
       x = "Year",
       y = "Population in 10000",
       fill = "type") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_brewer(palette = "Spectral")

####农民工情况
head(off_farm_workers)
ggplot(data = off_farm_workers,aes(x=year,y=农民工规模in_10000,fill=type))+
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "农民工流出地变化（2018-2021）",
       x = "Year",
       y = "Population in 100,000,000",
       fill = "type") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_brewer(palette = "Spectral")


#城乡教育发展情况比较
##导入数据
teachers <- import("C:/Users/ASUS/Desktop/2024007076-作品主文件夹/作品代码文件夹：2024007076-02素材与源码/Part-2/education.xlsx",sheet = 1)
students <- import("C:/Users/ASUS/Desktop/2024007076-作品主文件夹/作品代码文件夹：2024007076-02素材与源码/Part-2/education.xlsx",sheet = 2)
facility <- import("C:/Users/ASUS/Desktop/2024007076-作品主文件夹/作品代码文件夹：2024007076-02素材与源码/Part-2/education.xlsx",sheet = 3)
funding <- import("C:/Users/ASUS/Desktop/2024007076-作品主文件夹/作品代码文件夹：2024007076-02素材与源码/Part-2/education.xlsx",sheet = 4)

##学生基本情况
head(students)
ggplot(data = students, aes(x = year, y = schools_in_10000, fill = 学校类型, color = 城乡类型)) +
  geom_bar(width = 1, size=1.5,stat = "identity") +
  coord_polar(theta = "y") +
  labs(x = "year", y = "schools_in_10000", fill = "学校类型", color = "城乡类型") +
  theme_void() +
  theme(legend.position = "bottom")+ 
  scale_fill_brewer(palette = "Spectral")+
  labs(title = "学校数量变化（2018&2021）")+
       theme(plot.title = element_text(hjust = 0.5))



##教育资源:导入数据
head(students)
students_selected <- students %>% 
  filter(year == 2021)

ggplot(data = students, aes(x = 学校类型, y = teachers_in_10000, fill = 城乡类型)) +
  geom_col() +
  labs(x = "学校类型", y = "teachers_in_10000", fill = "城乡类型") +
  theme(legend.position = "bottom") +
  labs(title = "教师数量（2021）")+
  theme(plot.title = element_text(hjust = 0.5))

students_selected2 <- students %>% 
  filter(year == 2018)

ggplot(data = students_selected2, aes(x = 学校类型, y = teachers_in_10000, fill = 城乡类型)) +
  geom_col() +
  labs(x = "学校类型", y = "teachers_in_10000", fill = "城乡类型") +
  theme(legend.position = "bottom") +
  labs(title = "教师数量（2018）")+
  theme(plot.title = element_text(hjust = 0.5))

###教师资源（全国）
head(teachers)
ggplot(data = teachers, aes(x = 年份, y = 生师比teacher1, fill = 教育阶段)) +
  geom_col() +
  labs(x = "学校类型", y = "生师比（teacher=1）", fill = "城乡类型") +
  theme(legend.position = "bottom") +
  labs(title = "生师比变化情况")+
  theme(plot.title = element_text(hjust = 0.5))

###办学条件
head(funding)
plot(funding)

#2021
head(facility)
facility_selected <- facility %>% 
  filter(year == 2021)

ggplot(data = facility_selected, aes(x = 教育阶段, y = 占地面积m2, color = 城乡分布)) +
  geom_point(size=3) +
  labs(x = "学校类型", y = "占地面积m2", color = "城乡类型") +
  theme(legend.position = "bottom")+
  labs(title = "学校占地面积2021")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = facility_selected, aes(x = 教育阶段, y = 绿化用地面积m2, color = 城乡分布)) +
  geom_point(size=3) +
  labs(x = "学校类型", y = "绿化用地面积m2", color = "城乡类型") +
  theme(legend.position = "bottom")+
  labs(title = "绿化用地面积m2_2021")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = facility_selected, aes(x = 教育阶段, y = 运动场地面积m2, color = 城乡分布)) +
  geom_point(size=3) +
  labs(x = "学校类型", y = "运动场地面积m2", color = "城乡类型") +
  theme(legend.position = "bottom")+
  labs(title = "运动场地面积m2_2021")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = facility_selected, aes(x = 教育阶段, y = 教学仪器设备资产值in10000, color = 城乡分布)) +
  geom_point(size=3) +
  labs(x = "学校类型", y = "教学仪器设备资产值in10000", color = "城乡类型") +
  theme(legend.position = "bottom")+
  labs(title = "教学仪器设备资产值in10000_2021")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = facility_selected, aes(x = 教育阶段, y =  固定资产总值in10000, color = 城乡分布)) +
  geom_point(size=3) +
  labs(x = "学校类型", y = " 固定资产总值in10000", color = "城乡类型") +
  theme(legend.position = "bottom")+
  labs(title = "固定资产总值in10000_2021")+
  theme(plot.title = element_text(hjust = 0.5))

#2018
facility_selected2 <- facility %>% 
  filter(year == 2018)

ggplot(data = facility_selected2, aes(x = 教育阶段, y = 占地面积m2, color = 城乡分布)) +
  geom_point(size=3) +
  labs(x = "学校类型", y = "占地面积m2", color = "城乡类型") +
  theme(legend.position = "bottom")+
  labs(title = "学校占地面积2018")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = facility_selected2, aes(x = 教育阶段, y = 绿化用地面积m2, color = 城乡分布)) +
  geom_point(size=3) +
  labs(x = "学校类型", y = "绿化用地面积m2", color = "城乡类型") +
  theme(legend.position = "bottom")+
  labs(title = "绿化用地面积m2_2018")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = facility_selected2, aes(x = 教育阶段, y = 运动场地面积m2, color = 城乡分布)) +
  geom_point(size=3) +
  labs(x = "学校类型", y = "运动场地面积m2", color = "城乡类型") +
  theme(legend.position = "bottom")+
  labs(title = "运动场地面积m2_2018")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = facility_selected2, aes(x = 教育阶段, y = 教学仪器设备资产值in10000, color = 城乡分布)) +
  geom_point(size=3) +
  labs(x = "学校类型", y = "教学仪器设备资产值in10000", color = "城乡类型") +
  theme(legend.position = "bottom")+s
  labs(title = "教学仪器设备资产值in10000_2018")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = facility_selected2, aes(x = 教育阶段, y =  固定资产总值in10000, color = 城乡分布)) +
  geom_point(size=3) +
  labs(x = "学校类型", y = " 固定资产总值in10000", color = "城乡类型") +
  theme(legend.position = "bottom")+
  labs(title = "固定资产总值in10000_2018")+
  theme(plot.title = element_text(hjust = 0.5))

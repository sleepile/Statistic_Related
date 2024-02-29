# ML_homework

import numpy as np
import pandas as pd
import sklearn as sk 
import openpyxl 
import os
os.environ['PYDEVD_WARN_SLOW_RESOLVE_TIMEOUT'] = '2.0'  # 设置为更长的超时时间，例如2秒


# 设置列对齐（？）
pd.set_option('display.unicode.ambiguous_as_wide', True)
pd.set_option('display.unicode.east_asian_width', True)

#1. 数据导入
fulldata = pd.read_excel('C:/Users\ASUS\Desktop\Full Data.xlsx')

#2.数据处理
# 2.1 对数据集内的变量进行重新计算＆对分组变量进行编码：
#   2.1.1 计算变量
#         ①priming_performance_attedend(启动)
fulldata['priming_performance_attedend'] = 1- fulldata['mIDat']/fulldata['mIDn']

#         ②再认表现d1
#计算过程:
#z_hit_rate <-scale(hit_rate)
#z_FA_rate <- scale(FA_rate)
#d1 <- (z_hit_rate - z_FA_rate)

fulldata['hit_rate'] = ((20*fulldata['p_attJold']+20*fulldata['p_ignJold']+0.5)/(40+1))
fulldata['FA_rate'] = ((40*fulldata['p_newJold']+0.5)/(40+1))

#标准化
from sklearn.preprocessing import StandardScaler
data_to_normalize = ['hit_rate', 'FA_rate']

# 创建标准化器对象
scaler = StandardScaler()
fulldata[data_to_normalize] = scaler.fit_transform(fulldata[data_to_normalize])

#计算d1
fulldata['d1'] = fulldata['hit_rate'] - fulldata['FA_rate']

#   2.1.2 年龄分组-编码
fulldata['age_group'] = fulldata['Group'].replace(
    ['Adolescent (12-17)','Young adult (18-24)','Mid-young adult (25-34)',
     'Middle adult (35-49)','Mid-older adults (50-64)','Older adults (65+)'],
    [1,2,3,4,5,6])

print('---------fulldata-----------')
print(fulldata)

#检查列名是否存在于数据集中：
# 获取数据集的所有列名
all_columns = fulldata.columns.tolist()

print('------检查列是否在数据集中-----')

columns_to_check = ['sex', 'age_group', 'priming_performance_attedend', 'AdultChild',
                     'WTAR', 'eduYrs', 'd1', 'CIDPerCorr']
for column in columns_to_check:
    if column in all_columns:
        print(f"Column '{column}' exists in the dataset.")
    else:
        print(f"Column '{column}' does not exist in the dataset.")


#2.2 决定分类目标（=标签）
fulldata_data =fulldata[['sex','age_group',
                         'priming_performance_attedend','AdultChild','WTAR','eduYrs','d1','CIDPerCorr'] ] #数据
fulldata_target_label = fulldata[['age_group']] 
fulldata_feature_names = fulldata[['priming_performance_attedend','AdultChild','WTAR','eduYrs','d1','CIDPerCorr']] #特征名

print('------2.2------')

print('------fulldata_target_label------')
print(fulldata_target_label)

print('------fulldata_feature_names------')
print(fulldata_feature_names)

print('------fulldata_data------')
print(fulldata_data)

# 2.2 对数据集进行拆分
# 将数据集划分为训练集和测试集，其中test_size表示测试集的比例（可以是0.2表示20%的数据作为测试集）
from sklearn.model_selection import train_test_split
data_train, data_test, data_train_target, data_test_target = train_test_split(fulldata_feature_names, fulldata_target_label, test_size=0.24, random_state=42)

#2.3 对训练集进行预处理和降维
#对数据进行预处理
scaler = StandardScaler()
data_to_normalize2 = ['priming_performance_attedend','WTAR','eduYrs','CIDPerCorr']
fulldata[data_to_normalize2] = scaler.fit_transform(fulldata[data_to_normalize2])

#对数据进行降维

#3.创建k近邻分类器
from sklearn.neighbors import KNeighborsClassifier as Kfenlei
k1= Kfenlei(n_neighbors=5,weights='distance')  #分类器中选择最近的5个点作为参考，根据距离加权
k1.fit(data_train,data_train_target)  # 将训练数据X和标签Y送入分类器进行学习

#4.使用分类器进行预测
data_pred = k1.predict(data_test)

#5.对分类模型进行评价
print('对分类模型进行评价:')
from sklearn.metrics import recall_score
recall_score_value = recall_score(data_test_target, data_pred,average = 'micro') 
print("recall_score_value = ",recall_score_value)

from sklearn.metrics import accuracy_score
accuracy_score_value = accuracy_score(data_test_target, data_pred)
print("accuracy_score_value = ",accuracy_score_value)

#hamming_loss = 错误预测的标签比例
from sklearn.metrics import hamming_loss
hamming_loss_value = hamming_loss(data_test_target, data_pred)
print("hamming_loss_value = ",hamming_loss_value)
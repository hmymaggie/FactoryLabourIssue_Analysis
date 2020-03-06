#!/usr/bin/env python
# coding: utf-8

# In[143]:


import pinyin
import pandas as pd
import numpy as np
import csv


delete=['省','市','区','县','自治区','自治州','旗','左旗','中旗', '右旗','族','行政','盟','地区']


# In[148]:


reglist = pd.read_csv('region_list.csv')
df=pd.DataFrame(reglist)
#print(df['County'])
Countypy=[]
Prefecturepy=[]
Provincepy=[]
for var in df['County']:
    Countypy.append(str(var))
for var in df['Prefecture']:
    Prefecturepy.append(str(var))
for var in df['Province']:
    Provincepy.append(str(var))    
    
countypy = [w.replace('特别行政自治区', '').replace('自治区', '').replace('自治县', '').replace('旗', '').replace('左旗', '').replace('右旗', '')            .replace('族', '').replace('行政', '').replace('盟', '').replace('区', '').replace('县', '').replace('省', '')            .replace('市', '')  for w in Countypy]  
#words = [w.replace('省', '') for w in Countypy] 
provincepy=[w.replace('特别行政自治区', '').replace('自治区', '').replace('自治县', '').replace('旗', '').replace('左旗', '').replace('右旗', '')            .replace('族', '').replace('行政', '').replace('盟', '').replace('区', '').replace('县', '').replace('省', '')            .replace('市', '') for w in Provincepy]

prefecturepy=[w.replace('特别行政自治区', '').replace('自治区', '').replace('自治县', '').replace('旗', '').replace('左旗', '').replace('右旗', '')            .replace('族', '').replace('行政', '').replace('盟', '').replace('区', '').replace('县', '').replace('省', '')            .replace('市', '')  for w in Prefecturepy]

county=[]
prefecture=[]
province=[]

for char in countypy:
    py=pinyin.get(char, format="strip", delimiter="") #or delimiter =' '
    county.append(py)
    
    
for char in prefecturepy:
    py=pinyin.get(char, format="strip", delimiter="") #or delimiter =' '
    prefecture.append(py)
    
for char in provincepy:
    py=pinyin.get(char, format="strip", delimiter="") #or delimiter =' '
    province.append(py)
    
#print(province)

d={'Province':province, 'Prefecture':prefecture, 'county':county}
df=pd.DataFrame(d)
#print(df)
df.to_csv('converted_list.csv')            
#for var in df['County']:
 #   py=pinyin.get(str(var), format="strip", delimiter="") #or delimiter =' '
  #  Countypy.append(py)
    
#print(Countypy)
    #for char in delete:
     #   if var in char:
      #      var.replace(var,'')


#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd


# In[2]:


import numpy as np


# In[42]:


# read actual dataset
data = pd.read_excel("Dataset.xlsx")


# In[43]:


#taking null values of the dataset
data2 = data[data.isnull().any(axis=1)]


# In[44]:


df1 = pd.read_excel("df1.xlsx") #regions


# In[45]:


df2 = pd.read_excel("df2.xlsx") # income group


# In[47]:


df3 = pd.DataFrame(columns=["Region", "Income Group","Year","Life Expectancy"])


# In[48]:


# calculating avg of diffferent income group with regions to fill in the missing values
for i in range(len(df1)):
   for j in range(len(df2)):
       if (df1["Year"][i]== df2["Year"][j]):
           p = (df1["Life Expectancy"][i]+df2["Life Expectancy"][j] )/2
           new_entry= {"Region": df1["Country"][i], "Income Group":df2["Country"][j],"Year":df1["Year"][i],"Life Expectancy": p}             
           df3.loc[len(df3)] = new_entry   


# In[49]:


df3 = df3.drop_duplicates() # removing duplicates


# In[80]:


# mergining two datframes
df4 = pd.merge(data, df3, how="left", on=["Region", "Income Group","Year"]) 


# In[81]:


# replacing nan with avg value
df4['Life Expectancy'] = df4['Life Expectancy_x'].combine_first(df4['Life Expectancy_y']).astype(float) 


# In[82]:


df4.drop(columns = ["Life Expectancy_x","Life Expectancy_y"], inplace= True) # deleting extra columns


# In[84]:


final_df = df4


# In[85]:


final_df.index = np.arange(1, len(final_df) + 1)


# In[87]:


#saving complete dataframe
final_df.to_excel("F:\Masters\Principles Of Data Science\Group project\makeovermonday-life-expectancy-at-birth-by-country\Final_dataset.xlsx")


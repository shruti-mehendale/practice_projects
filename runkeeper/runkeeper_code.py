# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
#import matplotlib.pyplot as plt

# read the csv file 

# Define file containing dataset
runkeeper_file = 'cardioActivities.csv'

# Just read first two lines of the file to know contents of columns
#pd.read_csv(runkeeper_file, nrows=2)
# Create DataFrame with parse_dates and index_col parameters 
df_activities = pd.read_csv(runkeeper_file, parse_dates=["Date"], index_col=["Date"])

# check the shape
df_activities.shape


# First look at exported data: select sample of 3 random rows 

print(df_activities.sample(3))

# Print DataFrame summary
print(df_activities.info())


# Define list of columns to be deleted
cols_to_drop = ['Friend\'s Tagged','Route Name','GPX File','Activity Id','Calories Burned', 'Notes']

# Delete unnecessary columns
df_activities= df_activities.drop(columns=cols_to_drop)

# Count types of training activities
print(df_activities['Type'].value_counts())

# Rename 'Other' type to 'Unicycling'
df_activities['Type'] =df_activities['Type'].str.replace('Other', 'Unicycling')

# Count missing values for each column
#df_activities.isnull().sum()
print(df_activities['Type'].unique())

# Function to calculate avg of col2 with respect to uniue categories in col1
def avg_col2_col1(df,col1,col2):
    uniq = df[col1].unique()  
    avg_col2=[]
    for i in range(len(uniq)):
       # avg_col2 =[i]
       avg_col2.append(df[df[col1]==uniq[i]][col2].mean())
    
    
    return avg_col2;


uniq_lst = df_activities['Type'].unique()   
avg_hr_act = avg_col2_col1(df_activities,'Type','Average Heart Rate (bpm)')   
print(avg_hr_act)

if df_activities['Type']==unique_lst[0]:
     df_activities['Average Heart Rate (bpm)'].fillna(110, inplace=True)

#if df_activities['Type']==avg_hr_act[1,0]:    
 #   df_activities['Average Heart Rate (bpm)'].fillna(avg_hr_act[1,1], inplace=True)


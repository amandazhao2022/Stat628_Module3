import numpy as np
import pandas as pd
import csv
import os
import ast

city_name = 'Philadelphia'
data_path = os.path.abspath('Data/business.json')

data_df = pd.read_json(data_path, lines=True)
data_df_cleaned = pd.DataFrame()

for i in range(len(data_df)):
    category_str = data_df.loc[i, "categories"]
    if category_str:
        category_ls = category_str.split(', ')
    check_city = data_df.loc[i, "city"] == city_name
    if 'Coffee & Tea' in category_ls and check_city:
        data_df_cleaned = data_df_cleaned.append(data_df.loc[i])

data_df_cleaned.reset_index(drop=True, inplace=True)
attr_list = []

for j in range(len(data_df_cleaned)):
    attr_json = data_df_cleaned.loc[j, "attributes"]
    if attr_json:
        attr_keys = attr_json.keys()
        for word in attr_keys:
            if word not in attr_list:
                attr_list.append(word)

data_df_withattr = pd.DataFrame(columns =attr_list,index=list(range(len(data_df_cleaned))))


for j in range(len(data_df_cleaned)):
    attr_json = data_df_cleaned.loc[j, "attributes"]
    if attr_json:
        attr_keys = attr_json.keys()
        for word in attr_keys:

            if attr_json[word] == 'True':
                data_df_withattr.at[j,word]= 1
            elif attr_json[word] == 'False':
                data_df_withattr.at[j,word]= 0
            elif attr_json[word] == 'None':
                data_df_withattr.at[j,word]= np.nan
            else:
                data_df_withattr.at[j,word]= attr_json[word]
            if word =='BusinessParking':
                parkingDict = ast.literal_eval(attr_json[word])
                if parkingDict:
                    parking_boolean = any(parkingDict.values())
                    data_df_withattr.at[j,'ParkingAvailability']= int(parking_boolean)
            if word == 'WiFi':
                wifi_str = attr_json[word].replace('u', '').replace('\'', '')
                data_df_withattr.at[j,word]= wifi_str

data_df_result = pd.concat([data_df_cleaned, data_df_withattr], axis=1)
data_df_result = data_df_result.dropna(how='any',axis=1,thresh=300)

data_df_result = data_df_result.drop(columns=['is_open','attributes','RestaurantsDelivery', 'BusinessParking','BikeParking', 'Alcohol', 'Caters','RestaurantsGoodForGroups','Ambience','GoodForKids','RestaurantsReservations','NoiseLevel','RestaurantsAttire','HasTV'])

#data_df_result_dummy = pd.get_dummies(data_df_result,columns=['OutdoorSeating','BusinessAcceptsCreditCards','RestaurantsTakeOut','RestaurantsTakeOut','ParkingAvailability'])


csv_path = os.path.abspath('Data/data_cleaned.csv')
data_df_result.to_csv(csv_path, sep=',')


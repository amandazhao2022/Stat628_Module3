import pandas as pd
import csv

city_name = 'Philadelphia'
data_path = 'business.json'

data_df = pd.read_json(data_path, lines=True)
data_df_cleaned = pd.DataFrame()

for i in range(len(data_df)):
    category_str = data_df.loc[i, "categories"]
    if category_str:
        category_ls = category_str.split(', ')
    check_city = data_df.loc[i, "city"] == city_name
    if 'Coffee & Tea' in category_ls and check_city:
        data_df_cleaned = data_df_cleaned.append(data_df.loc[i])
        print("Data index " + str(i))

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
            data_df_withattr.at[j,word]= attr_json[word]

data_df_result = pd.concat([data_df_cleaned, data_df_withattr], axis=1)
# data_df_result = data_df_result.dropna(how='any',axis=1,thresh=50)
print(data_df_result)


csv_path = "data_cleaned.csv"
data_df_result.to_csv(csv_path, sep=',')


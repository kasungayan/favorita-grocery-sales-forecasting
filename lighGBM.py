# Favorita-grocery-sales-forecasting.
# LighGBM Forecasting Script.
# Kasun Bandara, June 2020

# Importing the required libraries
import lightgbm as lgbm
import numpy as np
import pandas as pd

#Setting the seed
np.random.seed(777)

numCols = [f"d_{day}" for day in range(1, 1660)]

catCols = ['store_id', 'item_id', 'family', 'class', 'perishable', 'city', 'state', 'state_type', 'cluster']

dtype = {numCol: "float32" for numCol in numCols}
dtype.update({catCol: "category" for catCol in catCols})

sales_data = pd.read_csv("Sales_train_final.csv", usecols=catCols + numCols, dtype=dtype)

calendarDTypes = {"dayweek": "category",
                  "month": "category",
                  "daymonth": "category",
                  "type": "category",
                  "description": "category",
                 "transferred": "category"}
                 
calendar_data = pd.read_csv("holiday_modified_all.csv", dtype=calendarDTypes)

for col, colDType in calendarDTypes.items():
    if colDType == "category":
        calendar_data[col] = calendar_data[col].cat.codes.astype("int16")
        calendar_data[col] -= calendar_data[col].min()
        
calendar_data = calendar_data.drop(columns=['transferred'])
calendar_data.rename(columns={'date':'d'}, inplace=True)

calendar_data["d"] = [f"d_{rank}" for rank in range(1,1689)]
data = pd.melt(sales_data, id_vars=['store_id', 'item_id', 'family', 'class', 'perishable', 'city', 'state', 'state_type', 'cluster'], var_name="d", value_name="sales")

for lags in range(1,36):
    data['lag' + str(lags)] = data[["item_id", "store_id","sales"]].groupby(['item_id','store_id'])["sales"].shift(lags)
    
full_training_data = data.merge(calendar_data, on="d", copy=False)
full_training_data_x = full_training_data.drop(columns=['sales', 'd'])

full_training_data_y = full_training_data.sales

cat_features = ['store_id', 'item_id', 'family', 'class', 'perishable', 'city', 'state', 'state_type', 'cluster',
                'month', 'daymonth', 'type', 'description']
               
validation_indices = np.random.choice(full_training_data_x.index.values, 1_0000, replace = False)
training_indices = np.setdiff1d(full_training_data_x.index.values, validation_indices)

training_data = lgbm.Dataset(full_training_data_x.loc[training_indices], label = full_training_data_y.loc[training_indices],
                        categorical_feature = cat_features, free_raw_data = False)
valid_data = lgbm.Dataset(full_training_data_x.loc[validation_indices], label = full_training_data_y.loc[validation_indices],
                        categorical_feature = cat_features, free_raw_data = False)
                        
params = {
          "objective" : "mse",
          "metric" :"rmse",
          "force_row_wise" : True,
          "learning_rate" : 0.075,
          "sub_row" : 0.75,
          "bagging_freq" : 1,
          "lambda_l2" : 0.1,
          "metric": ["rmse"],
          'verbosity': 1,
          'num_iterations' : 500,
          'num_leaves': 128,
          "min_data_in_leaf": 100,
         }
        
m_lgb = lgbm.train(params, training_data, valid_sets = [valid_data], verbose_eval = 20)
print("Training Completed")

sales_data.rename(columns={'class':'class_type'}, inplace=True)

lag_size = 35
testing_data = sales_data.iloc[:, -lag_size:]
testing_data['store_id'] = sales_data.store_id
testing_data['item_id'] = sales_data.item_id
testing_data['family'] = sales_data.family
testing_data['class'] = sales_data.class_type
testing_data['perishable'] = sales_data.perishable
testing_data['city'] = sales_data.city
testing_data['state']= sales_data.state
testing_data['state_type'] = sales_data.state_type
testing_data['cluster'] = sales_data.cluster

for day in range(1661, 1689):
    testing_data[f"d_{day}"] = np.nan
    
testing_data = pd.melt(testing_data, id_vars=['store_id', 'item_id', 'family', 'class', 'perishable', 'city', 'state', 'state_type', 'cluster'], var_name="d", value_name="sales")

for lags in range(1,36):
    testing_data['lag' + str(lags)] = testing_data[["item_id", "store_id","sales"]].groupby(['item_id','store_id'])["sales"].shift(lags)
  
print("Before Testing")
  
cols = [f"F{i}" for i in range(1,29)]
for prediction_point in range(1661, 1689):
    current_testing_data = testing_data[testing_data["d"].str.split("d_").str[1].astype(int) == prediction_point]
    current_testing_data = current_testing_data.drop(columns=["d", "sales"])
    prediction = m_lgb.predict(current_testing_data)

    # add the most recent prediction back to the testing data
    testing_data.loc[testing_data["d"].str.split("d_").str[1].astype(int) == prediction_point, "sales"] = prediction

    # recreate the lags
    for lags in range(1,11):
        testing_data['lag' + str(lags)] = testing_data[["item_id", "store_id","sales"]].groupby(['item_id','store_id'])["sales"].shift(lags)
        
sub = testing_data.loc[testing_data["d"].str.split("d_").str[1].astype(int) >= 1661, ["item_id", "store_id","sales"]].copy()
sub["F"] = [f"F{rank}" for rank in sub.groupby(['item_id','store_id'])["id"].cumcount()+1]
sub = sub.set_index(["id", "F" ]).unstack()["sales"][cols].reset_index()
sub.sort_values(['item_id','store_id'], inplace = True)
sub.reset_index(drop=True, inplace = True)


sub2 = sub.copy()
sub.to_csv("submission.csv",index=False)
import pandas as pd
import os

current_dir = os.path.dirname(os.path.realpath(__file__))
os.chdir(current_dir)
gerdf = pd.read_csv('datasets/datasets_germany.csv', ',')
eldf = pd.read_csv('datasets/amalias_traffic_dataset.csv', ',')

def design_match(df):
    des = pd.read_csv('datasets/design_params.csv',',')
    df = pd.merge(left=df, right=des, how="inner", left_on='case', right_on='case')
    return df

gerdf = design_match(gerdf)
eldf = design_match(eldf)

gerdf.set_index('period').to_csv('datasets/datasets_germany_DESIGN.csv')
eldf.set_index('period').to_csv('datasets/amalias_traffic_dataset_DESIGN.csv')


# KEEP ONLY LINES OF CODE YOU NEED FOR BOTH 
# CREATE SOME FUNCTIONS
# READ THE DATA FROM YOUR ONLINE LINK?
# READ COORDS OF ZONES FROM A FILE THAN WRITTING THEM IN THE CODE
# CREATE FUNCTION WITH PLOTS AND SAVE THEM SOMEWHERE ON GITHUB

import pandas as pd
pd.set_option('display.max_rows', None)
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from shapely.geometry import Point, Polygon, LineString
import os

# import datasets,
# the link for Frankfurter Straße, Lange Straße and Markplatz: 

# https://leopard.tu-braunschweig.de/receive/dbbs_mods_00071535



fsbr = pd.read_csv(os.getcwd()+'\datasets\\fsbr.csv', parse_dates=['timestamp'])
lsho = pd.read_csv(os.getcwd()+'\datasets\\lsho.csv', parse_dates=['timestamp'])
mke = pd.read_csv(os.getcwd()+'\datasets\\mke.csv', parse_dates=['timestamp'])

layout_fsbr = os.getcwd()+'\datasets\\layout_fsbr.png'
layout_lsho = os.getcwd()+'\datasets\\layout_lsho.png'
layout_mke = os.getcwd()+'\datasets\\layout_mke.png'

length_fsbr=LineString([(1098.3112,1440.008),(1098.3112+4759.3484,1440.008+109.8311)])

# Split the area into zones!!
#measurement for Frankfurter Straße, Bad Rothenfelde (fsbr)
# IMPORTANT, definition of circulation zone
circulation_zone_fsbr = Polygon([(1052.3301,1261.0919),
                                 (1052.3301-8.5209,1261.0919+353.617),
                                 (1052.3301-8.5209+622.0251,1261.0919+353.617+59.6463),
                                 (1052.3301-8.5209+622.0251+711.4944,1261.0919+353.617+59.6463+42.6044),
                                 (1052.3301-8.5209+622.0251+711.4944+1708.4387,1261.0919+353.617+59.6463+42.6044-12.7813),
                                 (1052.3301-8.5209+622.0251+711.4944+1708.4387+1759.5641,1261.0919+353.617+59.6463+42.6044-12.7813+8.5209),
                                 (1052.3301-8.5209+622.0251+711.4944+1708.4387+1759.5641,1261.0919+353.617+59.6463+42.6044-12.7813+8.5209-362.1379),
                                 (1052.3301-8.5209+622.0251+711.4944+1708.4387+1759.5641-1103.4555,1261.0919+353.617+59.6463+42.6044-12.7813+8.5209-362.1379+12.7813),
                                 (1052.3301-8.5209+622.0251+711.4944+1708.4387+1759.5641-1103.4555-668.8899,1261.0919+353.617+59.6463+42.6044-12.7813+8.5209-362.1379+12.7813-4.2604),
                                 (1052.3301-8.5209+622.0251+711.4944+1708.4387+1759.5641-1103.4555-668.8899-1763.8245,1261.0919+353.617+59.6463+42.6044-12.7813+8.5209-362.1379+12.7813-4.2604-29.8231)])
# here SAFE = ACTIVITY ZONE...
# one side
safe_zone_fsbr1 = Polygon([(5826.3491,1150.8094),
                           (5826.3491+30.1259,1150.8094+174.7302),
                           (5826.3491+30.1259-3542.8059,1150.8094+174.7302-24.1007),
                           (5826.3491+30.1259-3542.8059+18.0756,1150.8094+174.7302-24.1007-144.6043),
                           (5826.3491+30.1259-3542.8059+18.0756-825.4497,1150.8094+174.7302-24.1007-144.6043-60.2518),
                           (5826.3491+30.1259-3542.8059+18.0756-825.4497-18.0755,1150.8094+174.7302-24.1007-144.6043-60.2518+144.6043),
                           (5826.3491+30.1259-3542.8059+18.0756-825.4497-18.0755-156.6547,1150.8094+174.7302-24.1007-144.6043-60.2518+144.6043+6.0252),
                           (5826.3491+30.1259-3542.8059+18.0756-825.4497-18.0755-156.6547+277.1583,1150.8094+174.7302-24.1007-144.6043-60.2518+144.6043+6.0252-409.71227),
                           (5826.3491+30.1259-3542.8059+18.0756-825.4497-18.0755-156.6547+277.1583+825.4496,1150.8094+174.7302-24.1007-144.6043-60.2518+144.6043+6.0252-409.71227+216.90647)])
# the other side.
safe_zone_fsbr2 = Polygon([(1138.759,2018.4353),
                           (1138.759+6.0252,2018.4353-385.6115),
                           (1138.759+6.0252+1644.8742,2018.4353-385.6115+108.4532),
                           (1138.759+6.0252+1644.8742-24.1007,2018.4353-385.6115+108.4532+162.6799),
                           (1138.759+6.0252+1644.8742-24.1007+807.3741,2018.4353-385.6115+108.4532+162.6799-6.0252),
                           (1138.759+6.0252+1644.8742-24.1007+807.3741+24.1007,2018.4353-385.6115+108.4532+162.6799-6.0252-168.705),
                           (1138.759+6.0252+1644.8742-24.1007+807.3741+24.1007+1253.2375,2018.4353-385.6115+108.4532+162.6799-6.0252-168.705+24.1007),
                           (1138.759+6.0252+1644.8742-24.1007+807.3741+24.1007+1253.2375-24.1008,2018.4353-385.6115+108.4532+162.6799-6.0252-168.705+24.1007+162.6799),
                           (1138.759+6.0252+1644.8742-24.1007+807.3741+24.1007+1253.2375-24.1008+837.5001,2018.4353-385.6115+108.4532+162.6799-6.0252-168.705+24.1007+162.6799-12.0504),
                           (1138.759+6.0252+1644.8742-24.1007+807.3741+24.1007+1253.2375-24.1008+837.5001,2018.4353-385.6115+108.4532+162.6799-6.0252-168.705+24.1007+162.6799-12.0504+156.6547)])
d_fsbr = 0.0145

print('Area of circulation zone in fsbr:', circulation_zone_fsbr.area*d_fsbr**2, 'm²')
print('Area of safe zone in fsbr:', safe_zone_fsbr1.area*d_fsbr**2 + safe_zone_fsbr2.area*d_fsbr**2, 'm²')
print('Length of circulation zone in fsbr:', length_fsbr.length*d_fsbr, 'm')
fsbr['area_circulation'] = circulation_zone_fsbr.area*d_fsbr**2 
fsbr['area_safe'] = safe_zone_fsbr1.area*d_fsbr**2 + safe_zone_fsbr2.area*d_fsbr**2 
fsbr['length'] = length_fsbr.length*d_fsbr

fig, ax = plt.subplots()
ax.axis('off')
img = plt.imread(layout_fsbr)
ax.imshow(img)
ax.fill(*safe_zone_fsbr1.exterior.xy)
ax.fill(*safe_zone_fsbr2.exterior.xy)
ax.fill(*circulation_zone_fsbr.exterior.xy, facecolor='k')

#measurement for Lange Straße, Hessisch Oldendorf (lsho)
length_lsho=LineString([(770.3484,474.96941),(770.3484+845.9654,474.96941+2.36304)])

circulation_zone_lsho = Polygon([(725.21685,351.62029),
                                 (725.21685+2.44181,351.62029+234.41353),
                                 (725.21685+2.44181+915.67784,351.62029+234.41353+12.20904),
                                 (725.21685+2.44181+915.67784-7.3254,351.62029+234.41353+12.20904-253.94799)])

safe_zone_lsho1 = Polygon([(732.54228,349.17849),
                           (732.54228+75.69603,349.17849-231.97173),
                           (732.54228+75.69603+429.75809,349.17849-231.97173+7.32543),
                           (732.54228+75.69603+429.75809+95.2305,349.17849-231.97173+7.32543+224.6463)])

safe_zone_lsho2 = Polygon([(764.28578,605.56828),
                           (764.28578+26.85988,605.56828+73.25423),
                           (764.28578+26.85988+903.46884,605.56828+73.25423+17.09265),
                           (764.28578+26.85988+903.46884-109.8814,605.56828+73.25423+17.09265-85.46326)])

d_lsho = 0.029
print('Area of circulation zone in lsho:', circulation_zone_lsho.area*d_lsho**2, 'm²')
print('Area of safe zone in lsho:', safe_zone_lsho1.area*d_lsho**2 + safe_zone_lsho2.area*d_lsho**2, 'm²')
print('Length of circulation zone in lsho:', length_lsho.length*d_lsho, 'm')
lsho['area_circulation'] = circulation_zone_lsho.area*d_lsho**2 
lsho['area_safe'] = safe_zone_lsho1.area*d_lsho**2 + safe_zone_lsho2.area*d_lsho**2 
lsho['length'] = length_lsho.length*d_lsho

fig, ax = plt.subplots()
ax.axis('off')
img = plt.imread(layout_lsho)
ax.imshow(img)
ax.fill(*safe_zone_lsho1.exterior.xy)
ax.fill(*safe_zone_lsho2.exterior.xy)
ax.fill(*circulation_zone_lsho.exterior.xy, facecolor='k')

#measurement for Marktplazt, Königslutter am Elm (mke)
length_mke=LineString([(4202.2284,1344.7131),(9103.3538,1203.1643)])

circulation_zone_mke = Polygon([(4064.0758,993.44076),
                                (4030.2085,1682.0758),
                                (9302.218,1478.872),
                                (9268.3507,891.83886),
                                (8545.8483,925.70616),
                                (6288.0284,1083.7536)])

safe_zone_mke = Polygon([(4064.0758,745.08057),(4052.7867,1049.8863),(6446.0758,959.57346),(7507.2512,699.92417)])
     
activity_zone_mke = Polygon([(4176.9668,2765.8294),(7225.0237,3025.4787),(9076.436,1659.4976),(4222.1232,1715.9431)])

d_mke = 0.0106
print('Area of circulation zone in mke:', circulation_zone_mke.area*d_mke**2, 'm²')
print('Area of "safe" zone in mke:', safe_zone_mke.area*d_mke**2 + activity_zone_mke.area*d_mke**2, 'm²')
print('Length of circulation zone in mke:', length_mke.length*d_mke, 'm')
mke['area_circulation'] = circulation_zone_mke.area*d_mke**2
mke['area_safe'] = safe_zone_mke.area*d_mke**2 + activity_zone_mke.area*d_mke**2 
mke['length'] = length_mke.length*d_mke

fig, ax = plt.subplots()
ax.axis('off')
img = plt.imread(layout_mke)
ax.imshow(img)
ax.fill(*safe_zone_mke.exterior.xy)
ax.fill(*activity_zone_mke.exterior.xy)
ax.fill(*circulation_zone_mke.exterior.xy, facecolor='k')

def plot_user(df,layout,user):
    img = plt.imread(layout)
    fig, ax = plt.subplots()
    #ax.axis('off')
    ax.imshow(img)

    ax=sns.lineplot(data=df[df['user']==user], x='coordx',y='coordy',units='id',sort=False,legend=False,
                linewidth=0.3, estimator=None, ci=None)
    

def rearrange(df):
    #transforming all movements in the the circulation zone to crossing 
    #because in this context, I believe they can be considered as crossing (to be discussed later)
    df['action'].replace({'walking_circulation':'crossing'},inplace=True)
    for i in range(0,len(df['action'].unique())-1):
        if df['action'].unique()[i]=='not_reliable_crossing':
            df['action'].replace({'not_reliable_crossing':'crossing'},inplace=True)
        else:
            pass
        
    #aggregating traffic count/mean speed every 2 min    
    d0=df[(df['user']=='person')].groupby([pd.Grouper(key='timestamp',freq='2min',label='right')])['id'].nunique().to_frame().fillna(0).rename(columns={'id':'n_pedestrians'})
    d1=df[(df['user']=='person')&(df['zone']=='circulation_zone')&(df['action']=='crossing')].groupby(['action',pd.Grouper(key='timestamp',freq='2min',label='right')])['id'].nunique().unstack(0).fillna(0).rename(columns={'crossing':'ped_crossing'})
    d2=df[(df['user']=='car')].groupby([pd.Grouper(key='timestamp',freq='2min',label='right')]).agg({'id':'nunique','speed_local':'mean'}).rename(columns={'id':'n_cars', 'speed_local':'mean_car_speed'})
    d3=df[(df['user']=='bicycle')&(df['zone']=='circulation_zone')].groupby([pd.Grouper(key='timestamp',freq='2min',label='right')]).agg({'id':'nunique','speed_local':'mean'}).rename(columns={'id':'n_cyclists', 'speed_local':'mean_cycle_speed'})
    d4=df[df['user']=='person'].groupby('timestamp')['id'].nunique().to_frame().fillna(0).reset_index().groupby([pd.Grouper(key='timestamp',freq='2min',label='right')])['id'].last().to_frame().rename(columns={'id':'ped_timestamp'})
    d5=df[(df['user']=='person')&(df['zone']=='circulation_zone')&(df['action']=='crossing')].groupby('timestamp')['id'].nunique().to_frame().fillna(0).reset_index().groupby([pd.Grouper(key='timestamp',freq='2min',label='right')])['id'].last().to_frame().rename(columns={'id':'ped_crossing_timestamp'})
    d6=df[df['user']=='bicycle'].groupby('timestamp')['id'].nunique().to_frame().fillna(0).reset_index().groupby([pd.Grouper(key='timestamp',freq='2min',label='right')])['id'].last().to_frame().rename(columns={'id':'bike_timestamp'})
    d7=df[df['user']=='car'].groupby('timestamp')['id'].nunique().to_frame().fillna(0).reset_index().groupby([pd.Grouper(key='timestamp',freq='2min',label='right')])['id'].last().to_frame().rename(columns={'id':'car_timestamp'})
    
    df2 = pd.concat([d0,d1,d2,d3,d4,d5,d6,d7], axis=1).fillna(0)
    
    #mean traffic speed
    df2.loc[df2['mean_car_speed']==0, 'mean_traffic_speed'] = df2['mean_cycle_speed']
    df2.loc[df2['mean_cycle_speed']==0, 'mean_traffic_speed'] = df2['mean_car_speed']
    df2.loc[(df2['mean_cycle_speed']>0)&(df2['mean_car_speed']>0), 'mean_traffic_speed'] = (df2['mean_car_speed']+df2['mean_cycle_speed'])/2
    
    #traffic density
    df2['ped_dens_circ']=df2['ped_crossing_timestamp']/df['area_circulation'][0] 
    df2['ped_dens_safe']=(df2['ped_timestamp']-df2['ped_crossing_timestamp'])/df['area_safe'][0] #here area_safe was a sum of all safe zones within the space 
    df2['car_dens_circ']=df2['car_timestamp']/df['length'][0] 
    df2['cycle_dens_circ']=df2['bike_timestamp']/df['length'][0] 
    df2['traffic_dens_circ']=(df2['car_timestamp']+df2['bike_timestamp'])/df['length'][0]
    
    return df2.reset_index()

test_fsbr=rearrange(fsbr)
test_fsbr['case']='fsbr'

test_lsho=rearrange(lsho)
test_lsho['case']='lsho'

test_mke=rearrange(mke)
test_mke['case']='mke'

df=pd.concat([test_fsbr,test_lsho,test_mke], ignore_index=True)

path=os.getcwd()+'\datasets\\datasets_germany.csv' 
df.to_csv(path,index=False)

def density_calculation(df):
    df0=df.groupby(['timestamp','user'])['id'].nunique().unstack().fillna(0).rename(columns={'bicycle':'n_cyclists','car':'n_cars','person':'n_pedestrians'})
    df1=df[(df['user']=='person')&(df['zone']=='circulation_zone')&(df['action']=='crossing')].groupby('timestamp')['id'].nunique().to_frame().rename(columns={'id':'n_pedestrians_crossing'})
    df2 = pd.concat([df0, df1], axis=1).fillna(0).reset_index()
    
        #traffic density
    df2['ped_dens_circ']=df2['n_pedestrians_crossing']/df['area_circulation'][0] 
    df2['ped_dens_safe']=(df2['n_pedestrians']-df2['n_pedestrians_crossing'])/df['area_safe'][0] #here area_safe was a sum of all safe zones within the space 
    df2['car_dens_circ']=df2['n_cars']/df['length'][0] 
    df2['cycle_dens_circ']=df2['n_cyclists']/df['length'][0] 
    df2['traffic_dens_circ']=(df2['n_cars']+df2['n_cyclists'])/df['length'][0]
    
    return df2

test_fsbr=density_calculation(fsbr)
test_fsbr['case']='fsbr'

test_lsho=density_calculation(lsho)
test_lsho['case']='lsho'

test_mke=density_calculation(mke)
test_mke['case']='mke'

df=pd.concat([test_fsbr,test_lsho,test_mke], ignore_index=True)

path=os.getcwd()+'\datasets\\density_datasets_germany.csv' 
df.to_csv(path,index=False)

def count(df):
    d1=df[(df['user']=='person')&(df['zone']=='circulation_zone')&(df['action']!='walking_safe')].groupby(['action',pd.Grouper(key='timestamp',freq='2min',label='right')])['id'].nunique().unstack(0).fillna(0)
    # PANOS: I do not fully understand the last line of your code due to many grouping you did,
    # but I can understand the exported df below, 
    # 1) we need to know the flow of pedestrians (peds/2minutes), as it happens with cars
    #Mariana: 1.This happens quite fine with the pd.Grouper, the only thing I did is that I aggregated 
    #         this operation for every 2 min starting at the first "minute" of the dataset (i.e. 9:57) 
    #         instead of a "round hour" (10:00). Then the last line is only to make into a "proper" table.
    # 2) if we define that one crossing is each time a pedestrian touches the circulation_zone,
    # then I think that we should measure the times the zone changes from safe_zone to circulation_zone
    # or the movement pattern.
    # Because, one pedestrian can make more than one crossings in the selected time interval; this impacts on traffic speed.
    #Mariana: 2. The thing is the "action" column already determines the movement the road user is taking, I've done that 
    #         before to facilitate my work - I did very similarly to how you described there, but also examining manually to avoid
    #         (too many) mistakes. But, we can of course create a new function to determine they leaving or entering
    #         each zone, which is quite simple since the column "zone" tell where they are. The crossing action means that
    #         the main behaviour was crossing (they of course might have also walked in the safe zone, but that is easy to detect
    #         combining the column "action" and "zone"). I don't remember the same ID crossing twice (but I can double check that),
    #         simply because most of the time they would go somewhere, get out of the "camera detection", then be
    #         detected again and consequently be assigned another ID. In real life they might have crossed multiple times,
    #         but then they were detected and assigned different IDs in the dataframe.
    # 3) Where are the cyclists; 
    #Mariana: 3. will (re)add them!
    d2=df[(df['user']=='car')].groupby([pd.Grouper(key='timestamp',freq='2min',label='right')]).agg({'id':'nunique','speed_local':'mean'}).rename(columns={'id':'n_cars', 'speed_local':'mean_speed'})
    df2 = pd.concat([d1, d2], axis=1).fillna(0)
    return df2

def subzones_count(df):
    d1=df[(df['user']=='person')&(df['zone']=='circulation_zone')&(df['subzone']!='-')&(df['action']!='walking_safe')].groupby(['subzone','action',pd.Grouper(key='timestamp',freq='2min',label='right')])['id'].nunique().unstack(1).reset_index().set_index(['timestamp','subzone']).fillna(0)
    d2=df[(df['user']=='car')&(df['zone']=='circulation_zone')&(df['subzone']!='-')].groupby(['subzone', pd.Grouper(key='timestamp',freq='2min',label='right')]).agg({'id':'nunique','speed_local':'mean'}).rename(columns={'id':'n_cars', 'speed_local':'mean_speed'}).reset_index().set_index(['timestamp','subzone'])
    #circulation_zone to avoid getting parking/parked cars
    df2 = pd.concat([d1, d2], axis=1).fillna(0)
    return df2

data1=count(fsbr)
# PANOS: this is the first dataset we can export
# ideally, it will contain time_stamp, n_pedestrians, n_cyclist, ped_crossing, mean_car_speed, mean_cycle_speed
# and mean_traffic speed
# add a separated column called case: fsbr, lsho, mke, nafp to have one unique dataset
# lastly we can connect it with design parametersN per case
# Mariana: yeah, that was my line of thought as well and it is quite "easy" to do so.
len(data1) # so 64 observations from this case, we can double them if we select one minute time interval
#data1
max(data1.n_cars)*30 # PANOS: 810 pcu/h is close to capacity. NICE NICE
# this street was very busy, I think it will be quite interesting to see the results of this one

def plot_zones(layout,zone_map):
    #random array of random colours for visual purposes 
    col=[[np.random.uniform(0, 1), np.random.uniform(0, 1), np.random.uniform(0, 1),0.2], 
         [np.random.uniform(0, 1), np.random.uniform(0, 1), np.random.uniform(0, 1),0.2], 
         [np.random.uniform(0, 1), np.random.uniform(0, 1), np.random.uniform(0, 1),0.2], 
         [np.random.uniform(0, 1), np.random.uniform(0, 1), np.random.uniform(0, 1),0.2], 
         [np.random.uniform(0, 1), np.random.uniform(0, 1), np.random.uniform(0, 1),0.2], 
         [np.random.uniform(0, 1), np.random.uniform(0, 1), np.random.uniform(0, 1),0.2], 
         [np.random.uniform(0, 1), np.random.uniform(0, 1), np.random.uniform(0, 1),0.2], 
         [np.random.uniform(0, 1), np.random.uniform(0, 1), np.random.uniform(0, 1),0.2], 
         [np.random.uniform(0, 1), np.random.uniform(0, 1), np.random.uniform(0, 1),0.2], 
         [np.random.uniform(0, 1), np.random.uniform(0, 1), np.random.uniform(0, 1),0.2], 
         [np.random.uniform(0, 1), np.random.uniform(0, 1), np.random.uniform(0, 1),0.2], 
         [np.random.uniform(0, 1), np.random.uniform(0, 1), np.random.uniform(0, 1),0.2], 
         [np.random.uniform(0, 1), np.random.uniform(0, 1), np.random.uniform(0, 1),0.2]]
    
    fig, ax = plt.subplots()
    ax.axis('off')
    img = plt.imread(layout)
    ax.imshow(img)
    for (i, pol), col in zip(zone_map.items(), col):
        if i!=(len(zone_map)):
            ax.fill(*pol.exterior.xy, facecolor=col, edgecolor='k', linewidth=1)
            x = pol.centroid.x #pol.exterior.xy[0][0]+(pol.exterior.xy[0][1] - pol.exterior.xy[0][0])/4
            y = pol.centroid.y #pol.exterior.xy[1][2]+(pol.exterior.xy[1][1] - pol.exterior.xy[1][2])/4
            ax.text(x, y, i, color='w', size=10)#, weight='bold')


def add_zones (df,zones):
    current=[]
    for index,row in df.iterrows():
        for i in (range(1, len(zones) + 1)):
            if (zones[i].contains(Point(row['coordx'], row['coordy']))) is True:
                current.append(i)
                break
    df['subzone']=current
    n=len(zones)
    df['subzone'].replace({n:'-'}, inplace=True)
    return df

#Frankfurter Straße, Bad Rothenfelde
#Manually collected from Inkscape based on layout drawing

z1=Polygon([(950.07938,1887.3774),
            (950.07938+17.04178,1887.3774-805.2242),
            (950.07938+17.04178+1785.12674,1887.3774-805.2242+132.0738),
            (950.07938+17.04178+1785.12674-42.6045,1887.3774-805.2242+132.0738+758.3593)])

z2=Polygon([(3510.6072,1256.8315),
            (3510.6072-12.7813,1256.8315+558.1183),
            (3510.6072-12.7813+2138.7437,1256.8315+558.1183+25.5627),
            (3510.6072-12.7813+2138.7437-42.6045,1256.8315+558.1183+25.5627-604.9833)])

total=Polygon([(374.91922,323.79387),
               (409.00279,2743.727),
               (6561.0863,2794.8524),
               (6561.0863,306.75209)])

zones_fsbr = {1: z1, 2: z2, 3:total}

plot_zones(layout_fsbr,zones_fsbr) 
# PANOS: we do not need to create new zones. You have already created: the safe zone, circulation zone etc and
# you have classified the movements. But How? Do you know the size of these zones: length, width, area?
# Mariana: The movements I described before, but in summary it was basically related to their position, leaving and entering
#          certain parts of the space. I think in the readme file I described a bit better.
#          About the zones, I can check the measurements. They were assigned based on the design, circulation zone 
#          where everyone share road space, safe zone where motor vehicle access is restricted (pedestrian safe zone basically)
#          and activity zone where there is area to linger (benches, fountains etc) - not applicable in street segments though
#          
# measure number of pedestrians within this two zones per 2 minutes (same timestamps as before),
# divide with the area to find density (ped/m2). 
# measure car density in circulation zone (car/m2 or car/m/direction) and bicycle density
# measure total traffic density (veh/m2 or veh/m/direction)
# then we will have a new dataset: timestamp: ped_dens_safe, ped_dens_circ, cycle_dens_circ, car_dens_circ, 
# traffic_dens_circ
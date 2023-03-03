amalias_vehicle_dataset_space.csv
#### Columns ####
vid: vehicle id
date: date of measurements
time: exact time vehicle detected
case: if shnaf, shared space section; else conventional road section
type: vehicle type (moto, car, truck, long)
speed: vehicle speed in km/h
ped: average pedestrian volume in peds/2 minutes
cross: average number of crossings of the circulation zone/traffic lane in cross/2 minutes
park: 1, if parking manuever the circulation zone/traffic lane
headway: time headway between two consecutive vehicle in seconds

amalias_pedestrian_dataset.csv
#### Columns ####
tid: time interval id
date: date of measuarements
time: exact time of measurement (end of the 2 minutes - time interval)
case: if shnaf, shared space section; else conventional road section
ped: pedestrian volume in the road environment in peds/2 minutes
cross: number of crossings of the circulation zone/traffic lane in cross/2 minutes
headway: mean headway between two consecutive vehicle in seconds

amalias_traffic_dataset.csv
#### Columns ####
tid: time interval id
date: date of measuarements
time: exact time of measurement (end of the 2 minutes - time interval)
ped: pedestrian volume in the road environment in peds/2 minutes
cross: number of crossing of the circulation zone/traffic lane in cross/2 minutes
headway: mean time headway between two consecutive vehicle in seconds
speed: mean vehicle speed in km/h
spcu: passenger car units for conversion
flow: vehicle flow in veh/h
case: if shnaf, shared space section; else conventional road section

production_attraction_points_shared_space.csv
#### Columns ####
ID: attraction/production point ID
name: attraction/production name
prod: VRU productions, i.e. peds/h (28 June 2021, 11:00-12:00)   
attr: VRU attractions, i.e  peds/h (28 June 2021, 11:00-12:00)
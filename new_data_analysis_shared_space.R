library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
gerdat<-read.csv2('datasets/datasets_germany.csv', header=T, dec=".", sep=",")
# fundamental diagrams
# density vs flow
ggplot(subset(gerdat, case=='fsbr'), aes(y=n_cars, x=car_dens_circ))+geom_point()+theme_bw()#+
  #geom_smooth(method=lm, color="red", aes(group=2))
ggplot(subset(gerdat, case=='lsho'), aes(y=n_cars, x=car_dens_circ))+geom_point()+theme_bw()#+
  #geom_smooth(method=lm, color="red", aes(group=2))
ggplot(subset(gerdat, case=='mke'), aes(y=n_cars, x=car_dens_circ))+geom_point()+theme_bw()#+
  #geom_smooth(method=lm, color="red", aes(group=2))

# PANOS: here we have three perfect liner relationships, I do not understand why? 
# because on y-axis we have number of cars passed from a cross section, while 
# in x-axis in the number of cars per kilometer AT THE MOMENT we take the measurement
# a linear relationship between flow and density means that the mean speed is constant, 
# I cannot see that in the data.

# density vs speed
ggplot(subset(gerdat, case=='fsbr'), aes(y=mean_car_speed, x=car_dens_circ))+geom_point()+theme_bw()+
  geom_smooth(method=lm, color="red", aes(group=2)) # PANOS: no difference, we have to check density data
ggplot(subset(gerdat, case=='lsho'), aes(y=mean_car_speed, x=car_dens_circ))+geom_point()+theme_bw()+
  geom_smooth(method=lm, color="red", aes(group=2))
ggplot(subset(gerdat, case=='mke'), aes(y=mean_car_speed, x=car_dens_circ))+geom_point()+theme_bw()+
  geom_smooth(method=lm, color="red", aes(group=2))

# flow vs speed
ggplot(subset(gerdat, case=='fsbr'), aes(x=30*n_cars, y=mean_car_speed))+geom_point()+theme_bw()+
  geom_smooth(method=lm, color="red", aes(group=2), formula='y ~ x') # small decrease of speed
max(subset(gerdat, case=='fsbr')$n_cars)*30 # 810 cars/h, close to capacity
cor.test(subset(gerdat, case=='fsbr')$n_cars, subset(gerdat, case=='fsbr')$mean_car_speed) # correlation -0.081, 
#but not significant for a confidence interval of 95%

ggplot(subset(gerdat, case=='lsho'), aes(x=30*n_cars, y=mean_car_speed))+geom_point()+theme_bw()+
  geom_smooth(method=lm, color="red", aes(group=2), formula='y ~ x') # no change
max(subset(gerdat, case=='lsho')$n_cars)*30 # 330 cars/h in mke, low flows, far from capacity
cor.test(subset(gerdat, case=='lsho')$n_cars, subset(gerdat, case=='lsho')$mean_car_speed) # no correlation

ggplot(subset(gerdat, case=='mke'), aes(x=30*n_cars, y=mean_car_speed))+geom_point()+theme_bw()+
  geom_smooth(method=lm, color="red", aes(group=2), formula='y ~ x') # increase of speed with higher flow
max(subset(gerdat, case=='mke')$n_cars)*30 # 270 cars/h in mke, low flows, far from capacity
cor.test(subset(gerdat, case=='mke')$n_cars, subset(gerdat, case=='mke')$mean_car_speed) # correlation + 0.1729
# but not significant for a confidence interval of 95%

# PANOS: conclusion: we cannot see correlation between speed and flow, as in most of the cases we are in the free flow branch
# in fsbr it seems that we are getting closer to capacity but not enough data to see the influence of car flow and speed
# I will test the deviations of speed from mean to check all the cases

mean(subset(gerdat, case=='fsbr')$mean_car_speed) # PANOS: higher mean speed in fsbr, remember that higher flows but no capacity
# therefore more cars higher free flow speed, this is logical
mean(subset(gerdat, case=='lsho')$mean_car_speed) 
mean(subset(gerdat, case=='mke')$mean_car_speed)

# HOT HOT HOT
ggplot() + geom_point(data=subset(gerdat, case=='fsbr'), aes(x=30*n_cars, y=mean_car_speed-mean(mean_car_speed)), color='blue')+
  geom_point(data = subset(gerdat, case=='lsho'), aes(x=30*n_cars, y=mean_car_speed-mean(mean_car_speed)), color='red') +
  geom_point(data = subset(gerdat, case=='mke'), aes(x=30*n_cars, y=mean_car_speed-mean(mean_car_speed)))+
  theme_bw()
# PANOS: super interesting: higher deviations of speed in mke, where the flow are low,
# speed deviations no relationship with flow, they are constant!!

# Check the relationship of speed deviations with pedestrian flow in circ and safe zone
ggplot() + geom_point(data=subset(gerdat, case=='fsbr'), aes(x=30*n_pedestrians, y=mean_car_speed-mean(mean_car_speed)), color='blue')+
  geom_point(data = subset(gerdat, case=='lsho'), aes(x=30*n_pedestrians, y=mean_car_speed-mean(mean_car_speed)), color='red') +
  geom_point(data = subset(gerdat, case=='mke'), aes(x=30*n_pedestrians, y=mean_car_speed-mean(mean_car_speed)))+
  theme_bw()

ggplot(gerdat, aes(y=mean_car_speed, x=n_pedestrians))+geom_point()+theme_bw()+
  geom_smooth(method=lm, color="red", aes(group=2)) # PANOS: positive relationship....

ggplot() + geom_point(data=subset(gerdat, case=='fsbr'), aes(x=30*ped_crossing, y=mean_car_speed-mean(mean_car_speed)), color='blue')+
  geom_point(data = subset(gerdat, case=='lsho'), aes(x=30*ped_crossing, y=mean_car_speed-mean(mean_car_speed)), color='red') +
  geom_point(data = subset(gerdat, case=='mke'), aes(x=30*ped_crossing, y=mean_car_speed-mean(mean_car_speed)))+
  theme_bw()

ggplot(gerdat, aes(y=mean_car_speed, x=ped_crossing))+geom_point()+theme_bw()+
  geom_smooth(method=lm, color="red", aes(group=2)) # PANOS: no relationship...

ggplot() + geom_point(data=subset(gerdat, case=='fsbr'), aes(x=30*ped_crossing, y=30*n_pedestrians), color='blue')+
  geom_point(data = subset(gerdat, case=='lsho'), aes(x=30*ped_crossing, y=30*n_pedestrians), color='red') +
  geom_point(data = subset(gerdat, case=='mke'), aes(x=30*ped_crossing, y=30*n_pedestrians))+
  theme_bw() # of course, significant relationship between flow and crossing of pedestrians


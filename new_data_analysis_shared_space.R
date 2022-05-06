library(ggplot2)
library(ggpubr)
library(corrplot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
gerdat<-read.csv2('datasets/datasets_germany_DESIGN.csv', header=T, dec=".", sep=",")
eldat<-read.csv2('datasets/amalias_traffic_dataset_DESIGN.csv', header=T, dec=".", sep=",")

corel=data.frame(ped = gerdat$n_pedestrians, flow = gerdat$n_cars,
                 cycle = gerdat$n_cyclists, car_speed = gerdat$mean_car_speed,
                 cycle_speed = gerdat$mean_car_speed, cross = gerdat$ped_crossing,
                 ped_d_circ = gerdat$ped_dens_circ, 
                 car_d_circ = gerdat$car_dens_circ)
M = cor(corel)
testRes=cor.mtest(corel, conf.level=0.95)
corrplot(M, p.mat = testRes$p, sig.level = 0.05, insig = "blank", diag=FALSE)

# Starting with fundamental diagrams
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
# no data in Greece about densities


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

ggplot(subset(eldat, shared==1), aes(x=60/headway, y=speed))+geom_point()+theme_bw()+ # Greece
  geom_smooth(method=lm, color="red", aes(group=2), formula='y ~ x') # no change, very low flows
max(1/subset(eldat, shared==1)$headway)*60 # 750 pcu/h in Nafplio, seems far from capacity
cor.test(60/subset(eldat, shared==1)$headway, subset(eldat, shared==1)$speed) # no significant correlation

# PANOS: conclusion: we cannot see correlation between speed and flow, as in most of the cases we are in the free flow branch
# in fsbr it seems that we are getting closer to capacity but not enough data to see the influence of car flow and speed
# I will test the deviations of speed from mean to check all the cases

mean(subset(gerdat, case=='fsbr')$mean_car_speed) # PANOS: higher mean speed in fsbr, remember that higher flows but no capacity
# therefore more cars higher free flow speed, this is logical
mean(subset(gerdat, case=='lsho')$mean_car_speed) 
mean(subset(gerdat, case=='mke')$mean_car_speed)

# HOT HOT HOT
# create a new dataset, with all shared space sections...
x='fsbr'
traf<-data.frame(case=x, flow=30*subset(gerdat, case==x)$n_cars, 
                 dspeed = subset(gerdat, case==x)$mean_car_speed - mean(subset(gerdat, case==x)$mean_car_speed),
                 ped = 30*subset(gerdat, case==x)$n_pedestrians,
                 cross = 30*subset(gerdat, case==x)$ped_crossing,
                 shared = subset(gerdat, case==x)$shared,
                 oneway = subset(gerdat, case==x)$oneway,
                 park = subset(gerdat, case==x)$park,
                 mark = subset(gerdat, case==x)$mark,
                 trees = subset(gerdat, case==x)$trees,
                 bollards = subset(gerdat, case==x)$bollards, 
                 width = subset(gerdat, case==x)$width)
x='lsho'
traf<-rbind(traf, data.frame(case=x, flow=30*subset(gerdat, case==x)$n_cars, 
                             dspeed = subset(gerdat, case==x)$mean_car_speed - mean(subset(gerdat, case==x)$mean_car_speed),
                             ped = 30*subset(gerdat, case==x)$n_pedestrians,
                             cross = 30*subset(gerdat, case==x)$ped_crossing,
                             shared = subset(gerdat, case==x)$shared,
                             oneway = subset(gerdat, case==x)$oneway,
                             park = subset(gerdat, case==x)$park,
                             mark = subset(gerdat, case==x)$mark,
                             trees = subset(gerdat, case==x)$trees,
                             bollards = subset(gerdat, case==x)$bollards, 
                             width = subset(gerdat, case==x)$width))
x='mke'
traf<-rbind(traf, data.frame(case=x, flow=30*subset(gerdat, case==x)$n_cars, 
                             dspeed = subset(gerdat, case==x)$mean_car_speed - mean(subset(gerdat, case==x)$mean_car_speed),
                             ped = 30*subset(gerdat, case==x)$n_pedestrians,
                             cross = 30*subset(gerdat, case==x)$ped_crossing,
                             shared = subset(gerdat, case==x)$shared,
                             oneway = subset(gerdat, case==x)$oneway,
                             park = subset(gerdat, case==x)$park,
                             mark = subset(gerdat, case==x)$mark,
                             trees = subset(gerdat, case==x)$trees,
                             bollards = subset(gerdat, case==x)$bollards, 
                             width = subset(gerdat, case==x)$width))
x='shnaf'
traf<-rbind(traf, data.frame(case=x, flow=60/subset(eldat, shared==1)$headway, # Add Nafplio data
                             dspeed = subset(eldat, shared==1)$speed - mean(subset(eldat, shared==1)$speed),
                             ped = 30*subset(eldat, shared==1)$ped,
                             cross = 30*subset(eldat, shared==1)$cross,
                             shared = subset(eldat, case==x)$shared,
                             oneway = subset(eldat, case==x)$oneway,
                             park = subset(eldat, case==x)$park,
                             mark = subset(eldat, case==x)$mark,
                             trees = subset(eldat, case==x)$trees,
                             bollards = subset(eldat, case==x)$bollards, 
                             width = subset(eldat, case==x)$width))

x='covnaf'
traf<-rbind(traf, data.frame(case=x, flow=60/subset(eldat, shared==0)$headway, # Add Nafplio data
                             dspeed = subset(eldat, shared==0)$speed - mean(subset(eldat, shared==1)$speed),
                             ped = 30*subset(eldat, shared==0)$ped,
                             cross = 30*subset(eldat, shared==0)$cross,
                             shared = subset(eldat, case==x)$shared,
                             oneway = subset(eldat, case==x)$oneway,
                             park = subset(eldat, case==x)$park,
                             mark = subset(eldat, case==x)$mark,
                             trees = subset(eldat, case==x)$trees,
                             bollards = subset(eldat, case==x)$bollards, 
                             width = subset(eldat, case==x)$width))

corel = subset(traf, select = -case)
M = cor(corel)
testRes=cor.mtest(corel, conf.level=0.95)
corrplot(M, method = 'number', p.mat = testRes$p, sig.level = 0.05, insig = "blank", diag=FALSE)

p1<-ggplot(traf, aes(x=flow/30, y=dspeed)) + geom_point(size = 1.5, alpha = .3) + 
  geom_smooth(method=lm, color="red", aes(group=2)) + 
  scale_y_continuous(name ="Deviation from mean speed in km/h", limits=c(-18,18))+
  scale_x_continuous(name ="Car flow in cars/2 minutes")+
  theme_bw()
cor.test(traf$flow,traf$dspeed) # no significant correlation
cor.test(subset(traf, flow>=700)$flow, subset(traf, flow>700)$dspeed) # no correlation even in 700 cars/h, free flow conditions
# with constant speed, so we can assume a triangular fundamental diagram...

p2<-ggplot(traf, aes(y=dspeed, x=as.factor(cross/30))) +
  geom_boxplot() +   geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
  scale_x_discrete(name ="Pedestrian crossings in cross/2 minutes", limits=c("0","1","2","3","4","5","6","7","8","9","10")) +
  scale_y_continuous(name ="Deviation from mean speed in km/h", limits=c(-18,18)) + geom_smooth(method=lm, color="red", aes(group=2))
cor.test(traf$cross,traf$dspeed) # significant NEGATIVE correlation, more crossings lower speed

p3<-ggplot(traf, aes(y=flow/30, x=as.factor(cross/30))) +
  geom_boxplot() +   geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
  scale_x_discrete(name ="Pedestrian crossings in cross/2 minutes", limits=c("0","1","2","3","4","5","6","7","8","9","10")) +
  scale_y_continuous(name ="Car flow in cars/2 minutes") + geom_smooth(method=lm, color="red", aes(group=2))
cor.test(traf$cross,traf$flow) # very significant correlation, less traffic flow, then more crossings

p4<-ggplot(traf, aes(y=ped/30, x=as.factor(cross/30))) +
  geom_boxplot() +   geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
  scale_x_discrete(name ="Pedestrian crossings in cross/2 minutes", limits=c("0","1","2","3","4","5","6","7","8","9","10")) +
  scale_y_continuous(name ="Pedestrian flow in peds/2 minutes") + geom_smooth(method=lm, color="red", aes(group=2))
cor.test(traf$cross,traf$ped) # very significant correlation, more pedestrians, then more crossings

ggarrange(p1,p2,p3,p4, ncol=4)
# PANOS: so in conclusion, more pedestrians in the road environment
# means ok more crossing, which is expected. But, lower traffic flows,
# increase the number of crossings. Then, traffic speed are reduced, but
# traffic speeds are not affected by flows.

x='fsbr'
cycl<-data.frame(case=x, flow=30*subset(gerdat, case==x)$n_cars, 
                 dspeed = subset(gerdat, case==x)$mean_car_speed - mean(subset(gerdat, case==x)$mean_car_speed),
                 cycle = 30*subset(gerdat, case==x)$n_cyclists)
x='lsho'
cycl<-rbind(cycl, data.frame(case=x, flow=30*subset(gerdat, case==x)$n_cars, 
                             dspeed = subset(gerdat, case==x)$mean_car_speed - mean(subset(gerdat, case==x)$mean_car_speed),
                             cycle = 30*subset(gerdat, case==x)$n_cyclists))
x='mke'
cycl<-rbind(cycl, data.frame(case=x, flow=30*subset(gerdat, case==x)$n_cars, 
                             dspeed = subset(gerdat, case==x)$mean_car_speed - mean(subset(gerdat, case==x)$mean_car_speed),
                             cycle = 30*subset(gerdat, case==x)$n_cyclists))

ggplot(cycl, aes(y=dspeed, x=as.factor(cycle/30))) +
  geom_boxplot() +   geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
  scale_x_discrete(name ="Pedestrian crossings in cross/2 minutes", limits=c("0","1","2","3","4","5","6","7")) +
  scale_y_continuous(name ="Pedestrian flow in peds/2 minutes") + geom_smooth(method=lm, color="red", aes(group=2))
cor.test(cycl$dspeed,cycl$cycle) # no significant correlation, it is not related with the presence of cyclists

x='fsbr'
dens<-data.frame(case=x, dspeed=subset(gerdat, case==x)$mean_car_speed - mean(subset(gerdat, case==x)$mean_car_speed),
                 cross_d=subset(gerdat, case==x)$ped_dens_circ,
                 cross=subset(gerdat, case==x)$ped_crossing)
x='lsho'
dens<-rbind(dens, data.frame(case=x, dspeed=subset(gerdat, case==x)$mean_car_speed - mean(subset(gerdat, case==x)$mean_car_speed),
                 cross_d=subset(gerdat, case==x)$ped_dens_circ,
                  cross=subset(gerdat, case==x)$ped_crossing))
x='mke'
dens<-rbind(dens, data.frame(case=x, dspeed=subset(gerdat, case==x)$mean_car_speed - mean(subset(gerdat, case==x)$mean_car_speed),
                             cross_d=subset(gerdat, case==x)$ped_dens_circ,
                             cross=subset(gerdat, case==x)$ped_crossing))

ggplot(dens, aes(x=cross_d, y=dspeed)) + geom_point(size = 1.5, alpha = .3) + 
  geom_smooth(method=lm, color="red", aes(group=2)) + 
  scale_y_continuous(name ="Deviation from mean speed in km/h", limits=c(-18,18))+
  scale_x_continuous(name ="Car flow in cars/2 minutes")+
  theme_bw()

cor.test(dens$dspeed,dens$cross_d) # not significant for a 95% confidence interval, SPEED are affected by the crossing rate,
cor.test(traf$cross,traf$dspeed)

# not the number of crossings points, check the study of Knopp
cor.test(dens$cross,dens$cross_d) # Again very powerful relationship, densities are not correct, so the last conclusion is not correct

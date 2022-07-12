library(ggplot2)
library(ggpubr)
library(corrplot)
library(gvlma)
library(olsrr)
library(RColorBrewer)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
gerdat<-read.csv2('datasets/datasets_germany_DESIGN.csv', header=T, dec=".", sep=",")
eldat<-read.csv2('datasets/amalias_traffic_dataset_DESIGN.csv', header=T, dec=".", sep=",")

# NEW DATASET TO PLAY WITH
datafr_fun <- function(df, x, dats){
  if(dats == 'dats1'){datafr<-data.frame(case=x, flow=30*subset(df, case==x)$n_cars + 30*subset(df, case==x)$n_cyclists,
                                         # all vehicles in veh/h, no pcu was used...
                                         # cyclists cannot be considered separately
                                         # since no cyclists recorded in Nafplio, Greece
                                         dspeed = subset(df, case==x)$mean_car_speed - mean(subset(df, case==x)$mean_car_speed),
                                         # deviation from the mean speed.
                                         lspeed = subset(df, case==x)$mean_car_speed - 15,
                                         # difference from the speed limit, compliance of drivers
                                         comply = subset(df, case==x)$mean_car_speed/15,
                                         ped = 30*subset(df, case==x)$n_pedestrians,
                                         # flow of pedestrians per hour
                                         cross = 30*subset(df, case==x)$ped_crossing)} 
  
  if(dats == 'dats2'){datafr<-data.frame(case=x, flow=60/subset(df, case==x)$headway,
                                         # calculation flows from headways
                                         dspeed = subset(df, case==x)$speed - mean(subset(df, case==x)$speed),
                                         lspeed = subset(df, case==x)$speed - 30, # there is no speed limit, suppose 30
                                         comply = subset(df, case==x)$speed/30,
                                         ped = 30*subset(df, case==x)$ped,
                                         cross = 30*subset(df, case==x)$cross)}
  
  datafr<-cbind(datafr, data.frame(shared = subset(df, case==x)$shared,
                                    # 1, if shared space, only one section
                                    oneway = subset(df, case==x)$oneway,
                                    # 1, if oneway road
                                    # park = subset(df, case==x)$park, # no use, all section with parking
                                    mark = subset(df, case==x)$mark,
                                    # 1, if road markings
                                    trees = subset(df, case==x)$trees,
                                    # 1, if trees
                                    bollards = subset(df, case==x)$bollards,
                                    # 1, if bollards
                                    width = subset(df, case==x)$width,
                                    # width of the pavement
                                    cross_y = subset(df, case==x)$cross_y,
                                    # 1, existence of courtesy crossings
                                    s_width = subset(df, case==x)$s_width,
                                    # 1, sidewalk width
                                    # inters = subset(df, case==x)$inters, no use of this variable
                                    bench = subset(df, case==x)$bench,
                                    # 1, if benches
                                    obst = subset(df, case==x)$obst,
                                    # 1, if obstacles
                                    lev_segr = subset(df, case==x)$lev_segr)) # 1, if level segregatioN
  return(datafr)}

traf<-datafr_fun(gerdat,'fsbr', 'dats1')
traf<-rbind(traf, datafr_fun(gerdat,'lsho', 'dats1'))
traf<-rbind(traf, datafr_fun(gerdat,'mke', 'dats1'))
traf<-rbind(traf, datafr_fun(eldat,'shnaf','dats2'))
# traf<-rbind(traf, datafr_fun(eldat,'covnaf', 'dats2')) # no conventional section imported in the aanalyis

# CORRELATIONS
corr<-function(df, x){ # plot correlation table funntion
  corel = df
  M = cor(corel)
  testRes=cor.mtest(corel, conf.level=x)
  corrplot(M, method = 'number', p.mat = testRes$p, sig.level = 1-x, diag=FALSE)} 

corr(subset(traf, select=(-case)), 0.95)
# with X no significant correlations
# no correlation between speed deviation and crossing or flow
# but compliance have some interesting correlations...correlation with crossiing, not with flows.
# positive correlation with flow (????), high speed when we have high flow ??
# maybe free flow branch and car dominance lead to higher speeds..
# positive and significant correlation with road width and courtesy crossings, positive
# in crossing, super strong correlation with ped and shared

# DESCRIPTIVE STATISTICS
traf$pedcross<-traf$cross/traf$ped # pedestrian crossings per pedestrian (relative number)

# A. descriptive statistics..!
shapiro.test(traf$lspeed) # no normality
shapiro.test(traf$dspeed) # no normality
shapiro.test(traf$cross) # no normality

timeplot<-function(df,street,text){ # time series plot, relationship between dspeed, lspeed, pedcross
  p<-ggplot(subset(df, case==street)) +
    geom_line(aes(x = 1:nrow(subset(df, case==street)), y = dspeed, color = 'Deviation from mean traffic speed'), size=0.6, linetype='dashed') + 
    geom_line(aes(x = 1:nrow(subset(df, case==street)), y = lspeed, color = 'Deviation from speed limit'), size=0.6) +
    geom_line(aes(x = 1:nrow(subset(df, case==street)), y = (pedcross-0.25)*20, color = 'Crossings per pedestrian'), size=0.6) +
    scale_color_manual(name = '', values = c('Deviation from mean traffic speed'='brown1',
                                             'Deviation from speed limit'='black',
                                             'Crossings per pedestrian'='blue2')) +
    scale_x_continuous(name = 'Two minutes intervals', breaks=seq(0,nrow(subset(df, case==street)),5)) + 
    scale_y_continuous(name = 'Speed deviations in km/h', limits = c(-25,25),
                       sec.axis = sec_axis( trans=~ ./20 + 0.25, name="Crossings per pedestrian")) + 
    ggtitle(text) + theme_bw() + theme(legend.position="bottom")
return(p)}

ggarrange(timeplot(traf,'fsbr','Frankfurter Straße, Bad Rothenfelde, Germany'),
          timeplot(traf,'lsho','Lange Straße, Hessisch Oldendorf, Germany'),
          timeplot(traf,'mke','Marktplatz, Königslutter am Elm, Germany'),
          timeplot(traf,'shnaf','Amalias Street, Nafplio, Greece'), ncol=1, nrow=4,
          common.legend = TRUE, legend="bottom") 
# interesting comparisons comparisons in the last graph...based on the cofigurations of each road.

p1<-ggplot(traf, aes(x=as.factor(case), y = pedcross, fill=case)) + geom_boxplot() + 
   geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) +
   scale_x_discrete(name = 'Case', labels = c('Frankfurter Straße', 'Lange Straße',
                                            'Marktplatz', 'Amalias Street')) +
   scale_y_continuous(name = 'Number of crossings per pedestrian', limits=c(0,1.5)) +
   scale_fill_brewer(palette = 'Pastel2')+ theme_bw() + theme(legend.position = "none")

p2<-ggplot(traf, aes(x=as.factor(case), y = comply, fill=case)) + geom_boxplot() + 
   geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) +
   scale_x_discrete(name = 'Case', labels = c('Frankfurter Straße', 'Lange Straße',
                                             'Marktplatz', 'Amalias Street')) +
   scale_y_continuous(name = 'Speed compliance rate') +
   scale_fill_brewer(palette = 'Pastel2')+ theme_bw() + theme(legend.position = "none")

ggarrange(p1,p2, ncol=1, nrow=2)
# end up with two important parameters: compliance rate and crossings per pedestrian
# two different forces, comparison and analysis.

# DESIGN VS COMPLY AND PEDCROSS
boxpl_fun<-function(df, x, y1, y2, text){
  p1<-ggplot(df, aes(as.factor({{x}}), {{y1}})) + geom_boxplot( fill = "darkseagreen2") +
    geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
    scale_x_discrete(name = text, limits=c("0","1")) +
    scale_y_continuous(name ="Number of crossings per pedestrian")
  p2<-ggplot(df, aes(as.factor({{x}}), {{y2}})) + geom_boxplot( fill = "lightblue1") +
    geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
    scale_x_discrete(name = text, limits=c("0","1")) +
    scale_y_continuous(name ="Deviation of traffic speed from speed limit in km/h")
  ggarrange(p1, p2, ncol=2, nrow=1)
} # box plot function to see visually the impact of each road design feature

# Bollards, yer or no
mean(subset(traf, bollards == 1)$pedcross) - mean(subset(traf, bollards == 0)$pedcross) # 0.10 more crossings per pedestrian
wilcox.test(subset(traf, bollards == 1)$pedcross, subset(traf, bollards == 0)$pedcross, exact=FALSE, paired=FALSE) # significance
mean(subset(traf, bollards == 1)$comply) - mean(subset(traf, bollards == 0)$comply)
wilcox.test(subset(traf, bollards == 1)$comply, subset(traf, bollards == 0)$comply, exact=FALSE, paired=FALSE)
# statistically significant difference for 95% confidence interval
boxpl_fun(traf, bollards, pedcross, comply, '1, if there are bollards')

# Zebra crossing, yes or no
mean(subset(traf, cross_y == 1)$pedcross) - mean(subset(traf, cross_y == 0)$pedcross) # 0.09 less crossings
wilcox.test(subset(traf, cross_y == 1)$pedcross, subset(traf, cross_y == 0)$pedcross, exact=FALSE, paired=FALSE) # significance
mean(subset(traf, cross_y == 1)$comply) - mean(subset(traf, cross_y == 0)$comply) 
wilcox.test(subset(traf, cross_y == 1)$comply, subset(traf, cross_y == 0)$comply, exact=FALSE, paired=FALSE) # significance
boxpl_fun(traf, cross_y, pedcross, comply, '1, if there are zebra pedestrian crossings')

# Trees, yes or no
mean(subset(traf, trees == 1)$pedcross) - mean(subset(traf, trees == 0)$pedcross) # -0.133 less crossings
wilcox.test(subset(traf, trees == 1)$pedcross, subset(traf, trees == 0)$pedcross, exact=FALSE, paired=FALSE) # significance
mean(subset(traf, trees == 1)$comply) - mean(subset(traf, trees == 0)$comply)
wilcox.test(subset(traf, trees == 1)$comply, subset(traf, trees == 0)$comply, exact=FALSE, paired=FALSE) # significance
boxpl_fun(traf, trees, pedcross, comply, '1, if there are trees')

# Level_segregation, yes or no
mean(subset(traf, lev_segr == 1)$pedcross) - mean(subset(traf, lev_segr == 0)$pedcross) # 0.133 less crossings
wilcox.test(subset(traf, lev_segr == 1)$pedcross, subset(traf, lev_segr == 0)$pedcross, exact=FALSE, paired=FALSE) # significance
mean(subset(traf, lev_segr == 1)$comply) - mean(subset(traf, lev_segr == 0)$comply)
wilcox.test(subset(traf, lev_segr == 1)$comply, subset(traf, lev_segr == 0)$comply, exact=FALSE, paired=FALSE) # significance
boxpl_fun(traf, lev_segr, pedcross, comply, '1, if there is level segregation')

# Bench, yes or no
mean(subset(traf, bench == 1)$pedcross) - mean(subset(traf, bench == 0)$pedcross) # -0.059 more crossings
wilcox.test(subset(traf, bench == 1)$cross, subset(traf, bench == 0)$cross, exact=FALSE, paired=FALSE) # no significance here
mean(subset(traf, bench == 1)$comply) - mean(subset(traf, bench == 0)$comply)
wilcox.test(subset(traf, bench == 1)$comply, subset(traf, bench == 0)$comply, exact=FALSE, paired=FALSE) # significance
boxpl_fun(traf, bench, pedcross, comply, '1, if there are benches')

# Obstacles, yes or no
mean(subset(traf, obst == 1)$pedcross) - mean(subset(traf, obst == 0)$pedcross) # -0.133 more crossings
wilcox.test(subset(traf, obst == 1)$cross, subset(traf, obst == 0)$cross, exact=FALSE, paired=FALSE) # significance
mean(subset(traf, obst == 1)$comply) - mean(subset(traf, obst == 0)$comply)
wilcox.test(subset(traf, obst == 1)$comply, subset(traf, obst == 0)$comply, exact=FALSE, paired=FALSE) # significance
boxpl_fun(traf, obst, pedcross, comply, '1, if there are obstacles')


# MODELING, new models about compliance and crossings
# new variables....
traf$shvehspace<-traf$width/(2*traf$s_width + traf$width) # percentage of veh space over all 
traf$shpedspace<-(2*traf$s_width)/(2*traf$s_width + traf$width) # percentage of ped space over all

traf$shped<-traf$ped/(traf$flow + traf$ped) # percentage of pedestrians flow over all moving objects
traf$shveh<-traf$flow/(traf$flow + traf$ped) # percentage of vehicles flow over all moving objects

corr(subset(traf, select=c(cross, comply, shped, shveh, shvehspace, shpedspace)), 0.95)

traf$cross2<-sqrt(traf$cross) # transformation of crossing rate, to solve linearity problem
traf$pedcross2<-1/exp(traf$pedcross)
model1<-lm(pedcross2 ~ shped + shpedspace + cross_y,
           data=traf[-c(228, 226, 31, 196, 59, 255, 223, 70),])
summary(model1)
gvlma(model1) 
par(mfrow=c(2,2)) # all linear regression assumptions were met
plot(model1, pch=20)
ols_vif_tol(model1) # no multicolinearities

model2<-lm(comply ~ shped + shpedspace + lev_segr, data=traf[-c(250, 170, 137, 188, 167, 185, 160,
                                                                152, 150, 141, 259, 200, 191, 176, 157, 66, 148,
                                                                174, 154, 153),])
summary(model2)
gvlma(model2) 
par(mfrow=c(2,2)) # all linear regression assumptions were met 
plot(model2, pch=20)
ols_vif_tol(model2) # no multicolinearities











# OLD CODE I DID NOT EXTEND THIS ANALYSIS TO FUNDAMENTAL DIAGRAMS



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

traf<-datafr_fun(gerdat,'fsbr', 'dats1')
traf<-rbind(traf, datafr_fun(gerdat,'lsho', 'dats1'))
traf<-rbind(traf, datafr_fun(gerdat,'mke', 'dats1'))
traf<-rbind(traf, datafr_fun(eldat,'shnaf','dats2'))
traf<-rbind(traf, datafr_fun(eldat,'covnaf', 'dats2'))

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

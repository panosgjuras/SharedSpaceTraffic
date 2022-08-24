library(ggplot2)
library(ggpubr)
library(corrplot)
library(gvlma)
library(olsrr)
library(RColorBrewer)
library(psych)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
gerdat<-read.csv2('datasets/datasets_germany_DESIGN.csv', header=T, dec=".", sep=",")
eldat<-read.csv2('datasets/amalias_traffic_dataset_DESIGN.csv', header=T, dec=".", sep=",")

# 

# NEW DATASET TO PLAY WITH
datafr_fun <- function(df, x, dats, limit){
  if(dats == 'dats1'){datafr<-data.frame(case=x, flow=30*subset(df, case==x)$n_cars + 30*subset(df, case==x)$n_cyclists,
                                         # all vehicles in veh/h, no pcu was used...
                                         # cyclists cannot be considered separately
                                         # since no cyclists recorded in Nafplio, Greece
                                         speed = subset(df, case==x)$mean_car_speed,
                                         dspeed = subset(df, case==x)$mean_car_speed - mean(subset(df, case==x)$mean_car_speed),
                                         # deviation from the mean speed.
                                         lspeed = subset(df, case==x)$mean_car_speed - limit,
                                         # difference from the speed limit, compliance of drivers
                                         comply = subset(df, case==x)$mean_car_speed/limit,
                                         ped = 30*subset(df, case==x)$n_pedestrians,
                                         # flow of pedestrians per hour
                                         cross = 30*subset(df, case==x)$ped_crossing)} 
  
  if(dats == 'dats2'){datafr<-data.frame(case=x, flow=60/subset(df, case==x)$headway,
                                         # calculation flows from headways
                                         speed = subset(df, case==x)$speed,
                                         dspeed = subset(df, case==x)$speed - mean(subset(df, case==x)$speed),
                                         lspeed = subset(df, case==x)$speed - limit, # there is no speed limit, suppose 30
                                         comply = subset(df, case==x)$speed/limit,
                                         ped = 30*subset(df, case==x)$ped,
                                         cross = 30*subset(df, case==x)$cross)}
  
  datafr<-cbind(datafr, data.frame(shared = subset(df, case==x)$shared,
                                    # 1, if shared space, only one section
                                    oneway = subset(df, case==x)$oneway,
                                    lanes = subset(df, case==x)$lanes,
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
                                    lev_segr = subset(df, case==x)$lev_segr),
                                    vis_segr = subset(df, case==x)$vis_segr) # 1, if level segregatioN
  return(datafr)}

traf<-datafr_fun(gerdat,'fsbr', 'dats1', 20) # fsbr 20 km/h
traf<-rbind(traf, datafr_fun(gerdat,'lsho', 'dats1', 10)) # 10 km/h
traf<-rbind(traf, datafr_fun(gerdat,'mke', 'dats1', 20)) # 20 km/h
traf<-rbind(traf, datafr_fun(eldat,'shnaf','dats2', 30)) # # 30 km/h
traf$comply<-replace(traf$comply, traf$comply==0, 1)


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
traf$shpedspace<-(2*traf$s_width)/(2*traf$s_width + traf$width) # percentage of ped space over all
traf$shped<-traf$ped/(traf$flow + traf$ped)

# A. descriptive statistics..!
describe(subset(traf, case=='fsbr')$speed)
describe(subset(traf, case=='lsho')$speed)
describe(subset(traf, case=='mke')$speed)
describe(subset(traf, case=='shnaf')$speed)

describe(subset(traf, case=='fsbr')$shpedspace)
describe(subset(traf, case=='lsho')$shpedspace)
describe(subset(traf, case=='mke')$shpedspace)
describe(subset(traf, case=='shnaf')$shpedspace)

describe(subset(traf, case=='fsbr')$ped/30)
describe(subset(traf, case=='lsho')$ped/30)
describe(subset(traf, case=='mke')$ped/30)
describe(subset(traf, case=='shnaf')$ped/30)

describe(subset(traf, case=='fsbr')$flow/30)
describe(subset(traf, case=='lsho')$flow/30)
describe(subset(traf, case=='mke')$flow/30)
describe(subset(traf, case=='shnaf')$flow/30)

describe(subset(traf, case=='fsbr')$cross/30)
describe(subset(traf, case=='lsho')$cross/30)
describe(subset(traf, case=='mke')$cross/30)
describe(subset(traf, case=='shnaf')$cross/30)

describe(subset(traf, case=='fsbr')$shped)
describe(subset(traf, case=='lsho')$shped)
describe(subset(traf, case=='mke')$shped)
describe(subset(traf, case=='shnaf')$shped)

describe(subset(traf, case=='fsbr')$pedcross)
describe(subset(traf, case=='lsho')$pedcross)
describe(subset(traf, case=='mke')$pedcross)
describe(subset(traf, case=='shnaf')$pedcross)

describe(subset(traf, case=='fsbr')$dspeed)
describe(subset(traf, case=='lsho')$dspeed)
describe(subset(traf, case=='mke')$dspeed)
describe(subset(traf, case=='shnaf')$dspeed)

describe(subset(traf, case=='fsbr')$comply)
describe(subset(traf, case=='lsho')$comply)
describe(subset(traf, case=='mke')$comply)
describe(subset(traf, case=='shnaf')$comply)

traf$flow3<-traf$flow/(traf$lanes*30)
traf$ped2<-traf$ped/30
traf$vehped<-traf$flow/traf$ped
traf$l_width<-traf$width/traf$lanes
traf$spratio<-traf$l_width/traf$s_width
corr(select(traf, c('shpedspace', 'spratio', 'ped', 'flow', 'cross','pedcross', 'comply', 'dspeed')), 0.95)


mean(subset(traf, lanes==2)$pedcross)-mean(subset(traf, lanes==1)$pedcross)
wilcox.test(subset(traf, lanes==2)$pedcross, 
            subset(traf, lanes==1)$pedcross, 
            exact=FALSE, paired=FALSE)

mean(subset(traf, lanes==2)$comply)-mean(subset(traf, lanes==1)$comply)
wilcox.test(subset(traf, lanes==2)$comply, 
            subset(traf, lanes==1)$comply, 
            exact=FALSE, paired=FALSE)

mean(subset(traf, vis_segr==1)$pedcross)-mean(subset(traf, vis_segr==0)$pedcross)
wilcox.test(subset(traf, vis_segr==1)$pedcross, 
            subset(traf, vis_segr==0)$pedcross, 
            exact=FALSE, paired=FALSE)

mean(subset(traf, lev_segr==1)$pedcross)-mean(subset(traf, lev_segr==0)$pedcross)
wilcox.test(subset(traf, lev_segr==1)$pedcross, 
            subset(traf, lev_segr==0)$pedcross, 
            exact=FALSE, paired=FALSE)

mean(subset(traf, cross_y==1)$pedcross)-mean(subset(traf, cross_y==0)$pedcross)
wilcox.test(subset(traf, cross_y==1)$pedcross, 
            subset(traf, cross_y==0)$pedcross, 
            exact=FALSE, paired=FALSE)

mean(subset(traf, bollards==1)$pedcross)-mean(subset(traf, bollards==0)$pedcross)
wilcox.test(subset(traf, bollards==1)$pedcross, 
            subset(traf, bollards==0)$pedcross, 
            exact=FALSE, paired=FALSE)

mean(subset(traf, bench==1)$pedcross)-mean(subset(traf, bench==0)$pedcross)
wilcox.test(subset(traf, bench==1)$pedcross, 
            subset(traf, bench==0)$pedcross, 
            exact=FALSE, paired=FALSE)

mean(subset(traf, vis_segr==1)$comply)-mean(subset(traf, vis_segr==0)$comply)
wilcox.test(subset(traf, vis_segr==1)$comply, 
            subset(traf, vis_segr==0)$comply, 
            exact=FALSE, paired=FALSE)

mean(subset(traf, lev_segr==1)$comply)-mean(subset(traf, lev_segr==0)$comply)
wilcox.test(subset(traf, lev_segr==1)$comply, 
            subset(traf, lev_segr==0)$comply, 
            exact=FALSE, paired=FALSE)

mean(subset(traf, cross_y==1)$comply)-mean(subset(traf, cross_y==0)$comply)
wilcox.test(subset(traf, cross_y==1)$comply, 
            subset(traf, cross_y==0)$comply, 
            exact=FALSE, paired=FALSE)

mean(subset(traf, bollards==1)$comply)-mean(subset(traf, bollards==0)$comply)
wilcox.test(subset(traf, bollards==1)$comply, 
            subset(traf, bollards==0)$comply, 
            exact=FALSE, paired=FALSE)

mean(subset(traf, bench==1)$comply)-mean(subset(traf, bench==0)$comply)
wilcox.test(subset(traf, bench==1)$comply, 
            subset(traf, bench==0)$comply, 
            exact=FALSE, paired=FALSE)


shapiro.test(traf$ped/30) # no normality
shapiro.test(traf$flow/30) # no normality
shapiro.test(traf$lspeed) # no normality
shapiro.test(traf$dspeed) # no normality
shapiro.test(traf$pedcross) # no normality


timeplot<-function(df,street,text){ # time series plot, relationship between dspeed, lspeed, pedcross
  p<-ggplot(subset(df, case==street)) +
    geom_line(aes(x = 1:nrow(subset(df, case==street)), y = dspeed, color = 'Deviation from mean traffic speed'), size=0.75, linetype='solid') + 
    # geom_line(aes(x = 1:nrow(subset(df, case==street)), y = comply*20-0.25, color = 'Deviation from speed limit'), size=0.6) +
    geom_line(aes(x = 1:nrow(subset(df, case==street)), y = (pedcross*20) - 6, color = 'Crossings per pedestrian'), size=0.75) +
    scale_color_manual(name = '', values = c('Deviation from mean traffic speed'='red',
                                             #'Deviation from speed limit'='black',
                                             'Crossings per pedestrian'='blue2')) +
    scale_x_continuous(name = 'Two minutes intervals', breaks=seq(0,nrow(subset(df, case==street)),5)) + 
    scale_y_continuous(name = 'Speed deviations in km/h', limits = c(-25,25),
                       sec.axis = sec_axis( trans=~ (./20) + 0.3, name="Pedestrian crossing rate")) + 
    ggtitle(text) + theme_bw() + theme(legend.position="bottom")
return(p)}

timeplot2<-function(df,street,text){ # time series plot, relationship between dspeed, lspeed, pedcross
  p<-ggplot(subset(df, case==street)) +
    # geom_line(aes(x = 1:nrow(subset(df, case==street)), y = dspeed, color = 'Deviation from mean traffic speed'), size=0.75, linetype='solid') + 
    geom_line(aes(x = 1:nrow(subset(df, case==street)), y = comply, color = 'Compliance rate'), size=0.75) +
    geom_line(aes(x = 1:nrow(subset(df, case==street)), y = pedcross + 0.5, color = 'Crossings per pedestrian'), size=0.75) +
    scale_color_manual(name = '', values = c('Compliance rate'='green',
                                             'Crossings per pedestrian'='blue2')) +
    scale_x_continuous(name = 'Two minutes intervals', breaks=seq(0,nrow(subset(df, case==street)),5)) + 
    scale_y_continuous(name = 'Compliance rate', limits = c(0,2),
                       sec.axis = sec_axis( trans=~ . - 0.5, name="Pedestrian crossing rate")) + 
    ggtitle(text) + theme_bw() + theme(legend.position="bottom")
  return(p)}


ggarrange(timeplot(traf,'fsbr','Frankfurter Straße, Bad Rothenfelde, Germany'),
          timeplot(traf,'lsho','Lange Straße, Hessisch Oldendorf, Germany'),
          timeplot(traf,'mke','Marktplatz, Königslutter am Elm, Germany'),
          timeplot(traf,'shnaf','Amalias Street, Nafplio, Greece'), ncol=1, nrow=4,
          common.legend = TRUE, legend="bottom")

ggarrange(timeplot2(traf,'fsbr','Frankfurter Straße, Bad Rothenfelde, Germany'),
          timeplot2(traf,'lsho','Lange Straße, Hessisch Oldendorf, Germany'),
          timeplot2(traf,'mke','Marktplatz, Königslutter am Elm, Germany'),
          timeplot2(traf,'shnaf','Amalias Street, Nafplio, Greece'), ncol=1, nrow=4,
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

ggarrange(p1,p2, ncol=1, nrow=2, common.legend = TRUE, legend="bottom")

ggplot(traf, aes(y=dspeed, x=as.factor(cross/30))) +
  geom_boxplot(fill = "cadetblue1") + theme_bw() +
  geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) +
  scale_y_continuous(name ="Speed deviation in km/h") +
  scale_x_discrete(name ="Pedestrian crossing in peds/2 minutes")

p1<-ggplot(traf, aes(y=pedcross, x=flow/30, color=case))+ geom_point()+ theme_bw() + 
  scale_color_brewer(palette = 'Dark2') +   
  geom_smooth(method='lm', color='black', aes(group=case, linetype=case), size=1, alpha=0.2) +
  scale_linetype_manual(values=c('solid','dashed','dotted','dotdash')) +
  scale_y_continuous(name = 'Pedestrian crossing rate in cross/ped') + 
  scale_x_continuous(name = 'Flow of vehicles in veh/2 minutes')

p2<-ggplot(traf, aes(y=comply, x=ped/30, color=case))+ geom_point()+ theme_bw() + 
  scale_color_brewer(palette = 'Dark2') + 
  geom_smooth(method='lm', color='black', aes(group=case, linetype=case), size=1, alpha=0.2) +
  scale_linetype_manual(values=c('solid','dashed','dotted','dotdash')) +
  scale_y_continuous(name = 'Speed Compliance rate') + 
  scale_x_continuous(name = 'Flow of pedestrians in peds/2 minutes')

p3<-ggplot(traf, aes(y=dspeed, x=cross, color=case))+ geom_point()+ theme_bw() + 
  scale_color_brewer(palette = 'Dark2') + 
  geom_smooth(method='lm', color='black', aes(group=case, linetype=case), size=1, alpha=0.2) +
  scale_linetype_manual(values=c('solid','dashed','dotted','dotdash')) +
  scale_y_continuous(name = 'Deviation from mean speed in km/h') + 
  scale_x_continuous(name = 'Pedestrin crossing rate in cross/ped')

ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

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
    scale_y_continuous(name ="Compliance rate")
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

# corr(subset(traf, select=c(cross, comply, shped, shveh, shvehspace, shpedspace)), 0.95)

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


traf$flow2<-traf$flow/30
traf$pedcross2<-1/exp(traf$pedcross)
model3<-lm(pedcross2 ~ flow2 + spratio, data = traf[-c(228, 226, 196, 70, 31, 59, 46,
                                                      123,29, 246, 223),])
summary(model3)
gvlma(model3)
ols_vif_tol(model3)
par(mfrow=c(2,2))
plot(model3, pch=20)

traf$ped2<-traf$ped/30
model4<-lm(comply ~ shpedspace + ped2, data = traf[-c(250, 170, 157),])
summary(model4)
gvlma(model4)
ols_vif_tol(model4)
par(mfrow=c(2,2))
plot(model4, pch=20)

model5<-lm(dspeed ~ cross, data=traf)
summary(model5)
gvlma(model5)



traf$cross2<-(traf$cross)^3
traf$dspeed2<-traf$dspeed^2
model5<-lm(dspeed ~ cross, data=traf[-c(250,200,259,170,203,286, 176, 137, 191, 148, 235, 185,
                                         167, 157, 248, 221, 226, 244, 262, 66, 174, 160, 293, 254,
                                         207, 153, 141),])
summary(model5)
gvlma(model5)
par(mfrow=c(2,2))
plot(model5, pch=20)


sp_csr<-function(csr,qveh) (1/0.2066)*((1/exp(csr)) - 0.6692 - 0.0037*qveh)

ggplot(traf,aes(flow2)) + theme_bw() +
  stat_function(fun = function(qveh) sp_csr(0.05,qveh), size=1.05) +
  stat_function(fun = function(qveh) sp_csr(0.10,qveh), size=1.05) +
  stat_function(fun = function(qveh) sp_csr(0.15,qveh), size=1.05) +
  stat_function(fun = function(qveh) sp_csr(0.20,qveh), size=1.05) +
  stat_function(fun = function(qveh) sp_csr(0.25,qveh), size=1.05) +
  scale_y_continuous("Share of vehicle space", limits = c(0, 1)) +
  scale_x_continuous("Flow of vehicles in veh/2 mins", limits = c(0, 30))

sp_cmr<-function(cmr, qped) (1/0.3469)*(cmr - 0.6576  + 0.0060*qped)

ggplot(traf,aes(ped2)) + theme_bw() +
  stat_function(fun = function(qveh) sp_cmr(0.55,qveh), size=1.05) +
  stat_function(fun = function(qveh) sp_cmr(0.65,qveh), size=1.05) +
  stat_function(fun = function(qveh) sp_cmr(0.75,qveh), size=1.05) +
  stat_function(fun = function(qveh) sp_cmr(0.85,qveh), size=1.05) +
  stat_function(fun = function(qveh) sp_cmr(0.95,qveh), size=1.05) +
  scale_y_continuous("Share of pedestrian space", limits = c(0, 1)) +
  scale_x_continuous("Flow of pedestrians in peds/2 mins", limits = c(0, 30))

# OLD CODE I DID NOT EXTEND THIS ANALYSIS TO FUNDAMENTAL DIAGRAMS

cr_fun<-function(qveh,spveh) log(1) -log(0.6692 + 0.0037*qveh + 0.2066*spveh)

ggplot(traf,aes(shvehspace)) + theme_bw() +
  stat_function(fun = function(spveh) cr_fun(5, spveh), size=1.05)
  stat_function(fun = function(spveh) cr_fun(10, spveh), size=1.05)
  stat_function(fun = function(spveh) cr_fun(15, spveh), size=1.05)
  stat_function(fun = function(spveh) cr_fun(25, spveh), size=1.05)
  stat_function(fun = function(spveh) cr_fun(30, spveh), size=1.05)

  #stat_function(fun = function(qveh) sp_cmr(0.65,qveh), size=1.05) +
  #stat_function(fun = function(qveh) sp_cmr(0.75,qveh), size=1.05) +
  #stat_function(fun = function(qveh) sp_cmr(0.85,qveh), size=1.05) +
  #stat_function(fun = function(qveh) sp_cmr(0.95,qveh), size=1.05) +
  #scale_y_continuous("Share of pedestrian space", limits = c(0, 1)) +
  #scale_x_continuous("Flow of pedestrians in peds/2 mins", limits = c(0, 30))



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

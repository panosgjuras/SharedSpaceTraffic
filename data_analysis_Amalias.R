library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(ggpubr)
library(gvlma)
library(psych)
library(RColorBrewer)

# import pedestrian dataset
setwd("~/Shared_space_Traffic/datasets")
ped_data<-read.csv2("amalias_pedestrian_dataset.csv",header=T,dec=".",sep=",")
ped_data$time<-as.POSIXct(ped_data$time,format="%H:%M")

# import vehicle data
veh_data<-read.csv2("amalias_vehicle_dataset_space.csv",header=T,dec=".",sep=",")
veh_data$time<-as.POSIXct(veh_data$time,format="%H:%M:%S")

# 1. Descriptive statistics before - after
# 1.1. number of pedestrian crossings per 2 minutes
describe(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==0)$cross) # descriptive statistics, not shared
shapiro.test(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==0)$cross) # check normality, not shared
# in parenthesis the outliers from the model estimation
describe(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==1)$cross)# crossings shared
shapiro.test(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==1)$cross) 

wilcox.test(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==1)$cross, # non parametric test, 
            subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==0)$cross, # check the significance of the diff: shared vs not shared
            exact=FALSE, paired=FALSE)

# 1.2 pedestrian volume per 2 minutes
describe(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==0)$ped) # shared
shapiro.test(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==0)$ped)

describe(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==1)$ped)
shapiro.test(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==1)$ped) # not shared

wilcox.test(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==0)$ped, # shared vs not shared
            subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==1)$ped,
            exact=FALSE, paired=FALSE)

# 1.3 traffic speeds
describe(subset(veh_data, shared==0 & pcu==1 & 60*headway>5)$speed)
shapiro.test(subset(veh_data, shared==0 & pcu==1 & 60*headway>5)$speed)
no_shared_speed<-subset(veh_data, shared==0 & pcu==1 & 60*headway>5)

describe(subset(veh_data[-c(438,27, 89, 283, 17,19,54,132),], shared==1 & pcu==1 & 60*headway>5)$speed)
shapiro.test(subset(veh_data[-c(438,27, 89, 283, 17,19,54,132),], shared==1 & pcu==1 & 60*headway>5)$speed)
shared_speed<-subset(veh_data[-c(438,17, 27, 89, 283, 19,54,132),], shared==1 & pcu==1 & 60*headway>5)

wilcox.test(shared_speed$speed, no_shared_speed$speed, exact=FALSE, paired=FALSE) # shared vs not shared

# 1.4 vehicle headway
describe(subset(veh_data, shared==0 & pcu==1 & 60*headway>5)$headway)
shapiro.test(subset(veh_data, shared==0 & pcu==1 & 60*headway>5)$headway)
describe(subset(veh_data[-c(438,27, 89, 283, 17,19,54,132),], shared==1 & pcu==1 & 60*headway>5)$headway)
shapiro.test(subset(veh_data[-c(438,27, 89, 283, 17,19,54,132),], shared==1 & pcu==1 & 60*headway>5)$headway)
wilcox.test(shared_speed$headway, no_shared_speed$headway, exact=FALSE, paired=FALSE)

# 2. BOX PLOTS
# 2.1 crossings vs pedestrian volume
# in not shared
p1<-ggplot(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==0), aes(y=ped, x=as.factor(cross))) + ggtitle("Point A: Conventional Road Section") +
  geom_boxplot(fill = "darkseagreen2") +   geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
  scale_x_discrete(name ="Pedestrian crossings in cross/2 minutes", limits=c("0","1","2","3","4","5","6","7","8","9","10")) +
  scale_y_continuous(name ="Pedestrian volume in peds/2 minutes", limits=c(0,50)) + geom_smooth(method=lm, color="red", aes(group=2))
# check correlation
cor.test(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==0)$cross, subset(ped_data[-c(155,154,191,166), ], shared==0)$ped, method='pearson')
cor.test(sqrt(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==0)$cross), subset(ped_data[-c(155,154,191,166), ], shared==0)$ped, method='pearson')
# in shared
p2<-ggplot(subset(ped_data[-c(155,154,191,166), ], shared==1), aes(y=ped, x=as.factor(cross))) + ggtitle("Point B: Shared space section") +
  geom_boxplot(fill = "lightblue1") +   geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
  scale_x_discrete(name ="Pedestrian crossings in cross/2 minutes", limits=c("0","1","2","3","4","5","6","7","8","9","10")) +
  scale_y_continuous(name ="Pedestrian volume in peds/2 minutes", limits=c(0,50)) + geom_smooth(method=lm, color="red", aes(group=2))
# check correlation
cor.test(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==1)$cross, subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==1)$ped, method='pearson')
cor.test(sqrt(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==1)$cross), subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==1)$ped, method='pearson')

# 2.2 traffic speed vs crossing
# in not shared
p3<-ggplot(no_shared_speed, aes(y=speed, x=as.factor(cross))) + ggtitle("Point A: Conventional Road Section") +
  geom_boxplot(fill = "darkseagreen2") +   geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
  scale_x_discrete(name ="Pedestrian crossings in cross/2 minutes", limits=c("0","1","2","3","4","5","6","7","8","9","10")) +
  scale_y_continuous(name ="Traffic speed of cars in km/h", limits=c(0,50)) + geom_smooth(method=lm, color="red", aes(group=2))
# correlations
cor.test(no_shared_speed$cross, no_shared_speed$speed, method='pearson')
# in shared
p4<-ggplot(shared_speed, aes(y=speed, x=as.factor(cross))) + ggtitle("Point B: Shared space section") +
  geom_boxplot(fill = "lightblue1") +   geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
  scale_x_discrete(name ="Pedestrian crossings in cross/2 minutes", limits=c("0","1","2","3","4","5","6","7","8","9","10")) +
  scale_y_continuous(name ="Traffic speed of cars in km/h", limits=c(0,50)) + geom_smooth(method=lm, color="red", aes(group=2))
# correlations
cor.test(shared_speed$cross, shared_speed$speed, method='pearson')
# final plot with ggarrange
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# 3. Models
# 3.1. pedestrian crossings models
ped_data$cross2<-sqrt(ped_data$cross) # normalize pedestrian crossing to meet linear regression assumptions
hist(sqrt(ped_data$cross),xlab="pedestrian crossings", col="lightgrey", breaks = 10, probability = TRUE)
# not shared
model1<-lm(cross2~ped+headway+0, data=subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==0))
summary(model1)
gvlma(model1) # check regression assumptions
par(mfrow=c(2,2)) # Q-Q plots
plot(model1, pch=20)
# shared
model15<-lm(cross2~ped+headway+0, data=subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], shared==1)) # shared
summary(model15)
gvlma(model15)
par(mfrow=c(2,2))
plot(model15, pch=20)

# 3.2 Traffic speed models
# not shared
no_shared_speed$headway2<-no_shared_speed$headway
model3<-lm(speed~ cross + headway, data=no_shared_speed)
summary(model3) # intercept - only model
# shared
model2<-lm(speed~ cross + headway, data=shared_speed)
summary(model2)
gvlma(model2)
par(mfrow=c(2,2))
plot(model2, pch=20)

# 4. extending model results
h1<-60/60 
fun.11<-function(ped)  ((model15[["coefficients"]][["ped"]]*ped + model15[["coefficients"]][["headway"]]*h1)^2)-
  ((model1[["coefficients"]][["ped"]]*ped + model1[["coefficients"]][["headway"]]*h1)^2)
h2<-60/120
fun.12<-function(ped)  ((model15[["coefficients"]][["ped"]]*ped + model15[["coefficients"]][["headway"]]*h2)^2)-
  ((model1[["coefficients"]][["ped"]]*ped + model1[["coefficients"]][["headway"]]*h2)^2)
h3<-60/180
fun.13<-function(ped)  ((model15[["coefficients"]][["ped"]]*ped + model15[["coefficients"]][["headway"]]*h3)^2)-
  ((model1[["coefficients"]][["ped"]]*ped + model1[["coefficients"]][["headway"]]*h3)^2)
h4<-60/360
fun.14<-function(ped)  ((model15[["coefficients"]][["ped"]]*ped + model15[["coefficients"]][["headway"]]*h4)^2)-
  ((model1[["coefficients"]][["ped"]]*ped + model1[["coefficients"]][["headway"]]*h4)^2)
h5<-60/480
fun.15<-function(ped)  ((model15[["coefficients"]][["ped"]]*ped + model15[["coefficients"]][["headway"]]*h5)^2)-
  ((model1[["coefficients"]][["ped"]]*ped + model1[["coefficients"]][["headway"]]*h5)^2)
h6<-60/600
fun.16<-function(ped)  ((model15[["coefficients"]][["ped"]]*ped + model15[["coefficients"]][["headway"]]*h6)^2)-
  ((model1[["coefficients"]][["ped"]]*ped + model1[["coefficients"]][["headway"]]*h6)^2)

ped<-1:3500
df<-data.frame(ped)
cols<-brewer.pal(8, "GnBu")

p5<-ggplot(df,aes(ped))+theme_bw() +
  stat_function(fun = fun.11, mapping = aes(color = "fun.11"), size=1.05) +
  stat_function(fun = fun.12, mapping = aes(color = "fun.12"), size=1.05) +
  stat_function(fun = fun.13, mapping = aes(color = "fun.13"), size=1.05) +
  stat_function(fun = fun.14, mapping = aes(color = "fun.14"), size=1.05) +
  stat_function(fun = fun.15, mapping = aes(color = "fun.15"), size=1.05) +
  stat_function(fun = fun.16, mapping = aes(color = "fun.16"), size=1.05) +
  scale_y_continuous("Difference in pedestrian cross/2 minutes", limits = c(-6, 4)) +
  scale_x_continuous("Pedestrian volume in peds/2 minutes", limits = c(0, 50)) + # το μοντέλο μέχρι 50 μπορεί
  scale_color_manual(name="with vehicle headway:",values=c(cols[8],cols[7],cols[6],cols[5],cols[4],cols[3]), 
                     labels = c("60/60 minutes", 
                                "60/120 minutes", 
                                "60/180 minutes",
                                "60/360 minutes",
                                "60/480 minutes",
                                "60/600 minutes"))

fun.21<-function(ped) (model2[["coefficients"]][["(Intercept)"]] + 
                        model2[["coefficients"]][["cross"]]* ((model15[["coefficients"]][["ped"]]*ped + model15[["coefficients"]][["headway"]]*h1)^2)+
                        model2[["coefficients"]][["headway"]]*h1)-mean(subset(veh_data, shared==0 & pcu==1 & 60*headway>5)$speed)
fun.22<-function(ped) (model2[["coefficients"]][["(Intercept)"]] + 
                         model2[["coefficients"]][["cross"]]* ((model15[["coefficients"]][["ped"]]*ped + model15[["coefficients"]][["headway"]]*h2)^2)+
                         model2[["coefficients"]][["headway"]]*h2)-mean(subset(veh_data, shared==0 & pcu==1 & 60*headway>5)$speed)
fun.23<-function(ped) (model2[["coefficients"]][["(Intercept)"]] + 
                         model2[["coefficients"]][["cross"]]* ((model15[["coefficients"]][["ped"]]*ped + model15[["coefficients"]][["headway"]]*h3)^2)+
                         model2[["coefficients"]][["headway"]]*h3)-mean(subset(veh_data, shared==0 & pcu==1 & 60*headway>5)$speed)
fun.24<-function(ped) (model2[["coefficients"]][["(Intercept)"]] + 
                         model2[["coefficients"]][["cross"]]* ((model15[["coefficients"]][["ped"]]*ped + model15[["coefficients"]][["headway"]]*h4)^2)+
                         model2[["coefficients"]][["headway"]]*h4)-mean(subset(veh_data, shared==0 & pcu==1 & 60*headway>5)$speed)
fun.25<-function(ped) (model2[["coefficients"]][["(Intercept)"]] + 
                         model2[["coefficients"]][["cross"]]* ((model15[["coefficients"]][["ped"]]*ped + model15[["coefficients"]][["headway"]]*h5)^2)+
                         model2[["coefficients"]][["headway"]]*h5)-mean(subset(veh_data, shared==0 & pcu==1 & 60*headway>5)$speed)
fun.26<-function(ped) (model2[["coefficients"]][["(Intercept)"]] + 
                         model2[["coefficients"]][["cross"]]* ((model15[["coefficients"]][["ped"]]*ped + model15[["coefficients"]][["headway"]]*h6)^2)+
                         model2[["coefficients"]][["headway"]]*h6)-mean(subset(veh_data, shared==0 & pcu==1 & 60*headway>5)$speed)

cols<-brewer.pal(8, "YlOrRd")
p6<-ggplot(df,aes(ped))+theme_bw() +
  stat_function(fun = fun.21, mapping = aes(color = "fun.21"), size=1.05) +
  stat_function(fun = fun.22, mapping = aes(color = "fun.22"), size=1.05) +
  stat_function(fun = fun.23, mapping = aes(color = "fun.23"), size=1.05) +
  stat_function(fun = fun.24, mapping = aes(color = "fun.24"), size=1.05) +
  stat_function(fun = fun.25, mapping = aes(color = "fun.25"), size=1.05) +
  stat_function(fun = fun.26, mapping = aes(color = "fun.26"), size=1.05) +
  scale_y_continuous("Difference in mean car speed in km/h", limits = c(-20, 2)) +
  scale_x_continuous("Pedestrian volume in peds/2 minutes", limits = c(0, 50)) +
  scale_color_manual(name="with vehicle headway:",values=c(cols[8],cols[7],cols[6],cols[5],cols[4],cols[3]), 
                     labels = c("60/60 minutes", 
                                "60/120 minutes", 
                                "60/180 minutes",
                                "60/360 minutes",
                                "60/480 minutes",
                                "60/600 minutes"))
ggarrange(p5, p6, ncol = 1, nrow = 2)


library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(ggpubr)
library(gvlma)
library(psych)
library(RColorBrewer)
library(rstudioapi) 

# import pedestrian dataset
# setwd("C:/Users/panos/Desktop/github_tzouras/Shared_space_Traffic/datasets")
# https://data.mendeley.com/datasets/n3wzjd54pj

ped_data<-read.csv2("amalias_pedestrian_dataset.csv",header=T,dec=".",sep=",")
ped_data$time<-as.POSIXct(ped_data$time,format="%H:%M")

# import vehicle data
veh_data<-read.csv2("amalias_vehicle_dataset_space.csv",header=T,dec=".",sep=",")
veh_data$time<-as.POSIXct(veh_data$time,format="%H:%M:%S")

# Descriptive statistics before - after
# number of pedestrian crossings per 2 minutes
describe(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], case=="shnaf")$cross) # descriptive statistics, not shared
shapiro.test(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], case=="covnaf")$cross) # check normality, not shared
# in parenthesis the outliers from the model estimation
describe(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], case=="shnaf")$cross)# crossings shared
shapiro.test(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], case=="covnaf")$cross) 

wilcox.test(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], case=="shnaf")$cross, # non parametric test, 
            subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], case=="shnaf")$cross, # check the significance of the diff: shared vs not shared
            exact=FALSE, paired=FALSE)

#  pedestrian volume per 2 minutes
describe(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], case=="shnaf")$ped) # shared
shapiro.test(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], case=="shnaf")$ped)

describe(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], case=="covnaf")$ped)
shapiro.test(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], case=="covnaf")$ped) # not shared

wilcox.test(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], case=="shnaf")$ped, # shared vs not shared
            subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], case=="covnaf")$ped,
            exact=FALSE, paired=FALSE)

# descriptive statistics of traffic speeds
describe(subset(veh_data, case=="shnaf" & pcu==1 & 60*headway>5)$speed)
shapiro.test(subset(veh_data, case=="shnaf" & pcu==1 & 60*headway>5)$speed)
shared_speed<-subset(veh_data, case=="shnaf" & pcu==1 & 60*headway>5)

describe(subset(veh_data[-c(438,27, 89, 283, 17,19,54,132),], case=="covnaf" & pcu==1 & 60*headway>5)$speed)
shapiro.test(subset(veh_data[-c(438,27, 89, 283, 17,19,54,132),], case=="covnaf" & pcu==1 & 60*headway>5)$speed)
no_shared_speed<-subset(veh_data[-c(438,17, 27, 89, 283, 19,54,132),], case=="covnaf" & pcu==1 & 60*headway>5)

wilcox.test(shared_speed$speed, no_shared_speed$speed, exact=FALSE, paired=FALSE) # shared vs not shared

# descriptive statistics of vehicle headways
describe(subset(veh_data, case=="shnaf" & pcu==1 & 60*headway>5)$headway)
shapiro.test(subset(veh_data, case=="shnaf" & pcu==1 & 60*headway>5)$headway)
describe(subset(veh_data[-c(438,27, 89, 283, 17,19,54,132),], case=="covnaf" & pcu==1 & 60*headway>5)$headway)
shapiro.test(subset(veh_data[-c(438,27, 89, 283, 17,19,54,132),], case=="covnaf" & pcu==1 & 60*headway>5)$headway)
wilcox.test(shared_speed$headway, no_shared_speed$headway, exact=FALSE, paired=FALSE)

# BOX PLOTS
# crossings vs pedestrian volume in not shared
p1<-ggplot(subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], case=="shnaf"), aes(y=ped, x=as.factor(cross))) + ggtitle("Point A: Conventional Road Section") +
  geom_boxplot(fill = "darkseagreen2") +   geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
  scale_x_discrete(name ="Pedestrian crossings in cross/2 minutes", limits=c("0","1","2","3","4","5","6","7","8","9","10")) +
  scale_y_continuous(name ="Pedestrian volume in peds/2 minutes", limits=c(0,50)) + geom_smooth(method=lm, color="red", aes(group=2))
p2<-ggplot(subset(ped_data[-c(155,154,191,166), ], case=="covnaf"), aes(y=ped, x=as.factor(cross))) + ggtitle("Point B: Shared space section") +
  geom_boxplot(fill = "lightblue1") +   geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
  scale_x_discrete(name ="Pedestrian crossings in cross/2 minutes", limits=c("0","1","2","3","4","5","6","7","8","9","10")) +
  scale_y_continuous(name ="Pedestrian volume in peds/2 minutes", limits=c(0,50)) + geom_smooth(method=lm, color="red", aes(group=2))

# traffic speed vs crossing in not shared
p3<-ggplot(no_shared_speed, aes(y=speed, x=as.factor(cross))) + ggtitle("Point A: Conventional Road Section") +
  geom_boxplot(fill = "darkseagreen2") +   geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
  scale_x_discrete(name ="Pedestrian crossings in cross/2 minutes", limits=c("0","1","2","3","4","5","6","7","8","9","10")) +
  scale_y_continuous(name ="Traffic speed of cars in km/h", limits=c(0,50)) + geom_smooth(method=lm, color="red", aes(group=2))
# in shared
p4<-ggplot(shared_speed, aes(y=speed, x=as.factor(cross))) + ggtitle("Point B: Shared space section") +
  geom_boxplot(fill = "lightblue1") +   geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
  scale_x_discrete(name ="Pedestrian crossings in cross/2 minutes", limits=c("0","1","2","3","4","5","6","7","8","9","10")) +
  scale_y_continuous(name ="Traffic speed of cars in km/h", limits=c(0,50)) + geom_smooth(method=lm, color="red", aes(group=2))
# final plot with ggarrange
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2) # SUPER PLOT
setwd(dirname(getActiveDocumentContext()$path)) 
ggsave('figureplots/trends_behavior_Amalias.png', plot = last_plot(), device = 'png', height = 7.34, width = 7.10, units = 'in')

# Models
# pedestrian crossings models
ped_data$cross2<-sqrt(ped_data$cross) # normalize pedestrian crossing to meet linear regression assumptions
hist(sqrt(ped_data$cross),xlab="pedestrian crossings", col="lightgrey", breaks = 10, probability = TRUE)
# not shared
model100<-lm(cross2~ped+headway+0, data=subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], case=="covnaf"))
summary(model100)
gvlma(model100) # check regression assumptions, here problem.
par(mfrow=c(2,2)) # Q-Q plots
plot(model100, pch=20)
# shared
model200<-lm(cross2~ped+headway+0, data=subset(ped_data[-c(155,154,191,166, 33, 31, 1, 27, 69, 98, 51), ], case=="shnaf")) # shared
summary(model200)
gvlma(model200)
par(mfrow=c(2,2))
plot(model200, pch=20)

# Traffic speed models
# not shared
model300<-lm(speed~ cross + headway, data=no_shared_speed)
summary(model300) # intercept - only model
gvlma(model300)
par(mfrow=c(2,2))
plot(model400, pch=20)
# shared
model400<-lm(speed~ cross + headway, data=shared_speed) # 0UTLIERS ARE MISSING
summary(model400)
gvlma(model400) # PROBLEM WITH THE ASSUMPTIONS..
par(mfrow=c(2,2))
plot(model400, pch=20)

setwd(dirname(getActiveDocumentContext()$path)) 
sink("models/model100.txt")
print(summary(model100))
sink()
sink("models/model200.txt")
print(summary(model200))
sink()
sink("models/model300.txt")
print(summary(model300))
sink()
sink("models/model400.txt")
print(summary(model400))
sink()

# simulation of models
h1<-60/60 
fun.11<-function(ped)  ((model200[["coefficients"]][["ped"]]*ped + model200[["coefficients"]][["headway"]]*h1)^2)-
  ((model100[["coefficients"]][["ped"]]*ped + model100[["coefficients"]][["headway"]]*h1)^2)
h2<-60/120
fun.12<-function(ped)  ((model200[["coefficients"]][["ped"]]*ped + model200[["coefficients"]][["headway"]]*h2)^2)-
  ((model100[["coefficients"]][["ped"]]*ped + model100[["coefficients"]][["headway"]]*h2)^2)
h3<-60/180
fun.13<-function(ped)  ((model200[["coefficients"]][["ped"]]*ped + model200[["coefficients"]][["headway"]]*h3)^2)-
  ((model100[["coefficients"]][["ped"]]*ped + model100[["coefficients"]][["headway"]]*h3)^2)
h4<-60/360
fun.14<-function(ped)  ((model200[["coefficients"]][["ped"]]*ped + model200[["coefficients"]][["headway"]]*h4)^2)-
  ((model100[["coefficients"]][["ped"]]*ped + model100[["coefficients"]][["headway"]]*h4)^2)
h5<-60/480
fun.15<-function(ped)  ((model200[["coefficients"]][["ped"]]*ped + model200[["coefficients"]][["headway"]]*h5)^2)-
  ((model100[["coefficients"]][["ped"]]*ped + model100[["coefficients"]][["headway"]]*h5)^2)
h6<-60/600
fun.16<-function(ped)  ((model200[["coefficients"]][["ped"]]*ped + model200[["coefficients"]][["headway"]]*h6)^2)-
  ((model100[["coefficients"]][["ped"]]*ped + model100[["coefficients"]][["headway"]]*h6)^2)

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
  scale_x_continuous("Pedestrian volume in peds/2 minutes", limits = c(0, 50)) + # ?? ??????? ????? 50 ??????
  scale_color_manual(name="with vehicle headway:",values=c(cols[8],cols[7],cols[6],cols[5],cols[4],cols[3]), 
                     labels = c("60/60 minutes", 
                                "60/120 minutes", 
                                "60/180 minutes",
                                "60/360 minutes",
                                "60/480 minutes",
                                "60/600 minutes"))

fun.21<-function(ped) (model400[["coefficients"]][["(Intercept)"]] + 
                        model400[["coefficients"]][["cross"]]* ((model200[["coefficients"]][["ped"]]*ped + model200[["coefficients"]][["headway"]]*h1)^2)+
                        model400[["coefficients"]][["headway"]]*h1)-mean(subset(veh_data, case=="shnaf" & pcu==1 & 60*headway>5)$speed)
fun.22<-function(ped) (model400[["coefficients"]][["(Intercept)"]] + 
                         model400[["coefficients"]][["cross"]]* ((model200[["coefficients"]][["ped"]]*ped + model200[["coefficients"]][["headway"]]*h2)^2)+
                         model400[["coefficients"]][["headway"]]*h2)-mean(subset(veh_data, case=="shnaf" & pcu==1 & 60*headway>5)$speed)
fun.23<-function(ped) (model400[["coefficients"]][["(Intercept)"]] + 
                         model400[["coefficients"]][["cross"]]* ((model200[["coefficients"]][["ped"]]*ped + model200[["coefficients"]][["headway"]]*h3)^2)+
                         model400[["coefficients"]][["headway"]]*h3)-mean(subset(veh_data, case=="shnaf" & pcu==1 & 60*headway>5)$speed)
fun.24<-function(ped) (model400[["coefficients"]][["(Intercept)"]] + 
                         model400[["coefficients"]][["cross"]]* ((model200[["coefficients"]][["ped"]]*ped + model200[["coefficients"]][["headway"]]*h4)^2)+
                         model400[["coefficients"]][["headway"]]*h4)-mean(subset(veh_data, case=="shnaf" & pcu==1 & 60*headway>5)$speed)
fun.25<-function(ped) (model400[["coefficients"]][["(Intercept)"]] + 
                         model400[["coefficients"]][["cross"]]* ((model200[["coefficients"]][["ped"]]*ped + model200[["coefficients"]][["headway"]]*h5)^2)+
                         model400[["coefficients"]][["headway"]]*h5)-mean(subset(veh_data, case=="shnaf" & pcu==1 & 60*headway>5)$speed)
fun.26<-function(ped) (model400[["coefficients"]][["(Intercept)"]] + 
                         model400[["coefficients"]][["cross"]]* ((model200[["coefficients"]][["ped"]]*ped + model200[["coefficients"]][["headway"]]*h6)^2)+
                         model400[["coefficients"]][["headway"]]*h6)-mean(subset(veh_data, case=="shnaf" & pcu==1 & 60*headway>5)$speed)

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
setwd(dirname(getActiveDocumentContext()$path)) 
ggsave('figureplots/model_simulation_res_Amalias.png', plot = last_plot(), device = 'png', height = 8.60, width = 5.47, units = 'in')

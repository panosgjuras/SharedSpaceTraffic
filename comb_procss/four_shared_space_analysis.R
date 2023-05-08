library(ggplot2)
library(ggpubr)
library(corrplot)
library(gvlma)
# library(olsrr)
library(RColorBrewer)
library(psych)
library(dplyr)
library(rstudioapi)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("/Users/panosgtzouras/Desktop/github_tzouras/Shared_space_Traffic")
# https://leopard.tu-braunschweig.de/receive/dbbs_mods_00071535
gerdat<-read.csv2('lsaxony_procss/outdatasets/datasets_germany.csv', header=T, dec=".", sep=",")
# https://data.mendeley.com/datasets/n3wzjd54pj
eldat<-read.csv2('amalias_procss/outdatasets/amalias_traffic_dataset.csv', header=T, dec=".", sep=",") # as it is downloaded from the data
 
# NEW DATASET TO PLAY WITH
datafr_fun <- function(df, x, dats, limit){
  if(dats == 'dats1'){datafr<-data.frame(case=x, flow=30*subset(df, case==x)$n_cars + 30*subset(df, case==x)$n_cyclists,
                                         dflow = 30*subset(df, case==x)$n_cars + 30*subset(df, case==x)$n_cyclists - 
                                           mean(30*subset(df, case==x)$n_cars + 30*subset(df, case==x)$n_cyclists),
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
                                         dped = 30*subset(df, case==x)$n_pedestrians - mean(30*subset(df, case==x)$n_pedestrians),
                                         # flow of pedestrians per hour
                                         dcross = 30*subset(df, case==x)$ped_crossing - mean(30*subset(df, case==x)$ped_crossing),
                                         cross = 30*subset(df, case==x)$ped_crossing)} 
  
  if(dats == 'dats2'){datafr<-data.frame(case=x, flow=60/subset(df, case==x)$headway,
                                         dflow = (60/subset(df, case==x)$headway) - mean(60/subset(df, case==x)$headway),
                                         # calculation flows from headways
                                         speed = subset(df, case==x)$speed,
                                         dspeed = subset(df, case==x)$speed - mean(subset(df, case==x)$speed),
                                         lspeed = subset(df, case==x)$speed - limit, # there is no speed limit, suppose 30
                                         comply = subset(df, case==x)$speed/limit,
                                         ped = 30*subset(df, case==x)$ped,
                                         dped = 30*subset(df, case==x)$ped - mean(30*subset(df, case==x)$ped),
                                         dcross = 30*subset(df, case==x)$cross - mean(30*subset(df, case==x)$cross),
                                         cross = 30*subset(df, case==x)$cross)}
  
  # datafr<-cbind(datafr, data.frame(shared = subset(df, case==x)$shared,
  #                                  # 1, if shared space, only one section
  #                                  oneway = subset(df, case==x)$oneway,
  #                                  lanes = subset(df, case==x)$lanes,
  #                                  # 1, if oneway road
  #                                  # park = subset(df, case==x)$park, # no use, all section with parking
  #                                  mark = subset(df, case==x)$mark,
  #                                  # 1, if road markings
  #                                  trees = subset(df, case==x)$trees,
  #                                  # 1, if trees
  #                                  bollards = subset(df, case==x)$bollards,
  #                                  # 1, if bollards
  #                                  width = subset(df, case==x)$width,
  #                                  # width of the pavement
  #                                  cross_y = subset(df, case==x)$cross_y,
  #                                  # 1, existence of courtesy crossings
  #                                  s_width = subset(df, case==x)$s_width,
  #                                  # 1, sidewalk width
  #                                  # inters = subset(df, case==x)$inters, no use of this variable
  #                                  bench = subset(df, case==x)$bench,
  #                                  # 1, if benches
  #                                  obst = subset(df, case==x)$obst,
  #                                  # 1, if obstacles
  #                                  lev_segr = subset(df, case==x)$lev_segr),
  #                                  vis_segr = subset(df, case==x)$vis_segr) # 1, if level segregatioN
  return(datafr)}

traf<-datafr_fun(gerdat,'fsbr', 'dats1', 20) # fsbr 20 km/h
traf<-rbind(traf, datafr_fun(gerdat,'lsho', 'dats1', 10)) # 10 km/h
traf<-rbind(traf, datafr_fun(gerdat,'mke', 'dats1', 20)) # 20 km/h
traf<-rbind(traf, datafr_fun(eldat,'shnaf','dats2', 30)) # # 30 km/h
traf$comply<-replace(traf$comply, traf$comply==0, 1)


# traf<-rbind(traf, datafr_fun(eldat,'covnaf', 'dats2')) # no conventional section imported in the aanalyis

# CORRELATIONS
corr<-function(df, x, m){ # plot correlation table funntion
  corel = df
  M = cor(corel)
  testRes=cor.mtest(corel, conf.level=x, method = m)
  corrplot(M, method = 'number', p.mat = testRes$p, sig.level = 1-x, diag=FALSE)} 

# with X no significant correlations
# no correlation between speed deviation and crossing or flow
# but compliance have some interesting correlations...correlation with crossiing, not with flows.
# positive correlation with flow (????), high speed when we have high flow ??
# maybe free flow branch and car dominance lead to higher speeds..
# positive and significant correlation with road width and courtesy crossings, positive
# in crossing, super strong correlation with ped and shared

# DESCRIPTIVE STATISTICS

traf$pedcross<-traf$cross/traf$ped # pedestrian crossings per pedestrian (relative number)
# traf$shpedspace<-(2*traf$s_width)/(2*traf$s_width + traf$width) # percentage of ped space over all
# traf$shped<-traf$ped/(traf$flow + traf$ped)

traf$circ<-ifelse(traf$case == 'fsbr', 6, ifelse(traf$case == 'lsho', 5, 
                                                 ifelse(traf$case == 'mke', 6, 
                                                        ifelse(traf$case == 'shnaf', 4.5, 0))))

traf$minsafe<-ifelse(traf$case == 'fsbr', 0, ifelse(traf$case == 'lsho', 3, 
                                                 ifelse(traf$case == 'mke', 3.5, 
                                                        ifelse(traf$case == 'shnaf', 6, 0))))

traf$maxsafe<-ifelse(traf$case == 'fsbr', 7, ifelse(traf$case == 'lsho', 6, 
                                                    ifelse(traf$case == 'mke', 3.5, 
                                                           ifelse(traf$case == 'shnaf', 6, 0))))

traf$minact<-ifelse(traf$case == 'fsbr', 0, ifelse(traf$case == 'lsho', 0, 
                                                    ifelse(traf$case == 'mke', 14, 
                                                           ifelse(traf$case == 'shnaf', 6.5, 0))))

traf$maxact<-ifelse(traf$case == 'fsbr', 7, ifelse(traf$case == 'lsho', 3, 
                                                   ifelse(traf$case == 'mke', 14, 
                                                          ifelse(traf$case == 'shnaf', 6.5, 0))))

traf$totspace <- (traf$minsafe + traf$maxact + traf$circ)
traf$shvehspace <- traf$circ/traf$totspace
traf$shpedspace <- 1 - traf$shvehspace




# A. descriptive statistics..!
describe(subset(traf, (case=='fsbr' & speed!=0))$speed)
describe(subset(traf, (case=='lsho' & speed!=0))$speed)
describe(subset(traf, (case=='mke' & speed!=0))$speed)
describe(subset(traf, (case=='shnaf' & speed!=0))$speed)

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


traf$circped <- traf$circ/(1-traf$totspace)
traf$circsaf <- traf$minsaf/traf$circ
traf$shcirc<- traf$circ/traf$totspace
traf$shminsafe<-traf$minsafe/traf$totspace
traf$shmaxact<- traf$maxact/traf$totspace

corr(select(traf, c('shcirc', 'shmaxact', 'shminsafe',
                    'dflow', 'dped', 'dspeed', 'pedcross', 'comply')), 0.95, 'kendall')

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
    scale_x_continuous(name = 'Two minutes intervals', limits = c(0,105), breaks=seq(0,nrow(subset(df, case==street)),5)) + 
    scale_y_continuous(name = 'Speed deviations in km/h', limits = c(-25,25),
                       sec.axis = sec_axis( trans=~ (./20) + 0.3, name="Pedestrian crossing rate")) + 
    ggtitle(text) + theme_bw() + theme(legend.position="bottom")
return(p)}

p100<-ggarrange(timeplot(traf,'fsbr','Frankfurter Street, Bad Rothenfelde, Germany'),
                timeplot(traf,'lsho','Lange Street, Hessisch Oldendorf, Germany'),
                timeplot(traf,'mke','Marktplatz, Konigslutter am Elm, Germany'),
          timeplot(traf,'shnaf','Amalias Street, Nafplio, Greece'), ncol=1, nrow=4,
          common.legend = TRUE, legend="bottom")

p101<-ggplot(traf, aes(y=dspeed, x=as.factor(cross/30))) +
      geom_boxplot(fill = "cadetblue1") + theme_bw() +
      geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) +
      scale_y_continuous(name ="Speed deviation in km/h") +
      scale_x_discrete(name ="Pedestrian crossing in ped/2 minutes")

ggarrange(p101, p100, ncol = 2)
setwd(dirname(getActiveDocumentContext()$path))
ggsave('figureplots/timeplots_all.png', plot = last_plot(), device = 'png', height = 7.95, width = 10.72, units = 'in')


p11<-ggplot(traf, aes(x=as.factor(case), y = pedcross, fill=case)) + geom_boxplot() + 
   geom_point(size = 0.9, alpha = .3, position = position_jitter(seed = 1, width = .2)) +
   scale_x_discrete(name = 'Case', labels = c('Frankfurter Str', 'Lange Str',
                                            'Marktplatz', 'Amalias Street')) +
   scale_y_continuous(name = 'Pedestrian crossing rate', limits=c(0,1.5)) +
   scale_fill_brewer(palette = 'Pastel2')+ theme_bw() + theme(legend.position = "none")

p22<-ggplot(traf, aes(x=as.factor(case), y = comply, fill=case)) + geom_boxplot() + 
   geom_point(size = 0.9, alpha = .3, position = position_jitter(seed = 1, width = .2)) +
   scale_x_discrete(name = 'Case', labels = c('Frankfurter Str', 'Lange Str',
                                             'Marktplatz', 'Amalias Street')) +
   scale_y_continuous(name = 'Speed compliance rate') +
   scale_fill_brewer(palette = 'Pastel2')+ theme_bw() + theme(legend.position = "none")


p1<-ggplot(traf, aes(y=pedcross, x=dflow, color=case))+ geom_point(size = 0.90)+ theme_bw() + 
  scale_color_brewer(palette = 'Dark2') +   
  geom_smooth(method='lm', aes(group=case, color=case), size=1, alpha=0.2) +
  scale_linetype_manual(values=c('solid','dashed','dotted','dotdash')) +
  scale_y_continuous(name = 'Pedestrian crossing rate') + 
  scale_x_continuous(name = 'Deviation of vehicle flow in veh/h')

p2<-ggplot(traf, aes(y=pedcross, x=dped, color=case))+ geom_point(size = 0.90)+ theme_bw() + 
  scale_color_brewer(palette = 'Dark2') +   
  geom_smooth(method='lm', aes(group=case, color=case), size=1, alpha=0.2) +
  scale_linetype_manual(values=c('solid','dashed','dotted','dotdash')) +
  scale_y_continuous(name = 'Pedestrian crossing rate') + 
  scale_x_continuous(name = 'Deviation of pedestrian flow in ped/h')

p3<-ggplot(traf, aes(y=comply, x=dflow, color=case))+ geom_point(size = 0.90)+ theme_bw() + 
  scale_color_brewer(palette = 'Dark2') + 
  geom_smooth(method='lm', aes(group=case, color=case), size=1, alpha=0.2) +
  scale_y_continuous(name = 'Speed Compliance rate') + 
  scale_x_continuous(name = 'Deviation of vehicle flow in veh/h')

p4<-ggplot(traf, aes(y=comply, x=dped, color=case))+ geom_point(size = 0.90)+ theme_bw() + 
  scale_color_brewer(palette = 'Dark2') + 
  geom_smooth(method='lm', aes(group=case, color=case), size=1, alpha=0.2) +
  scale_y_continuous(name = 'Speed Compliance rate') + 
  scale_x_continuous(name = 'Deviation of pedestrian flow in ped/h')

# p3<-ggplot(traf, aes(y=dspeed, x=cross, color=case))+ geom_point()+ theme_bw() +
#  scale_color_brewer(palette = 'Dark2') + 
#  geom_smooth(method='lm', aes(group=case, color=case), size=1, alpha=0.2) +
#  scale_y_continuous(name = 'Deviation from mean speed in km/h') + 
#  scale_x_continuous(name = 'Pedestrin crossing rate in cross/ped')

ggarrange(p11, p22, p1, p3, p2, p4, ncol=2, nrow=3, common.legend = TRUE, legend="bottom")
setwd(dirname(getActiveDocumentContext()$path))
ggsave('figureplots/boxscatterplots_all.png', plot = last_plot(), device = 'png', height = 8.60, width = 6.71, units = 'in')

# end up with two important parameters: compliance rate and crossings per pedestrian
# two different forces, comparison and analysis.

# DESIGN VS COMPLY AND PEDCROSS
boxpl_fun<-function(df, x, y1, y2, text){
  p1<-ggplot(df, aes(as.factor({{x}}), {{y1}})) + geom_boxplot( fill = "darkseagreen2") +
    geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
    scale_x_discrete(name = text) +
    scale_y_continuous(name ="Number of crossings per pedestrian")
  p2<-ggplot(df, aes(as.factor({{x}}), {{y2}})) + geom_boxplot( fill = "lightblue1") +
    geom_point(size = 1.5, alpha = .3, position = position_jitter(seed = 1, width = .2)) + theme_bw() +
    scale_x_discrete(name = text) +
    scale_y_continuous(name ="Compliance rate")
  ggarrange(p1, p2, ncol=2, nrow=1)
} # box plot function to see visually the impact of each road design feature

# Bollards, yer or no
# FIX FIX PROBLEM WITH THE DESIGN
mean(subset(traf, bollards == 1)$pedcross) - mean(subset(traf, bollards == 0)$pedcross) # 0.10 more crossings per pedestrian
wilcox.test(subset(traf, bollards == 1)$pedcross, subset(traf, bollards == 0)$pedcross, exact=FALSE, paired=FALSE) # significance
mean(subset(traf, bollards == 1)$comply) - mean(subset(traf, bollards == 0)$comply)
wilcox.test(subset(traf, bollards == 1)$comply, subset(traf, bollards == 0)$comply, exact=FALSE, paired=FALSE)
# statistically significant difference for 95% confidence interval
boxpl_fun(traf, bollards, pedcross, comply, '1, if there are bollards')
boxpl_fun(traf, case, pedcross, comply, 'Case')

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

#################

# HOT HOT HOT HOT FINALLY
traf$pedcross2<-1/exp(traf$pedcross)
traf$dflow2 <- traf$dflow/100
traf$dped2 <- traf$dped/100
model77<-lm(pedcross2 ~ dflow + dped + shcirc + shminsafe, data = traf[-c(228, 226, 196, 70, 31, 59, 46),])
summary(model77)
# ols_vif_tol(model77)
gvlma(model77)
par(mfrow=c(2,2))
plot(model77, pch=20)
# SAVE THE PLOTS TO GIVE THE FINAL RESULTS

model88<-lm(comply ~ dflow + dped + shcirc + shminsafe, data = traf[-c(250, 170, 157),])
summary(model88)
# ols_vif_tol(model88)
gvlma(model88)
par(mfrow=c(2,2))
plot(model88, pch=20)
# HOT HOT HOT HOT FINALLY

# output of models
setwd(dirname(getActiveDocumentContext()$path)) 
sink("models/model77.txt")
print(summary(model77))
sink()
sink("models/model88.txt")
print(summary(model88))
sink()

# Simulation of these models: model88 and model77
magicpl<-function(circ, saf, act, name) {
  lmin = -625
  lmax = 625
  df <- expand.grid(x = lmin:lmax, y = lmin:lmax)
  
  name = name
  s_circ = circ/15
  s_saf = saf/15
  s_act = act/15

  b0 = model77[["coefficients"]][["(Intercept)"]]
  b1 = model77[["coefficients"]][["dflow"]]
  b2 = model77[["coefficients"]][["dped"]]
  b3 = model77[["coefficients"]][["shcirc"]]
  b4 = model77[["coefficients"]][["shminsafe"]]
  
  df["pedcross"] <- log(1/(b0 + b1 * df$y + b2 * df$x
                           + b3 * s_circ + b4 * s_saf))
  df$pedcross <- ifelse(df$pedcross < 0, 0, df$pedcross)
  
  p1<-ggplot(df, aes(x = x, y = y, fill = pedcross)) + geom_raster() + theme_bw() +
    scale_y_continuous(name = "dqveh in veh/h") +
    scale_x_continuous(name = "dqped in ped/h") +
    scale_fill_viridis_c("Pedestrian crossing rate", option = "D", direction = 1, limits = c(0, 0.9))
  
  b00 = model88[["coefficients"]][["(Intercept)"]]
  b11 = model88[["coefficients"]][["dflow"]]
  b22 = model88[["coefficients"]][["dped"]]
  b33 = model88[["coefficients"]][["shcirc"]]
  b44 = model88[["coefficients"]][["shminsafe"]]
  
  df["comply"] <- b00 + b11 * df$y + b22 * df$x + b33 * s_circ + b44 * s_saf
  
  p2 <- ggplot(df, aes(x = x, y = y, fill = comply)) + geom_raster() + theme_bw() +
    scale_y_continuous(name = "dqveh in veh/h") +
    scale_x_continuous(name = "dqped in ped/h") +
    scale_fill_viridis_c("Speed compliance rate", option = "B", direction = 1, limits = c(0.3, 1.35))
  
  ggarrange(p1, p2, nrow = 1, ncol = 2, legend = 'none')}

ggarrange(magicpl(15, 0, 0, 's1: circ: 15%; saf: 0%; act: 0%'),
          magicpl(6, 4.5, 4.5, 's2: circ: 40%; saf: 30%; act: 30%'),
          magicpl(0, 15, 0, 's3: circ: 0%; saf: 100%; act: 0%'),
          magicpl(4.5, 4.5, 6, 's4: circ: 30%; saf: 30%; act: 40%'),
          magicpl(0, 0, 15, 's5: circ: 0%; saf: 0%; act: 100%'),
          ncol=1, nrow=5)
ggsave('figureplots/model_simu_results_all.png', plot = last_plot(), device = 'png', height = 8.22, width = 7.04, units = 'in')

rm(list = ls())

library(data.table)
library(ggplot2)
library(dplyr)
library(zoo)

setwd('C:/Users/karndt.WHRC/Desktop/sites/council/')

#read in the full data set
era = fread('./data/ERA5hourly_2016_2023_Council.csv')
names(era)

#make more R friendly names
names(era) = c('index','date','dew','id','st1','st2','le','pres','h','rad','airt','ppt','u','v','vwc1','vwc2','geo')

#adjust to the timezone of interest (make sure it's listed in UTC)
era$date
tz = -9 #hours from UTC
era$date = era$date+(tz*60*60)

summary(era$date)
#subset down to time range of interest
era = subset(era,era$date >= as.POSIXct('2016-1-1 00:00',tz='UTC'))
era = subset(era,era$date <= as.POSIXct('2023-1-1 00:00',tz='UTC'))

#convert temps from K to deg C
era$airt  = era$airt-273.15
era$dew   = era$dew-273.15
era$st1 = era$st1-273.15
era$st2 = era$st2-273.15

#make negatives NAs so we can fill them using linear interpolation.
era$rad = era$rad/3600 #convert from J m-2 to Wm-2, divide by seconds in an hour
era$le = era$le/-3600 #convert from J m-2 to Wm-2, divide by seconds in an hour
era$h = era$h/-3600 #convert from J m-2 to Wm-2, divide by seconds in an hour

#calculate rh from the dew point and temperature
era$rh = 100*(exp((17.625*era$dew)/(243.04+era$dew))/exp((17.625*era$airt)/(243.04+era$airt)))

#create wind speed from u and v
era$ws = sqrt(era$v^2 + era$u^2)

#create a date data frame with every half hour in the time frame of interest
date = seq(from = as.POSIXct('2016-1-1 00:00',tz='UTC'),
           to = as.POSIXct('2023-01-01 00:00',tz='UTC'),
           by = 60*30)
datedf = as.data.frame(date)

#merge with era 5
eram = merge(datedf,era,by = 'date',all = T)

#gap fill middle half hours
eram$dew   = na.approx(object = eram$dew,maxgap = 1)
eram$rh   = na.approx(object = eram$rh,maxgap = 1)
eram$st1   = na.approx(object = eram$st2,maxgap = 1)
eram$st2   = na.approx(object = eram$st2,maxgap = 1)
eram$rad   = na.approx(object = eram$rad,maxgap = 1)
eram$ppt   = na.approx(object = eram$ppt,maxgap = 1)
eram$pres   = na.approx(object = eram$pres,maxgap = 1)
eram$airt  = na.approx(object = eram$airt,maxgap = 1)
eram$vwc1   = na.approx(object = eram$vwc1,maxgap = 1)
eram$vwc2   = na.approx(object = eram$vwc2,maxgap = 1)
eram$ws   = na.approx(object = eram$ws,maxgap = 1)
eram$le   = na.approx(object = eram$le,maxgap = 1)
eram$h   = na.approx(object = eram$h,maxgap = 1)

#check out the data
# ggplot(data = eram)+theme_bw()+geom_hline(yintercept = 0)+
#   geom_point(aes(date,airt,col='airT'))+
#   geom_point(aes(date,st1,col='soilT1'))+
#   geom_point(aes(date,st2,col='soilT2'))
# 
# ggplot(data = eram)+theme_bw()+geom_hline(yintercept = 0)+
#   geom_point(aes(date,rad))
# 
# ggplot(data = eram)+theme_bw()+geom_hline(yintercept = 0)+
#   geom_point(aes(date,vwc1,col='vwc1'))+
#   geom_point(aes(date,vwc2,col='vwc2'))
#   
# ggplot(data = eram)+theme_bw()+geom_hline(yintercept = 0)+
#   geom_point(aes(date,rh))
# 
# ggplot(data = eram)+theme_bw()+
#   geom_point(aes(date,pres))
# 
# ggplot(data = eram)+theme_bw()+geom_hline(yintercept = 0)+
#   geom_point(aes(date,ws))

#resave off for comparison
write.csv(x = eram,file = './data/era5_council.csv',row.names = F)

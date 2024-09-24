rm(list = ls())

library(data.table)
Sys.setenv(TZ='UTC')
setwd('C:/Users/karndt.WHRC/Desktop/sites/council/data') #use this to set your directory

#Prep for Reddy for an initial gap filling
dat = fread('./council_2017_2022_gf.csv')

#time variables #########################################################
dat$ts = as.POSIXct(dat$ts)

Year = format(dat$ts,'%Y')
DoY  = format(dat$ts,'%j')
h    = as.numeric(format(dat$ts,'%H')) #full hours
h.5  = as.numeric(ifelse(format(dat$ts,'%M') == '00',0,0.5)) #half hour decimals
Hour = h+h.5 #Hour in the expected format

dat$nee = ifelse(is.na(dat$FC),dat$rfnee,dat$FC)
dat$ch4 = ifelse(is.na(dat$FCH4),dat$rfch4,dat$FCH4)

#Reddy df
reddy = data.frame(Year,DoY,Hour,
                   dat$nee,
                   dat$ch4,
                   dat$H,
                   dat$LE,
                   dat$USTAR,
                   dat$rad.eramod,
                   dat$airt.eramod,
                   dat$tsoil.eramod,
                   dat$rh.eramod)
names(reddy) = c('Year','DoY','Hour','NEE','CH4','H','LE','Ustar','Rg','Tair','Tsoil','RH')

#check the assumptions
summary(reddy$Rg) #no negatives allowed
summary(reddy$RH) #no values over 100 allowed

#run these if needed
reddy$Rg = reddy$Rg - min(reddy$Rg)
reddy$Rg = ifelse(reddy$Rg < 0,0,reddy$Rg) #set negatives to 0
reddy$RH = ifelse(reddy$RH > 100,100,reddy$RH)

# experiment further cleaning to see if partitioning will work
reddy$NEE = ifelse(reddy$NEE < -15,NA,reddy$NEE) #set negatives to 0
reddy$NEE = ifelse(reddy$NEE > 15,NA,reddy$NEE) #set negatives to 0

#add the second header row and resave as a txt
h2 = c('--','--','--','umolm-2s-1','nmolm-2s-1','Wm-2','Wm-2','ms-1','Wm-2','DegC','DegC','%')
names(h2) = names(reddy)
h2 = as.character(h2)
reddy2 = rbind(h2,reddy)
reddy2$Year = as.character(reddy2$Year)
reddy2$DoY  = as.character(reddy2$DoY)
reddy2[1,1] = '--'
reddy2[1,2] = '--'

reddy2 = reddy2[complete.cases(reddy2$Year),]

write.table(x = reddy2,file = './reddy_council.txt',row.names = F,sep = '\t',quote = F,na = '-9999')

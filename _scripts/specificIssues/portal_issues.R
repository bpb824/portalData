# Initialization ----------------------------------------------------------

#Load neccesary libraries
require(RPostgreSQL)
require(DBI)
require(timeSeries)
require(plyr)
require(ggplot2)
require(lubridate)
require(rjson)

#Set working directory
setwd("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/")

#Connect to Portal db, make sure you VPN into PSU CECS network
db_cred = fromJSON(file="db_credentials.json")
con <- dbConnect(dbDriver("PostgreSQL"), host=db_cred$db_credentials$db_host, port= 5432, user=db_cred$db_credentials$db_user, password = db_cred$db_credentials$db_pass, dbname=db_cred$db_credentials$db_name)

#Read in relational Tables
stations= dbGetQuery(con,"SELECT * FROM public.stations")
highways = dbGetQuery(con,"SELECT * FROM public.highways")
detectors = dbGetQuery(con,"SELECT * FROM public.detectors")
corridors  = dbGetQuery(con,"SELECT * FROM public.corridors")
corridor_stations = dbGetQuery(con,"SELECT * FROM public.corridorstations")

#Load neccesary functions
source("_functions/querying.R")
source("_functions/filtering.R")
source("_functions/aggregation.R")

# Issue 3: I-5 SB @ Wilsonville -------------------------------------------
stas = c(3182)


# Issue 5 - I-205 NB and SB - Clackamas Hwy -------------------------------
stas = c(1043,1100)
subStations = subset(stations,stations$stationid %in% stas)
dets = detectors$detectorid[detectors$stationid %in% stas]
startDate = "2015-05-01"
endDate = "2015-05-31"
raw = dbGetQuery(con,freewayQuery(dets,startDate,endDate))
clean = filter(raw)
joined = join(clean,detectors,by ="detectorid")
agg = aggTime(unAgg=joined,aggVars=c("stationid","lanenumber"),timeCut = "hour", hod = TRUE)

#plotting
timeLabs = c("3 AM","6 AM","9 AM","12 PM","3 PM","6 PM","9 PM")
timeTicks = seq(3,21,3)
theme_set(theme_grey(base_size = 20))
png("_results/img/specificIssues/2/clackHwyNB.png", width = 900,height = 400)
ggplot(agg[agg$stationid==1043,],aes(x=hod,y=speed,group =lanenumber, colour = lanenumber))+geom_line()+scale_colour_discrete(name="Lane Number")+ggtitle("I-205 NB @ Clackamas Hwy (Mean over 05-01-15 to 05-31-15)")+ylab("Mean Speed (mph)")+xlab("")+scale_x_continuous(breaks = timeTicks,labels = timeLabs)
dev.off()
png("_results/img/specificIssues/2/clackHwySB.png", width = 900,height = 400)
ggplot(agg[agg$stationid==1100,],aes(x=hod,y=speed,group =lanenumber, colour = lanenumber))+geom_line()+scale_colour_discrete(name="Lane Number")+ggtitle("I-205 SB @ Clackamas Hwy (Mean over 05-01-15 to 05-31-15)")+ylab("Mean Speed (mph)")+xlab("")+scale_x_continuous(breaks = timeTicks,labels = timeLabs)
dev.off()

#Six month analysis
startDate = "2015-01-01"
endDate = "2015-06-30"
raw = dbGetQuery(con,freewayQuery(dets,startDate,endDate))
clean = filter(raw)
joined = join(clean,detectors,by ="detectorid")
joined$period = cut(joined$starttime, breaks = "hour")
agg = ddply(joined,c("lanenumber","stationid","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
agg$period = as.POSIXct(agg$period)
agg$lanenumber = factor(agg$lanenumber)
agg$hour = hour(agg$period)
agg_hour= ddply(agg,c("lanenumber","stationid","hour"),function(X) data.frame(volume=mean(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume), speed15 = as.numeric(quantile(X$speed,0.15)),speed85 = as.numeric(quantile(X$speed,0.85))))
timeLabs = c("3 AM","6 AM","9 AM","12 PM","3 PM","6 PM","9 PM")
timeTicks = seq(3,21,3)

png("2/clackHwyNB-6month.png", width = 900,height = 400)
ggplot(agg_hour[agg_hour$stationid==1043,])+geom_line(aes(x=hour,y=speed,group =lanenumber, colour = lanenumber))+
  geom_line(aes(x=hour,y=speed15,group =lanenumber, colour = lanenumber),linetype="dashed",alpha=0.5)+
  geom_line(aes(x=hour,y=speed85,group =lanenumber, colour = lanenumber),linetype="dashed",alpha=0.5)+
  scale_colour_discrete(name="Lane Number")+ggtitle("Clackamas Hwy NB (Mean over 01-01-15 to 06-30-15)")+ylab("Mean Speed (mph)")+xlab("")+scale_x_continuous(breaks = timeTicks,labels = timeLabs)
dev.off()
png("2/clackHwySB-6month.png", width = 900,height = 400)
ggplot(agg_hour[agg_hour$stationid==1100,])+geom_line(aes(x=hour,y=speed,group =lanenumber, colour = lanenumber))+
  geom_line(aes(x=hour,y=speed15,group =lanenumber, colour = lanenumber),linetype="dashed",alpha=0.5)+
  geom_line(aes(x=hour,y=speed85,group =lanenumber, colour = lanenumber),linetype="dashed",alpha=0.5)+
  scale_colour_discrete(name="Lane Number")+ggtitle("Clackamas Hwy SB (Mean over 01-01-15 to 06-30-15)")+ylab("Mean Speed (mph)")+xlab("")+scale_x_continuous(breaks = timeTicks,labels = timeLabs)
dev.off()

#diff12 = agg_hour$speed[agg_hour$lanenumber ==2 & agg_hour$stationid==1043] - agg_hour$speed[agg_hour$lanenumber ==3 & agg_hour$stationid==1043]
#diff23 = agg_hour$speed[agg_hour$lanenumber ==3 & agg_hour$stationid==1043] - agg_hour$speed[agg_hour$lanenumber ==4 & agg_hour$stationid==1043]

#diff12 = agg_hour$speed[agg_hour$lanenumber ==1 & agg_hour$stationid==1100] - agg_hour$speed[agg_hour$lanenumber ==2 & agg_hour$stationid==1100]
#diff23 = agg_hour$speed[agg_hour$lanenumber ==2 & agg_hour$stationid==1100] - agg_hour$speed[agg_hour$lanenumber ==3 & agg_hour$stationid==1100]

# Issue 6: I-205 SB @ OR-224/82nd: Missing Data -----------------------------------------------------------------
stas = 1099
subStations = subset(stations,stations$stationid %in% stas)
dets = detectors$detectorid[detectors$stationid %in% stas ]
subDetectors = subset(detectors,detectors$detectorid %in% dets)
startDate = "2015-05-01"
endDate = "2015-05-31"
raw = dbGetQuery(con,freewayQuery(dets,startDate,endDate))
clean = filter(raw)
joined = join(clean,detectors,by ="detectorid")
joined$period = cut(joined$starttime, breaks = "hour")
agg = ddply(joined,c("lanenumber","stationid","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
agg$period = as.POSIXct(agg$period)
agg$lanenumber = factor(agg$lanenumber)
agg$hour = hour(agg$period)
agg_hour= ddply(agg,c("lanenumber","stationid","hour"),summarise, volume = mean(volume),occupancy = mean(occupancy),speed=mean(speed))
timeLabs = c("3 AM","6 AM","9 AM","12 PM","3 PM","6 PM","9 PM")
timeTicks = seq(3,21,3)
ggplot(agg_hour,aes(x=hour,y=speed,group =lanenumber, colour = lanenumber))+geom_line()+scale_colour_discrete(name="Lane Number")+ggtitle("Clackamas Hwy NB (Mean over 05-01-15 to 05-31-15)")+ylab("Mean Speed (mph)")+xlab("")+scale_x_continuous(breaks = timeTicks,labels = timeLabs)

# Issue 8: I-205 SB @ Stark and Washington - Hourly Ramp Volumes ----------
stas = c(5051)
subStations = subset(stations,stations$stationid %in% stas)
dets = detectors$detectorid[detectors$stationid %in% stas ]
subDetectors = subset(detectors,detectors$detectorid %in% dets)
startDate = "2014-05-01"
endDate = "2015-06-30"
#query = freewayQuery(dets,startTime = startDate,endTime=endDate)
#raw = dbGetQuery(con,query)

monthRange = timeSequence("2014-01-01","2015-06-01",by="month")
stark = list()
for (i in 1:length(monthRange)){
  month = as.character(monthRange[i])
  query = publicQuery(dets,month)
  raw = dbGetQuery(con,query)
  stark[[i]]=raw
  print(i)
}

rampDets = detectors$detectorid[detectors$stationid>=5000]

starkAgg = list()
for (i in 1:length(stark)){
  mf = stark[[i]]
  mf = filter(mf)
  mf$period = cut(mf$starttime,breaks="week")
  mf$lanenumber[mf$detectorid %in% rampDets]="on-ramp"
  for(j in 1:nrow(mf)){
    if(!(mf$detectorid[i] %in% rampDets)){
      mf$lanenumber[i]=detectors$lanenumber[detectors$detectorid==mf$detectorid[i]]
    }
  }
  agg = ddply(mf,c("lanenumber","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
  agg$period = as.POSIXct(agg$period)
  agg$lanenumber = factor(agg$lanenumber)
  starkAgg[[i]]=agg
}

bigStark = starkAgg[[1]]

for (i in 2:length(starkAgg)){
  bigStark=rbind(bigStark,starkAgg[[i]])
}

saveRDS(stark,"6/starkRaw.rds")

theme_set(theme_grey(base_size = 20))
png("6/weekRampVolumes.png", width = 900,height = 400)
ggplot(bigStark,aes(x=period,y=volume,group = lanenumber, colour = lanenumber))+geom_line()+scale_x_datetime()+xlab("")+ylab("Vehicles Per Week")+ggtitle("Weekly Volumes on I-205 SB @ Stark/Washington")+scale_colour_discrete(name="Lane Number")
dev.off()

#boxplot(volume~hour,data = agg[as.character(agg$lanenumber)=="on-ramp",],main = "On-Ramp Volumes over January 2014",ylab = "Volume (veh/hr)",xaxt = "n")
#axis(1,at=timeTicks,labels = timeLabs)

# Issue 9: I-5 SB @ Carman, Missing data ----------------------------------
stas =3114
subStations = subset(stations,stations$stationid %in% stas)
dets = detectors$detectorid[detectors$stationid %in% stas ]
subDetectors = subset(detectors,detectors$detectorid %in% dets)
startDate = "2015-05-01"
endDate = "2015-05-31"
raw = dbGetQuery(con,freewayQuery(dets,startDate,endDate))
#Query returns zero rows

# Issue 10: I-5 SB @ Broadway, zero ramp volume during peak hours ---------
stas = 3121
subStations = subset(stations,stations$stationid %in% stas)
dets = detectors$detectorid[detectors$stationid %in% stas ]
subDetectors = subset(detectors,detectors$detectorid %in% dets)
startDate = "2015-05-01"
endDate = "2015-05-31"
raw = dbGetQuery(con,freewayQuery(dets,startDate,endDate))
#Data comes back fine. 

# Issue 11: US-26 @ Bethany Rd: Missing Lane 3 data -----------------------
stas = c(1136,5136)
subStations = subset(stations,stations$stationid %in% stas)
dets = detectors$detectorid[detectors$stationid %in% stas ]
subDetectors = subset(detectors,detectors$detectorid %in% dets)

# Issue 12: I-5 NB @ Macadam Ave: Missing ramp volumes in PM Peak  --------
stas = c(1015,5015)
subStations = subset(stations,stations$stationid %in% stas)
dets = detectors$detectorid[detectors$stationid %in% stas ]
subDetectors = subset(detectors,detectors$detectorid %in% dets)
startDate = "2015-02-01"
endDate = "2015-06-30"
query = freewayQuery(dets,startDate,endDate)
raw = dbGetQuery(con,query)
clean = filter(raw)
clean = join(clean,detectors,by="detectorid")
ramp = 5015
rampData = subset(raw,raw$stationid == ramp)
rampData$period = cut(rampData$starttime, breaks = "hour")
aggRampData = ddply(rampData,c("lanenumber","stationid","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
aggRampData$period = as.POSIXct(aggRampData$period)
ggplot(aggRampData,aes(x=period,y=volume))+geom_point()+scale_x_datetime()


# Issue 13: I-5 SB @ Jantzen Beach On-Ramp --------------------------------
stas = c(1026, 5026)
subStations = subset(stations,stations$stationid %in% stas)
dets = detectors$detectorid[detectors$stationid %in% stas ]
subDetectors = subset(detectors,detectors$detectorid %in% dets)
startDate = "2015-06-01"
endDate = "2015-06-30"
query = freewayQuery(dets,startDate,endDate)
raw = dbGetQuery(con,query)
raw = join(raw,detectors,by="detectorid")
#clean = filter(raw)
#clean = join(raw,detectors,by="detectorid")
raw$period = cut(raw$starttime,breaks = "hour")
raw$lanenumber[raw$stationid ==5026]= "on-ramp"
agg = aggTime(unAgg=raw,aggVars=c("lanenumber"),timeCut = "hour", hod = TRUE)
timeLabs = c("3 AM","6 AM","9 AM","12 PM","3 PM","6 PM","9 PM")
timeTicks = seq(3,21,3)
ggplot(agg,aes(x=hod,y=volume,group = lanenumber, colour = lanenumber))+geom_line()+scale_x_continuous(breaks = timeTicks,labels = timeLabs)+xlab("")+ylab("Volume (vehicles per hour)")+ggtitle("Mean hourly volumes over June 2015 seperated by lane for station 1026")

# Issue 17 ----------------------------------------------------------------
stations = c(3106,3107)
dets = detectors$detectorid[detectors$stationid %in% stations]
subDetectors= detectors[detectors$stationid %in% stations,]
startDate = "2015-04-01"
endDate = "2015-07-31"
query = freewayQuery(dets,startDate,endDate)
raw = dbGetQuery(con,query)
raw = join(raw,detectors,by="detectorid")
clean = filter(raw)
clean = join(clean,detectors,by="detectorid")
clean$period = cut(clean$starttime,breaks = "hour")
clean$lanenumber[clean$stationid ==5026]= "on-ramp"
clean$dow = weekdays(clean$starttime,abbreviate = TRUE)
mf = subset(clean,clean$dow != "Sat" & clean$dow != "Sun")
agg = ddply(mf,c("lanenumber","stationid","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
agg$period = as.POSIXct(agg$period)
agg$lanenumber = factor(agg$lanenumber)
agg$hour = hour(agg$period)
agg_hour = ddply(agg,c("lanenumber","stationid","hour"),summarise, volume = mean(volume),occupancy = mean(occupancy),speed=mean(speed))
timeLabs = c("3 AM","6 AM","9 AM","12 PM","3 PM","6 PM","9 PM")
timeTicks = seq(3,21,3)
theme_set(theme_grey(base_size = 25))
png("17/NB.png",width = 1200,height=500)
ggplot(agg_hour[agg_hour$stationid==3106,],aes(x=hour,y=speed,group = lanenumber, colour = lanenumber))+geom_line()+scale_x_continuous(breaks = timeTicks,labels = timeLabs)+xlab("")+ylab("Speed (mph)")+ggtitle("Mean hourly Speeds over weekdays in April to July 2015 \n seperated by lane for station 3106 (NB)")
dev.off()
png("17/SB.png",width = 1200,height=500)
ggplot(agg_hour[agg_hour$stationid==3107,],aes(x=hour,y=speed,group = lanenumber, colour = lanenumber))+geom_line()+scale_x_continuous(breaks = timeTicks,labels = timeLabs)+xlab("")+ylab("Speed (mph)")+ggtitle("Mean hourly Speeds over weekdays in April to July \n seperated by lane for station 3107 (SB)")
dev.off()

prescott = dbGetQuery(con,query)
prescott = join(prescott, detectors,by ="detectorid")
prescott$period = cut(prescott$starttime, breaks = "1 hour")
#prescott$hour = hour(prescott$starttime)


plyed = ddply(prescott,c("lanenumber","stationid","period"),summarise, volume = sum(volume),occupancy = mean(occupancy),speed=mean(speed))
plyed$period = as.POSIXct(plyed$period)
plyed$hour = hour(plyed$period)
plyed$lanenumber=factor(plyed$lanenumber)
plyed = ddply(plyed,c("lanenumber","stationid","hour"),summarise, volume = mean(volume),occupancy = mean(occupancy),speed=mean(speed))

#plyed$date = as.Date(plyed$starttime)

ggplot(plyed[plyed$stationid==3106,],aes(x=hour,y=volume,group =lanenumber, colour = lanenumber))+ geom_line()+scale_colour_discrete(name="Lane Number")

ggplot(plyed[plyed$stationid==3107,],aes(x=hour,y=volume,group =lanenumber, colour = lanenumber))+ geom_line()+scale_colour_discrete(name="Lane Number")

# Issue #19: US-26 @ 185th SB to EB: Missing Data -------------------------
stas= c(1084,5084)
subStations = subset(stations,stations$stationid %in% stas)
dets = detectors$detectorid[detectors$stationid %in% stas ]
subDetectors = subset(detectors,detectors$detectorid %in% dets)
monthRange = timeSequence("2014-07-01","2015-07-01",by="month")
monthList = list()

for (i in 1:length(monthRange)){
  month = substr(as.character(monthRange[i]),1,7)
  rawRequest = dbGetQuery(con,publicQuery(dets,month)) 
  dets = unique(rawRequest$detectorid)
  detList=list()
  for (j in 1:length(dets)){
    subRaw = subset(rawRequest,rawRequest$detectorid %in% dets[j])
    subRaw$hour = hour((subRaw$starttime))
    hours = sort(unique(subRaw$hour))
    hourList = list()
    for (k in 1:length(hours)){
      subRawHour = subset(subRaw,subRaw$hour==hours[k])
      hourList[[as.character(hours[k])]]=goodData(subRawHour)
    }
    detList[[as.character(dets[j])]]=hourList
  }
  monthList[[month]]=detList
  print(paste0("Finished good data filter for month ",i," of ",length(monthRange)))
}

minGood = 0.8
rowCount = 0

for (k in 1:length(monthList)){
  detList = monthList[[k]]
  for (i in 1:length(detList)){
    det = detList[[i]]
    for (j in 1:length(det)){
      good = det[[j]]
      if(good<minGood){
        rowCount = rowCount+1
      }
    }
  }
}


ri = 1
pltFrame = data.frame(matrix(nrow = rowCount,ncol=3))
colnames(pltFrame)=c("month","hour","lane")

for (k in 1:length(monthList)){
  detList = monthList[[k]]
  for (i in 1:length(detList)){
    det = detList[[i]]
    for (j in 1:length(det)){
      good= det[[j]]
      if(good<minGood){
        pltFrame$month[ri]=names(monthList[k])
        pltFrame$hour[ri]=names(det[j])
        did = names(detList[i])
        sid = detectors$stationid[detectors$detectorid==did]
        if(as.numeric(sid)>5000){
          pltFrame$lane[ri]="on-ramp"
        }else{
          pltFrame$lane[ri]=detectors$lanenumber[detectors$detectorid==did]
        }
        ri = ri+1
      }
    }
  }
}

pltFrame$hour=as.numeric(pltFrame$hour)
pltFrame$lane=factor(pltFrame$lane)

time = as.Date(strptimeDate(paste0(pltFrame$month,"-01"),format="%Y-%m-%d"))
pltFrame$date=time

theme_set(theme_grey(base_size = 35))
plt = ggplot(pltFrame,aes(x=hour,y=lane))+geom_point(color="red",size=4)+facet_wrap(~month,scales="fixed")
png("issuePlot.png",width = 2000,height=1200)
print(plt)
dev.off()


plt = ggplot(pltFrame,aes(x=date,y=lane))+geom_point(color="red",alpha=1/24,size=4)+xlab("Month")+ylab("Lane")+scale_x_date()

###June 2015
stas= c(1084,5084)
subStations = subset(stations,stations$stationid %in% stas)
dets = detectors$detectorid[detectors$stationid %in% stas ]
subDetectors = subset(detectors,detectors$detectorid %in% dets)
rawRequest = dbGetQuery(con,freewayQuery(dets = dets,startTime = "2015-06-01",endTime = "2015-08-07")) 
clean = filter(rawRequest)
joined = join(clean,detectors,by ="detectorid")
joined$period = cut(joined$starttime, breaks = "hour")
agg = ddply(joined,c("lanenumber","stationid","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
agg$period = as.POSIXct(agg$period)
agg$lanenumber[agg$stationid>5000]="on-ramp"
agg$lanenumber = factor(agg$lanenumber)
agg$hour = hour(agg$period)
agg_hour= ddply(agg,c("lanenumber","stationid","hour"),summarise, volume = mean(volume),occupancy = mean(occupancy),speed=mean(speed))
timeLabs = c("3 AM","6 AM","9 AM","12 PM","3 PM","6 PM","9 PM")
timeTicks = seq(3,21,3)
ggplot(agg_hour,aes(x=hour,y=speed,group =lanenumber, colour = lanenumber))+geom_line()+scale_colour_discrete(name="Lane Number")+ggtitle("Clackamas Hwy NB (Mean over 05-01-15 to 05-31-15)")+ylab("Mean Speed (mph)")+xlab("")+scale_x_continuous(breaks = timeTicks,labels = timeLabs)

#Caution: this takes a while
stas= c(1084,5084)
subStations = subset(stations,stations$stationid %in% stas)
dets = detectors$detectorid[detectors$stationid %in% stas ]
subDetectors = subset(detectors,detectors$detectorid %in% dets)
monthRange = timeSequence("2014-01-01","2015-06-01",by="month")
monthListUp = list()
for (i in 1:length(monthRange)){
  month = substr(as.character(monthRange[i]),1,7)
  raw = dbGetQuery(con,freewayQuery(dets,"2014-05-01","2014-05-07"))
  rawRequest =  dbGetQuery(con,publicQuery(dets,month))
  #incomplete = rawRequest[!complete.cases(rawRequest),]
  #complete = rawRequest[complete.cases(rawRequest),]
  dets = unique(rawRequest$detectorid)
  detList=list()
  for (j in 1:length(dets)){
    subRaw = subset(rawRequest,rawRequest$detectorid %in% dets[j])
    subRaw$hour = hour((subRaw$starttime))
    hours = sort(unique(subRaw$hour))
    hourList = list()
    for (k in 1:length(hours)){
      subRawHour = subset(subRaw,subRaw$hour==hours[k])
      flagCount = count(subRawHour$status)
      flagCount$prop = flagCount$freq/sum(flagCount$freq)
      flagList = list()
      for(l in 1:length(statFlags)){
        flagProp = flagCount$prop[flagCount$x==l-1]
        if(length(flagProp)==1){
          flagList[[statFlags[l]]]=flagProp
        }else{
          flagList[[statFlags[l]]]=0
        }
      }
      hourList[[as.character(hours[k])]]=flagList
    }
    detList[[as.character(dets[j])]]=hourList
  }
  monthListUp[[month]]=detList
  print(paste0("Finished status flags for month ",i," of ",length(monthRange)))
}

# Issue 20: I-84 WB west of Grand – Very High Lane Volumes ----------------
sta = 3126
dets = detectors$detectorid[detectors$stationid %in% sta]
subDetectors= detectors[detectors$stationid %in% sta,]
startDate = "2015-04-01"
endDate = "2015-07-31"
query = freewayQuery(dets,startDate,endDate)
raw = dbGetQuery(con,query)
raw = join(raw,detectors,by="detectorid")
raw$period = cut(raw$starttime,breaks = "1 hour")
raw$dow = weekdays(raw$starttime,abbreviate = TRUE)
mf = subset(raw,raw$dow != "Sat" & raw$dow != "Sun")
agg = ddply(mf,c("lanenumber","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
agg$tod = strftime(agg$period,format="%H:%M")
#agg_tod = ddply(agg,c("lanenumber","tod"),summarise, volume = mean(volume),occupancy = mean(occupancy),speed=mean(speed))
agg$time=as.POSIXct(agg$period)
agg$lanenumber=factor(agg$lanenumber)
agg$tod=hour(agg$time)
timeLabs = c("3 AM","6 AM","9 AM","12 PM","3 PM","6 PM","9 PM")
timeTicks = seq(3,21,3)
theme_set(theme_bw(base_size = 25))
plt = ggplot(agg,aes(x=tod,y=volume,group=lanenumber,colour=lanenumber))+geom_point(alpha=0.3)+geom_smooth()+
  xlab("Hour of Day")+ylab("Volume (vph)")+scale_x_continuous(breaks = timeTicks,labels = timeLabs)+scale_y_continuous(limits=c(0,3000))+
  ggtitle("Weekday volumes plotted by hour of day and lane for I-84 WB W of Grand")+scale_colour_discrete(name="Lane Number")
png("20/volumes.png",width=1200,height=600)
print(plt)
dev.off()
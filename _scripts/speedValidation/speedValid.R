require(portalr)
require(RPostgreSQL)
require(DBI)
require(timeSeries)
require(plyr)
require(ggplot2)
require(lubridate)
require(reshape2)
require(rjson)
require(portalr)

setwd("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/")

####Connect to Portal db
####Make sure you VPN into cecs network
db_cred = fromJSON(file="db_credentials.json")
con <- dbConnect(dbDriver("PostgreSQL"), host=db_cred$db_credentials$db_host, port= 5432, user=db_cred$db_credentials$db_user, password = db_cred$db_credentials$db_pass, dbname=db_cred$db_credentials$db_name)

####Relational Tables
stations= dbGetQuery(con,"SELECT * FROM public.stations")
detectors = dbGetQuery(con,"SELECT * FROM public.detectors")

#First round
speeds = read.csv("_data/freeway/speedValidation/speedValidation.csv",stringsAsFactors = FALSE)
speeds = speeds[,1:3]
speeds$time = paste0("2015/08/19 ",speeds$Timestamp)
speeds$time = as.POSIXct(strptime(speeds$time,format = "%Y/%m/%d %H:%M:%S"))

locs = unique(speeds$Location)
locs = locs[locs != ""]

indices = which(speeds$Location %in% locs)
indices[9]=nrow(speeds)

speedList = list()
for (i in 1:(length(locs))){
  subList = list()
  subList[["location"]]=locs[i]
  sub = speeds[(indices[i]:(indices[i+1]-1)),c(2,4)]
  subList[["observed"]]=sub[complete.cases(sub),]
  speedList[[i]]=subList
}

speedList[[1]]$highway="I-5 SB"
speedList[[1]]$crossStreet ="Spring Garden"
speedList[[1]]$lane=1
speedList[[1]]$sid = 1108
speedList[[2]]$highway="I-5 SB"
speedList[[2]]$crossStreet ="Spring Garden"
speedList[[2]]$lane=3
speedList[[2]]$sid = 1108
speedList[[3]]$highway="I-5 NB"
speedList[[3]]$crossStreet ="Spring Garden"
speedList[[3]]$lane=1
speedList[[3]]$sid = 1011
speedList[[4]]$highway="I-5 NB"
speedList[[4]]$crossStreet ="Spring Garden"
speedList[[4]]$lane=3
speedList[[4]]$sid = 1011
speedList[[5]]$highway="I-205 NB"
speedList[[5]]$crossStreet ="Park Place"
speedList[[5]]$lane=1
speedList[[5]]$sid = 1144
speedList[[6]]$highway="I-205 NB"
speedList[[6]]$crossStreet ="Park Place"
speedList[[6]]$lane=3
speedList[[6]]$sid = 1144
speedList[[7]]$highway="I-84 WB"
speedList[[7]]$crossStreet ="33rd Ave"
speedList[[7]]$lane=3
speedList[[7]]$sid = 1062
speedList[[8]]$highway="I-84 WB"
speedList[[8]]$crossStreet ="33rd Ave"
speedList[[8]]$lane=1
speedList[[8]]$sid = 1062

#Second Round
speeds = read.csv("_data/freeway/speedValidation/speedValidation2.csv",stringsAsFactors = FALSE)
speeds = speeds[,1:3]
speeds$time = paste0("2015/09/03 ",speeds$Timestamp)
speeds$time = as.POSIXct(strptime(speeds$time,format = "%Y/%m/%d %H:%M:%S"))

locs = unique(speeds$Location)
locs = locs[locs != ""]

indices = which(speeds$Location %in% locs)
indices[5]=nrow(speeds)

for (i in 1:(length(locs))){
  subList = list()
  subList[["location"]]=locs[i]
  sub = speeds[(indices[i]:(indices[i+1]-1)),c(2,4)]
  subList[["observed"]]=sub[complete.cases(sub),]
  speedList[[i+8]]=subList
}

speedList[[9]]$highway="US-26 WB"
speedList[[9]]$crossStreet ="185th Ave."
speedList[[9]]$lane=1
speedList[[9]]$sid = 1129
speedList[[10]]$highway="US-26 WB"
speedList[[10]]$crossStreet ="185th Ave."
speedList[[10]]$lane=2
speedList[[10]]$sid = 1129
speedList[[11]]$highway="I-205 SB"
speedList[[11]]$crossStreet ="Clackamas Hwy"
speedList[[11]]$lane=1
speedList[[11]]$sid = 1100
speedList[[12]]$highway="I-205 SB"
speedList[[12]]$crossStreet ="Clackamas Hwy"
speedList[[12]]$lane=3
speedList[[12]]$sid = 1100

saveRDS(speedList,"_data/freeway/speedValidation/speedList.rds")
speedList=readRDS("_data/freeway/speedValidation/speedList.rds")

for (i in 1:length(speedList)){
  sid = speedList[[i]]$sid
  lane = speedList[[i]]$lane
  did = detectors$detectorid[detectors$stationid == sid & detectors$lanenumber == lane & is.na(detectors$end_date)]
  speedList[[i]]$did = did
}

for(i in 1:length(speedList)){
  did = speedList[[i]]$did
  obs = speedList[[i]]$observed
  start = min(obs$time)
  start = as.character(round(start,"mins"))
  end = max(obs$time)
  end = as.character(round(end,"mins"))

  request = freewayData(con,did,start,end)
  speedList[[i]]$portal = request

  histStart = as.POSIXct(start)
  week(histStart)<-week(histStart)-9
  histStart = as.character(histStart)
  histStartSeq = as.character(timeSequence(from = histStart, to = start, by = "week"))

  histEnd = as.POSIXct(end)
  week(histEnd)<-week(histEnd)-9
  histEnd = as.character(histEnd)
  histEndSeq = as.character(timeSequence(from = histEnd, to = end, by = "week"))

  query = paste0("SELECT * FROM freeway.data WHERE ((starttime >='",histStartSeq[1],"' AND starttime <='",histEndSeq[1],"')")
  for (j in 2:length(histStartSeq)){
    query = paste0(query," OR (starttime >='",histStartSeq[j],"' AND starttime <='",histEndSeq[j],"')")
  }
  query = paste0(query,") AND detectorid = ",did)

  request = dbGetQuery(con,query)
  request$time = paste0(hour(request$starttime),":",minute(request$starttime),":",second(request$starttime))

  agg = ddply(request,c("time"),summarise,speed=mean(speed,na.rm=TRUE))
  if(i %in% 1:8){
    agg$time = as.POSIXct(strptime(paste0("2015-08-19 ",agg$time),format= "%Y-%m-%d %H:%M:%S"))
  }else{
    agg$time = as.POSIXct(strptime(paste0("2015-09-03 ",agg$time),format= "%Y-%m-%d %H:%M:%S"))
  }
  

  speedList[[i]]$hist = agg
}

theme_set(theme_grey(base_size = 25))

for (i in 1:length(speedList)){
  obs = speedList[[i]]$observed
  obs$period = cut(obs$time,"1 min")
  obsAgg = ddply(obs,c("period"),summarise,speed = mean(Reading))

  portal = speedList[[i]]$portal
  portal$period = cut(portal$starttime,"1 min")
  portalAgg = ddply(portal,c("period"),summarise,speed = mean(speed))

  hist = speedList[[i]]$hist
  hist$period = cut(hist$time,"1 min")
  histAgg = ddply(hist,c("period"),summarise,speed = mean(speed))

  joined = join(obsAgg,portalAgg,by="period")
  joined = join(joined,histAgg, by ="period")
  colnames(joined)=c("time","observed","portal","hist")
  melted = melt(joined,id="time")
  melted$time=as.POSIXct(melted$time)

  speedList[[i]]$melted = melted


  plot=ggplot(melted,aes(x=time,y=value,group=variable,colour=variable))+geom_smooth()+scale_x_datetime()+
    ggtitle(paste0(speedList[[i]]$highway," @ ",speedList[[i]]$crossStreet,", Lane",speedList[[i]]$lane))+
    ylab("Speed (mph)")+xlab("")

  png(paste0("_results/img/speedValidation/",speedList[[i]]$location,".png"),width=900,height=600)
  print(plot)
  dev.off()
}

saveRDS(speedList,"_data/freeway/speedValidation/processedSpeedData.rds")


# Meta --------------------------------------------------------------------
# @file laneNumberCheck.R
# @description This script checks the speed differential between the inside-most and outside-most lane
# @author Bryan Blanc <bryanpblanc@gmail.com>

# Initialization ----------------------------------------------------------
library(RPostgreSQL)
library(DBI)
library(timeSeries)
library(plyr)
library(ggplot2)
library(lubridate)
library(mail)
library(dplyr)
library(rgdal)
library(rjson)

#Set working directory
setwd("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations")

#Connect to Portal db, Make sure you VPN into PSU CECS network
db_cred = fromJSON(file="db_credentials.json")
con <- dbConnect(dbDriver("PostgreSQL"), host=db_cred$db_credentials$db_host, port= 5432, user=db_cred$db_credentials$db_user, password = db_cred$db_credentials$db_pass, dbname=db_cred$db_credentials$db_name)

####Relational Tables
stations= dbGetQuery(con,"SELECT * FROM public.stations")
detectors = dbGetQuery(con,"SELECT * FROM public.detectors")

oregonStationGeom = readOGR("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/_data/GIS/","oregonStations")
orStationData = oregonStationGeom@data
currentStations = subset(orStationData,is.na(orStationData$end_date))
stationVec = sort(unique(currentStations$stationid))

#Load stock functions
source("_functions/querying.R")
source("_functions/aggregation.R")

# Functions ---------------------------------------------------------------
laneNumsSuspect= function(station_id,startTime,endTime,percentile){
  dets = detectors$detectorid[detectors$stationid %in% station_id]
  if(length(dets>0)){
    query = freewayQuery(dets,startTime,endTime)
    rawRequest = dbGetQuery(con,query)
    if (nrow(rawRequest)>0){
      raw = join(rawRequest,detectors,by="detectorid")
      raw$period = cut(raw$starttime,breaks = "hour")
      raw = subset(raw,raw$volume !=0)
      if(nrow(raw)>0){
        agg = ddply(raw,c("lanenumber","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
        agg$period = as.POSIXct(agg$period)
        agg$lanenumber = factor(agg$lanenumber)
        lanes = as.character(unique(agg$lanenumber))
        if(length(lanes)>1){
          numLanes = length(lanes)
          agg$hour = hour(agg$period)
          agg_hour = ddply(agg,c("lanenumber","hour"),function(X) data.frame(volume=mean(X$volume), occupancy = mean(X$occupancy), speed = mean(X$speed)))
          diff = agg_hour$speed[as.character(agg_hour$lanenumber) ==lanes[1]] - 
            agg_hour$speed[as.character(agg_hour$lanenumber) ==lanes[length(lanes)]]
          return(as.numeric(quantile(diff,percentile,na.rm=TRUE)))
        }else{
          return(NA)
        }
      }else{
        return(NA)
      }
    }else{
      return(NA)
    }
  }else{
    return(NA)
  }
}


# Implementation ----------------------------------------------------------
stationCheck = data.frame(matrix(nrow = length(stationVec),ncol=4))
colnames(stationCheck)=c("stationid","pDiff_10","multiLane","flag")
stationCheck$stationid = stationVec
stationCheck$multiLane=FALSE

for(i in 1:nrow(stationCheck)){
  sid = stationCheck$stationid[i]
  dets = detectors[detectors$stationid %in% sid,]
  if(length(unique(dets$lanenumber))>1){
    stationCheck$multiLane[i] = TRUE
  }
}

stationCheck = subset(stationCheck,stationCheck$multiLane==TRUE)

for (i in 1:nrow(stationCheck)){
  station_id = stationCheck$stationid[i]
  stationCheck$pDiff_10[i] = laneNumsSuspect(station_id,startTime = "2015-06-01",endTime ="2015-06-30", percentile = 0.05)
  print(paste0("Checked station # ",i," of ",nrow(stationCheck)))
}

stationCheck$flag=FALSE
stationCheck$flag[stationCheck$pDiff_10 <0]=TRUE
stationCheckFinal=stationCheck[!duplicated(stationCheck$stationid),]

saveRDS(stationCheckFinal,"_data/freeway/laneNumberCheck/laneNumberCheck.rds")

#if(i == nrow(stationCheck)){
#  sendmail("bblanc@pdx.edu",subject="Finished speed calculation for lanes",message="You're awesome!", password="rmail")
#}


# Plotting ----------------------------------------------------------------
laneCheck = readRDS("_data/freeway/laneNumberCheck/laneNumberCheck.rds")
laneCheck$flag=FALSE
laneCheck$flag[laneCheck$pDiff_10 <0]=TRUE
laneFrame = laneCheck[complete.cases(laneCheck),]
quants = quantile(laneFrame$pDiff_10,c(0.25,0.75))
iqr = as.numeric(quants[2]-quants[1])
fences = as.numeric(c(quants[1]-3*iqr,quants[1]-1.5*iqr,quants[2]+1.5*iqr,quants[2]+3*iqr))
laneFrame$out = "OK"
laneFrame$out[(laneFrame$pDiff_10 <fences[2] & laneFrame$pDiff_10 >= fences[1])|(laneFrame$pDiff_10 <=fences[4] & laneFrame$pDiff_10 > fences[3])]="Minor"
laneFrame$out[(laneFrame$pDiff_10 <fences[1])|(laneFrame$pDiff_10 >fences[4])]="Major"
laneFrame$out=factor(laneFrame$out,ordered = TRUE)


stationsLoc=read.csv("_data/GIS/orStationsLoc.csv")
flagged = subset(laneFrame,laneFrame$out == "Minor" | laneFrame$out =="Major" )

startTime = "2015-06-01"
endTime = "2015-06-30"

theme_set(theme_grey(base_size=50))

for (i in 1:length(flagged$stationid)){
       sid = flagged$stationid[i]
       stationName =stations$locationtext[stations$stationid==sid]
       dets = detectors$detectorid[detectors$stationid %in% sid]
       if(length(dets>0)){
           query = paste0("SELECT * FROM freeway.data WHERE (starttime >='",startTime,"T00:00:00' AND starttime <='",endTime,"T23:59:59') AND (detectorid = ",dets[1])
           if(length(dets)>1){
               for (i in 2:length(dets)){
                   query = paste0(query," OR detectorid = ",dets[i])
                 }
             }
           query = paste0(query,")")
           rawRequest = dbGetQuery(con,query)
           if (nrow(rawRequest)>0){
               raw = join(rawRequest,detectors,by="detectorid")
               raw$dow = weekdays(raw$starttime)
               raw = subset(raw,(raw$dow != "Saturday" & raw$dow != "Sunday"))
               raw$period = cut(raw$starttime,breaks = "1 hour")
               raw = subset(raw,raw$volume !=0)
               if(as.numeric(quantile(raw$volume,0.05,na.rm=TRUE))>0){
                   agg = ddply(raw,c("lanenumber","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
                 }else{
                  agg = ddply(raw,c("lanenumber","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = mean(X$speed)))
               }
               agg$period = as.POSIXct(agg$period)
               agg$lanenumber = factor(agg$lanenumber)
               lanes = as.character(unique(agg$lanenumber))
               numLanes = length(lanes)
               agg$hour = hour(agg$period)
               if(as.numeric(quantile(agg$volume,0.05,na.rm=TRUE))>0){
                   agg_hour = ddply(agg,c("lanenumber","hour"),function(X) data.frame(volume=mean(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
               }else{
                     agg_hour = ddply(agg,c("lanenumber","hour"),function(X) data.frame(volume=mean(X$volume), occupancy = mean(X$occupancy), speed = mean(X$speed)))
               }
               timeTicks = c(3,6,9,12,15,18,21)
               timeLabs = c("3 AM","6 AM","9 AM","12 PM","3 PM","6 PM","9 PM")
               
               png(paste0("plots/",sid,".png"),height = 1000, width = 1600)
               print(ggplot(agg_hour, aes(x=hour,y=speed,group = lanenumber))+geom_line(aes(colour = lanenumber),size =2)+scale_color_discrete(name="Lane Number")+xlab("")+ylab("Speed (mph)")+scale_x_continuous(breaks=timeTicks,labels = timeLabs)+ggtitle(paste0("Weekday Weighted Mean Speed During June 2015 \n @ Station #",sid," (", stationName,")")))
               dev.off() 
             }
       }
       print(paste0("plotted for station #",i," of ",nrow(flagged)))
}

map= leaflet() %>% addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>% setView(-122.6662589, 45.5317385, zoom = 10)
for(i in 1:nrow(flagged)){
         sid = flagged$stationid[i]
         plot_url = paste0("http://bigtransportdata.com/_portal/laneNumbers/plots/",sid,".png")
         flagged$html[i] = paste0("<div align='center'><a href='",plot_url,"' target='_blank'><img src='",plot_url,"' alt='VolumePlot' height='200' width='300'></a></div>")
         if(sid >= 5000){
           if(length(stationsLoc$Y[stationsLoc$stationid==sid-4000]==1)){
             flagged$lat[i]=stationsLoc$Y[stationsLoc$stationid==sid-4000]
           }
           if(length(stationsLoc$X[stationsLoc$stationid==sid-4000])==1){
             flagged$lng[i]=stationsLoc$X[stationsLoc$stationid==sid-4000]
           }
         }else{
           if(length(stationsLoc$Y[stationsLoc$stationid==sid]==1)){
             flagged$lat[i]=stationsLoc$Y[stationsLoc$stationid==sid]
           }
           if(length(stationsLoc$X[stationsLoc$stationid==sid])==1){
             flagged$lng[i]=stationsLoc$X[stationsLoc$stationid==sid]
           }
         }
       }
redFlag = makeIcon(iconUrl = "http://bigtransportdata.com/_portal/laneNumbers/redFlag.png",iconWidth = 37,iconHeight = 41,iconAnchorX = 2,iconAnchorY = 40)
yellowFlag = makeIcon(iconUrl = "http://bigtransportdata.com/_portal/laneNumbers/yellowFlag.png",iconWidth = 37,iconHeight = 41,iconAnchorX = 2,iconAnchorY = 40)  

map =addMarkers(map=map,icon=redFlag,lng =flagged$lng[flagged$out=="Major"],lat=flagged$lat[flagged$out=="Major"],popup = flagged$html[flagged$out=="Major"])
map =addMarkers(map=map,icon=yellowFlag,lng =flagged$lng[flagged$out=="Minor"],lat=flagged$lat[flagged$out=="Minor"],popup = flagged$html[flagged$out=="Minor"])

saveWidget(map, "laneNumFlagsMap.html")

library(RPostgreSQL)
library(DBI)
library(timeSeries)
library(plyr)
library(ggplot2)
library(lubridate)
library(mail)
library(dplyr)

sysName = Sys.info()["sysname"]
if (sysName == "Linux"){
  setwd("~/_ODOT_Portal/investigations/laneNumbers")
}else{
  setwd("/Users/bblanc/OneDrive/_Portal_ODOT/investigations/laneNumbers/")
}

####Connect to Portal db
####Make sure you VPN into cecs network
db_cred = fromJSON(file="/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/db_credentials.json")
con <- dbConnect(dbDriver("PostgreSQL"), host=db_cred$db_credentials$db_host, port= 5432, user=db_cred$db_credentials$db_user, password = db_cred$db_credentials$db_pass, dbname=db_cred$db_credentials$db_name)


####Relational Tables
stations= dbGetQuery(con,"SELECT * FROM public.stations")
detectors = dbGetQuery(con,"SELECT * FROM public.detectors")

laneNumsSuspect= function(station_id,detectors,startTime,endTime,percentile){
  dets = detectors$detectorid[detectors$stationid %in% station_id]
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
      raw$period = cut(raw$starttime,breaks = "1 hour")
      if(as.numeric(quantile(raw$volume,0.05,na.rm=TRUE))>0){
        agg = ddply(raw,c("lanenumber","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
      }else{
        agg = ddply(raw,c("lanenumber","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = mean(X$speed)))
      }
      agg$period = as.POSIXct(agg$period)
      agg$lanenumber = factor(agg$lanenumber)
      lanes = as.character(unique(agg$lanenumber))
      if(length(lanes)>1){
        numLanes = length(lanes)
        agg$hour = hour(agg$period)
        if(as.numeric(quantile(agg$volume,0.05,na.rm=TRUE))>0){
          agg_hour = ddply(agg,c("lanenumber","hour"),function(X) data.frame(volume=mean(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
        }else{
          agg_hour = ddply(agg,c("lanenumber","hour"),function(X) data.frame(volume=mean(X$volume), occupancy = mean(X$occupancy), speed = mean(X$speed)))
        }
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
  
  
  #   compare = data.frame(matrix(nrow=24,ncol=numLanes))
#   laneCols = paste0(lanes[1:(length(lanes)-1)],"_diff_",lanes[2:length(lanes)])
#   colnames(compare)=c("hour",laneCols)
#   compare$hour = 0:23
#   for (i in 1:length(laneCols)){
#     laneCol_1 = substr(laneCols[i],1,1)
#     laneCol_2 = substr(laneCols[i],8,8)
#     diff = agg_hour$speed[as.character(agg_hour$lanenumber) ==laneCol_2]-
#       agg_hour$speed[as.character(agg_hour$lanenumber) ==laneCol_1]
#     compare[,laneCols[i]]=diff
#   }
  
  #timeLabs = c("3 AM","6 AM","9 AM","12 PM","3 PM","6 PM","9 PM")
  #timeTicks = seq(3,21,3)
  #ggplot(agg_hour,aes(x=hour,y=speed,group=lanenumber,colour=lanenumber))+geom_line()+scale_x_continuous(breaks = timeTicks,labels = timeLabs)+scale_colour_discrete(name="Lane Number") 
}

stationCheck = data.frame(matrix(nrow = nrow(stations),ncol=3))
colnames(stationCheck)=c("stationid","pDiff_10","flag")
stationCheck$stationid = stations$stationid

for (i in 218:nrow(stationCheck)){
  station_id = stationCheck$stationid[i]
  stationCheck$pDiff_10[i] = laneNumsSuspect(station_id,detectors,startTime = "2015-05-01",endTime ="2015-05-30", percentile = 0.1)
  print(paste0("Checked station # ",i," of ",nrow(stationCheck)))
}

stationCheck$flag[stationCheck$pDiff_10 <0]=TRUE

saveRDS(stationCheck,"stationCheck.rds")

if(i == nrow(stationCheck)){
  sendmail("bblanc@pdx.edu",subject="Finished speed calculation for lanes",message="You're awesome!", password="rmail")
}

###plotting

stationCheck = readRDS("stationCheck.rds")
stationsShp = readOGR(dsn="/Users/bblanc/OneDrive/_Portal_ODOT/",layer ="stations")
stationsLoc=stationsShp@data
flagged = subset(stationCheck,stationCheck$flag==TRUE)

theme_set(theme_grey(basesize=50))

for (i in 1:nrow(flagged)){
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
             print(ggplot(agg_hour, aes(x=hour,y=speed,group = lanenumber))+geom_line(aes(colour = lanenumber),size =2)+scale_color_discrete(name="Lane Number")+xlab("")+ylab("Speed (mph)")+scale_x_continuous(breaks=timeTicks,labels = timeLabs)+ggtitle(paste0("Weekday Weighted Mean Speed During May 2015 \n @ ", stationName)))
             dev.off() 
           }
     }
     print(paste0("plotted for station #",i," of ",nrow(flagged)))
}

map= leaflet() %>% addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>% setView(-122.6662589, 45.5317385, zoom = 10)
for(i in 1:nrow(flagged)){
         sid = flagged$stationid[i]
         plot_url = paste0("http://bigtransportdata.com/laneNumbers/plots/",sid,".png")
         flagged$html[i] = paste0("<div align='center'><a href='",plot_url,"' target='_blank'><img src='",plot_url,"' alt='VolumePlot' height='200' width='300'></a></div>")
         if(sid >= 5000){
             flagged$lat[i]=stationsLoc$lat[stationsLoc$stationid==sid-4000]
             flagged$lng[i]=stationsLoc$lng[stationsLoc$stationid==sid-4000]
          }else if(sid ==1089){
            flagged$lat[i]=45.506521
            flagged$lng[i]=-122.770808
          }else{
              flagged$lat[i]=stationsLoc$lat[stationsLoc$stationid==sid]
              flagged$lng[i]=stationsLoc$lng[stationsLoc$stationid==sid]
          }
       }
redFlag = makeIcon(iconUrl = "http://bigtransportdata.com/laneNumbers/redFlag.png",iconWidth = 37,iconHeight = 41)
  
map =addMarkers(map=map,icon=redFlag,lng =flagged$lng,lat=flagged$lat,popup = flagged$html)
saveWidget(map, "flaggedSpeedMap.html")
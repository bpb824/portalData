library(RPostgreSQL)
library(DBI)
library(timeSeries)
library(plyr)
library(ggplot2)
library(lubridate)
library(rjson)
library(rgdal)
require(portalr)

setwd("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/")

####Connect to Portal db
####Make sure you VPN into cecs network
db_cred = fromJSON(file="db_credentials.json")
con <- dbConnect(dbDriver("PostgreSQL"), host=db_cred$db_credentials$db_host, port= 5432, user=db_cred$db_credentials$db_user, password = db_cred$db_credentials$db_pass, dbname=db_cred$db_credentials$db_name)

####Relational Tables
stations= dbGetQuery(con,"SELECT * FROM public.stations")
highways = dbGetQuery(con,"SELECT * FROM public.highways")
detectors = dbGetQuery(con,"SELECT * FROM public.detectors")
corridors  = dbGetQuery(con,"SELECT * FROM public.corridors")
corridor_stations = dbGetQuery(con,"SELECT * FROM public.corridorstations")
#
# ##Load standard PORTAL functions
# source("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/_codeResources/portalFxns.R")

oregonStationGeom = readOGR("_data/GIS/","oregonStations")
orStationData = oregonStationGeom@data
currentStations = subset(orStationData,is.na(orStationData$end_date))
stationVec = sort(unique(currentStations$stationid))

juneRaw = readRDS("_data/freeway/juneData.rds")

juneAgg= list()
for (i in 1:length(juneRaw)){
  sid = juneRaw[[i]]$station_id
  if(sid %in% stationVec){
    sData = juneRaw[[i]]$data
    if(nrow(sData)>0){
      clean = portalr::filterFreeway(sData)
      if(nrow(clean)>0){
        clean = join(sData,detectors,by="detectorid")
        clean$period = cut(clean$starttime,breaks="1 hour")
        agg = ddply(clean,c("lanenumber","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
        agg$period=as.POSIXct(agg$period)
        agg$dow = weekdays(agg$period,abbreviate=TRUE)
        agg$hour = hour(agg$period)
        agg_hd = ddply(agg,c("lanenumber","hour","dow"),function(X) data.frame(volume=mean(X$volume), occupancy = mean(X$occupancy), speed = mean(X$speed)))
        juneAgg[[as.character(sid)]] = agg_hd
      }else{
        juneAgg[[as.character(sid)]] = NA
      }

    }else{
      juneAgg[[as.character(sid)]] = NA
    }
  }
  print (i)
}

saveRDS(juneAgg,"_data/freeway/juneAggData_clean.rds")
saveRDS(currentStations, "_data/freeway/stationMeta.rds")

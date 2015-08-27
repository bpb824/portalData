library(RPostgreSQL)
library(DBI)
library(timeSeries)
library(plyr)
library(ggplot2)
library(lubridate)

setwd("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/histBuilder/")

####Connect to Portal db
####Make sure you VPN into cecs network
db_cred = fromJSON(file="/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/db_credentials.json")
con <- dbConnect(dbDriver("PostgreSQL"), host=db_cred$db_credentials$db_host, port= 5432, user=db_cred$db_credentials$db_user, password = db_cred$db_credentials$db_pass, dbname=db_cred$db_credentials$db_name)


####Relational Tables
stations= dbGetQuery(con,"SELECT * FROM public.stations")
highways = dbGetQuery(con,"SELECT * FROM public.highways")
detectors = dbGetQuery(con,"SELECT * FROM public.detectors")
corridors  = dbGetQuery(con,"SELECT * FROM public.corridors")
corridor_stations = dbGetQuery(con,"SELECT * FROM public.corridorstations")

##Load standard PORTAL functions
source("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/_codeResources/portalFxns.R")

juneRaw = readRDS("juneData.rds")

juneAgg= list()
for (i in 1:length(juneRaw)){
  sid = juneRaw[[i]]$station_id
  sData = juneRaw[[i]]$data
  if(nrow(sData)>0){
    #clean = filter(sData)
    #if(nrow(clean)>0){
      clean = join(sData,detectors,by="detectorid")
      clean$period = cut(clean$starttime,breaks="1 hour")
      agg = ddply(clean,c("lanenumber","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
      agg$period=as.POSIXct(agg$period)
      agg$dow = weekdays(agg$period,abbreviate=TRUE)
      agg$hour = hour(agg$period)
      agg_hd = ddply(agg,c("lanenumber","hour","dow"),function(X) data.frame(volume=mean(X$volume), occupancy = mean(X$occupancy), speed = mean(X$speed)))
      station = list()
      station[["station_id"]]=sid
      station[["data"]]=agg_hd
      juneAgg[[i]]=station
    #}else{
    #  station = list()
    #  station[["station_id"]]=sid
    #  station[["data"]]=NA
    #  juneAgg[[i]] = station
    #}
    
  }else{
    station = list()
    station[["station_id"]]=sid
    station[["data"]]=NA
    juneAgg[[i]] = station
  }
  print (i)
}

saveRDS(juneAgg,"juneAggData.rds")

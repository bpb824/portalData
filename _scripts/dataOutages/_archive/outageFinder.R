library(RPostgreSQL)
library(DBI)
library(timeSeries)
library(plyr)
library(ggplot2)
library(lubridate)
library(mail)
library(rjson)

setwd("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/")

####Connect to Portal db
####Make sure you VPN into cecs network
db_cred = fromJSON(file="/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/db_credentials.json")
con <- dbConnect(dbDriver("PostgreSQL"), host=db_cred$db_credentials$db_host, port= 5432, user=db_cred$db_credentials$db_user, password = db_cred$db_credentials$db_pass, dbname=db_cred$db_credentials$db_name)


####Relational Tables
stations= dbGetQuery(con,"SELECT * FROM public.stations")
detectors = dbGetQuery(con,"SELECT * FROM public.detectors")

missingData = function(station_id,detectors,startTime,endTime,percentile){
  dets = detectors$detectorid[detectors$stationid %in% station_id]
  if(length(dets)>0){
    query = paste0("SELECT * FROM freeway.data WHERE (starttime >='",startTime,"T00:00:00' AND starttime <='",endTime,"T23:59:59') AND (detectorid = ",dets[1])
    if(length(dets)>1){
      for (i in 2:length(dets)){
        query = paste0(query," OR detectorid = ",dets[i])
      }
    }
    query = paste0(query,")")
    rawRequest = dbGetQuery(con,query)
    if(nrow(rawRequest)==0){
      tableMonth= gsub("-","_",substr(startTime,1,7))
      query = paste0("SELECT * FROM public.loopdata_",tableMonth," WHERE (detectorid = ",dets[1])
      if(length(dets)>1){
        for (k in 2:length(dets)){
          query = paste0(query," OR detectorid = ",dets[k])
        }
      }
      query = paste0(query,")")
      rawRequest = dbGetQuery(con,query)
    }
    if (nrow(rawRequest)>0){
      complete = rawRequest[complete.cases(rawRequest),]
      if(nrow(complete)>0){
        raw = join(complete,detectors,by="detectorid")
        raw$period = cut(raw$starttime,breaks = "1 hour")
        agg = ddply(raw,c("lanenumber","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
        agg$period = as.POSIXct(agg$period)
        agg$lanenumber = factor(agg$lanenumber)
        lanes = as.character(unique(agg$lanenumber))
        numLanes = length(lanes)
        agg$hour = hour(agg$period)
        agg_hour = ddply(agg,c("lanenumber","hour"),function(X) data.frame(volume=mean(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
        volume = as.numeric(quantile(agg_hour$volume,percentile, na.rm = TRUE))
        speed = as.numeric(quantile(agg_hour$speed,percentile, na.rm = TRUE))
        occupancy = as.numeric(quantile(agg_hour$occupancy,percentile, na.rm = TRUE))
        if (volume <= 1 | speed <=1 | occupancy <=0.01){
          #Flag 1 means data were very low (probably zero) values
          flag = 1
        }else{
          #Flag 0 means no flag
          flag = 0
        }
        result = list(c(volume,speed,occupancy),flag)
        return(result)
      }else{
        #Flag 2 means query turned up zero rows of complete data (only missing data rows)
        result = list(c(NA,NA,NA),2)
        return(result)
      }
    }else{
      #Flag 3 means query turned up zero rows of data
      result = list(c(NA,NA,NA),3)
      return(result)
    }
  }else{
    #Flag 4 means no detectors could be found for this station id
    result = list(c(NA,NA,NA),4)
    return(result)
  }
}

stationCheck = data.frame(matrix(nrow = nrow(stations),ncol=5))
colnames(stationCheck)=c("stationid","volume","speed","occupancy","flag")
stationCheck$stationid = stations$stationid



loopStart = Sys.time()
for (i in 1:nrow(stationCheck)){
  station_id = stationCheck$stationid[i]
  result = missingData(station_id,detectors,"2015-06-01","2015-06-30",0.05)
  stationCheck[i,2:4]=result[[1]]
  stationCheck$flag[i]=result[[2]]
  totalTime = as.numeric(difftime(Sys.time(),loopStart,units ="mins"))
  print(paste0("Checked station # ",i," of ",nrow(stationCheck),", ",totalTime," minutes so far"))
}

saveRDS(stationCheck,"newOutageFlags.rds")

# Meta --------------------------------------------------------------------
# @file missingDataFinder.R
# @description This script finds missing data within the PORTAL database. 
# @author Bryan Blanc <bryanpblanc@gmail.com>

# Initialization ----------------------------------------------------------
require(RPostgreSQL)
require(DBI)
require(timeSeries)
require(plyr)
require(ggplot2)
require(lubridate)
require(mail)
require(rjson)

setwd("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations")

####Connect to Portal db
####Make sure you VPN into cecs network
db_cred = fromJSON(file="db_credentials.json")
con <- dbConnect(dbDriver("PostgreSQL"), host=db_cred$db_credentials$db_host, port= 5432, user=db_cred$db_credentials$db_user, password = db_cred$db_credentials$db_pass, dbname=db_cred$db_credentials$db_name)

####Relational Tables
stations= dbGetQuery(con,"SELECT * FROM public.stations")
detectors = dbGetQuery(con,"SELECT * FROM public.detectors")

# functions ---------------------------------------------------------------
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
    if (nrow(rawRequest)>0){
      raw = join(rawRequest,detectors,by="detectorid")
      raw$period = cut(raw$starttime,breaks = "15 min")
      agg = ddply(raw,c("lanenumber","period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
      agg$period = as.POSIXct(agg$period)
      agg$lanenumber = factor(agg$lanenumber)
      lanes = as.character(unique(agg$lanenumber))
      if(length(lanes)>1){
        numLanes = length(lanes)
        agg$hour = hour(agg$period)
        agg_hour = ddply(agg,c("lanenumber","hour"),function(X) data.frame(volume=mean(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
        volume = as.numeric(quantile(agg_hour$volume,percentile, na.rm = TRUE))
        speed = as.numeric(quantile(agg_hour$speed,percentile, na.rm = TRUE))
        occupancy = as.numeric(quantile(agg_hour$occupancy,percentile, na.rm = TRUE))
        if (volume <= 1 | speed <=1 | occupancy <=0.01){
          flag = TRUE
        }else{
          flag = FALSE
        }
        result = list(c(volume,speed,occupancy),flag)
        return(result)
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

periodMissing=function(missingReport, monthRange,stations,detectors){
  
  ###Station Flags
  stationFlagReports = list()
  
  for (i in 1:nrow(stationFlagged)){
    sid = stationFlagged$stationid[i]
    values = stationFlagged[i,]
    #     speedFlag = FALSE
    #     volumeFlag = FALSE
    #     occupancyFlag = FALSE
    #     if(values$occupancy<1){occupancyFlag=TRUE}
    #     if(values$speed<1){speedFlag=TRUE}
    #     if(values$volume<1){volumeFlag=TRUE}
    stationReport = list()
    stationReport[["Station_ID"]]=sid
    dets = detectors$detectorid[detectors$stationid %in% sid]
    outageReports = list()
    rc =1
    for (j in length(monthRange):2){
      startTime = as.character(monthRange[j-1])
      endTime = as.character(monthRange[j])
      if(length(dets)>0){
        query = paste0("SELECT * FROM freeway.data WHERE (starttime >='",startTime,"T00:00:00' AND starttime <='",endTime,"T23:59:59') AND (detectorid = ",dets[1])
        if(length(dets)>1){
          for (k in 2:length(dets)){
            query = paste0(query," OR detectorid = ",dets[k])
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
          raw = join(rawRequest,detectors,by="detectorid")
          raw$period = cut(raw$starttime,breaks = "1 hour")
          if(!is.na(quantile(raw$volume,0.05))){
            if(as.numeric(quantile(raw$volume,0.05,na.rm = TRUE))>0){
              agg = ddply(raw,c("lanenumber","period"),function(X) data.frame(volume=sum(X$volume,na.rm = TRUE), occupancy = mean(X$occupancy,na.rm = TRUE), weighted.mean(X$speed,X$volume,na.rm = TRUE)))
              agg$period = as.POSIXct(agg$period)
              agg$lanenumber = factor(agg$lanenumber) 
              agg$hour = hour(agg$period)
              agg_hour = ddply(agg,c("lanenumber","hour"),function(X) data.frame(volume=mean(X$volume,na.rm = TRUE), occupancy = mean(X$occupancy,na.rm = TRUE), weighted.mean(X$speed,X$volume,na.rm = TRUE)))
            }else{
              agg = ddply(raw,c("lanenumber","period"),function(X) data.frame(volume=sum(X$volume,na.rm = TRUE), occupancy = mean(X$occupancy,na.rm = TRUE), speed = mean(X$speed,na.rm = TRUE)))
              agg$period = as.POSIXct(agg$period)
              agg$lanenumber = factor(agg$lanenumber) 
              agg$hour = hour(agg$period)
              agg_hour = ddply(agg,c("lanenumber","hour"),function(X) data.frame(volume=mean(X$volume,na.rm = TRUE), occupancy = mean(X$occupancy,na.rm = TRUE), speed = mean(X$speed,na.rm = TRUE)))
            }
            
            lowCases = subset(agg_hour,(agg_hour$volume<1 | agg_hour$speed <1 | agg_hour$occupancy <0.01))
            
            if(nrow(lowCases)>0){
              outageReport = list()
              outageReport[["timePeriod"]]=paste0(startTime, " to ",endTime)
              outageReport[["hours"]]= unique(lowCases$hour)
              outageReport[["lanes"]]=as.character(unique(lowCases$lanenumber))
              qi = c(mean(lowCases$volume,na.rm = TRUE)<1,mean(lowCases$speed,na.rm = TRUE)<1, mean(lowCases$occupancy,na.rm = TRUE)<0.01)
              qi[is.na(qi)]=TRUE
              quants = c("volume","speed","occupancy")[qi]
              outageReport[["quants"]]=quants
              
            }else{
              break
            }
          }
          
        }else{
          outageReport = list()
          outageReport[["timePeriod"]]=paste0(startTime, " to ",endTime)
          outageReport[["hours"]]= "ALL"
          outageReport[["lanes"]]="ALL"
          outageReport[["quants"]]="ALL"
        }
        outageReports[[rc]]=outageReport
        print(paste0("Logged outage report for station ", i," of ",nrow(stationFlagged)," with date range of ", paste0(startTime, " to ",endTime)))
        rc = rc+1
      }
    }
    stationReport[["outages"]]=outageReports
    stationFlagReports[[i]]= stationReport
  }
  saveRDS(stationFlagReports,"stationFlagReports.rds")
}

# Implementaion -----------------------------------------------------------
stationCheck = data.frame(matrix(nrow = nrow(stations),ncol=6))
colnames(stationCheck)=c("stationid","volume","speed","occupancy","valueFlag","stationFlag")
stationCheck$stationid = stations$stationid

startTime = Sys.time()
for (i in 1:nrow(stationCheck)){
  station_id = stationCheck$stationid[i]
  result = missingData(station_id,detectors,"2015-05-01","2015-05-30",0.05)
  if (!is.na(result)){
    stationCheck[i,2:4]=result[[1]]
    stationCheck$valueFlag[i]=result[[2]]
    stationCheck$stationFlag[i]=FALSE
  }else{
    stationCheck[i,2:4]=NA
    stationCheck$valueFlag[i]=FALSE
    stationCheck$stationFlag[i]=TRUE
  }
  
  totalTime = as.numeric(difftime(Sys.time(),startTime,units ="mins"))
  print(paste0("Checked station # ",i," of ",nrow(stationCheck),", ",totalTime," minutes so far"))
}

monthRange = timeSequence("2014-01-01","2015-06-01",by="month")


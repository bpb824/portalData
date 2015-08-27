library(RPostgreSQL)
library(DBI)
library(timeSeries)
library(plyr)
library(ggplot2)
library(lubridate)
library(mail)

setwd("~/_ODOT_Portal/investigations")
####Connect to Portal db
####Make sure you VPN into cecs network
db_cred = fromJSON(file="/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/db_credentials.json")
con <- dbConnect(dbDriver("PostgreSQL"), host=db_cred$db_credentials$db_host, port= 5432, user=db_cred$db_credentials$db_user, password = db_cred$db_credentials$db_pass, dbname=db_cred$db_credentials$db_name)



####Relational Tables
stations= dbGetQuery(con,"SELECT * FROM public.stations")
detectors = dbGetQuery(con,"SELECT * FROM public.detectors")

valueTest = function(station_id, detectors, startTime,endTime,threshold,con){
  
  dets = detectors$detectorid[detectors$stationid %in% station_id]
  if(length(dets)>0){
    
    thresh = vector(length=6)
    names(thresh)=c("count","occupancy","highSpeed","lowSpeed","lowMaxOcc","lowAvgOcc")
    
    if(threshold=="high"){
      thresh["count"]=0.002
      thresh["occupancy"]=0.0025
      thresh["highSpeed"]=0.005
      thresh["lowSpeed"]=0.05
      thresh["lowMaxOcc"]=25
      thresh["lowAvgOcc"]=6
    }else if(threshold=="medium"){
      thresh["count"]=0.003
      thresh["occupancy"]=0.005
      thresh["highSpeed"]=0.01
      thresh["lowSpeed"]=0.1
      thresh["lowMaxOcc"]=20
      thresh["lowAvgOcc"]=5
    }else if (threshold=="low"){
      thresh["count"]=0.01
      thresh["occupancy"]=0.05
      thresh["highSpeed"]=0.05
      thresh["lowSpeed"]=0.30
      thresh["lowMaxOcc"]=0
      thresh["lowAvgOcc"]=4
    }else{
      stop("Please select 'high', 'medium', or 'low' as a threshold")
    }
    
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
    
    complete = rawRequest[complete.cases(rawRequest),]
    
    if(nrow(rawRequest)>0 & nrow(complete)>0){
      dets = unique(rawRequest$detectorid)
      detList = list()
      for (i in 1:length(dets)){
        di = dets[i]
        detector=list()
        detector[["detectorid"]]=di
        subRaw = subset(rawRequest,rawRequest$detectorid == di)
        subRaw$dow = weekdays(subRaw$starttime,abbreviate=TRUE)
        subRaw = subset(subRaw,subRaw$dow != "Sat" & subRaw$dow != "Sun")
        subRaw[,c("speed","volume","occupancy")][is.na(subRaw[,c("speed","volume","occupancy")])]=0
        
        #Quality Tests
        flags = rep(FALSE,6)
        names(flags)=names(thresh)
        
        ##Count
        if(as.numeric(quantile(subRaw$volume,(1-thresh["count"]),na.rm=TRUE))>17){
          flags["count"]=TRUE
        }
        
        ##Occupancy
        if(as.numeric(quantile(subRaw$occupancy,(1-thresh["occupancy"]),na.rm=TRUE))>95){
          flags["occupancy"]=TRUE
        }
        
        ##High Speed
        if(as.numeric(quantile(subRaw$speed,(1-thresh["highSpeed"]),na.rm=TRUE))>100){
          flags["highSpeed"]=TRUE
        }
        
        ##Low Speed
        if(as.numeric(quantile(subRaw$speed,(thresh["lowSpeed"]),na.rm=TRUE))<5){
          flags["lowSpeed"]=TRUE
        }
        
        ##Low Max Occupancy
        if(max(subRaw$occupancy,na.rm=TRUE)<thresh["lowMaxOcc"]){
          flags["lowMaxOcc"]=TRUE
        }
        
        ##Low Avg Occupancy
        subRaw$hour = hour(subRaw$starttime)
        peak = subset(subRaw,(subRaw$hour >=7 & subRaw$hour <=9) | (subRaw$hour >=4 & subRaw$hour <=6))
        if(nrow(peak[complete.cases(peak),])>0){
          if(mean(peak$occupancy,na.rm=TRUE)<thresh["lowAvgOcc"]){
            flags["lowAvgOcc"]=TRUE
          }
        }
        
        detector[["flags"]]=flags
        detList[[i]]=detector
      }
      
      #staList = list()
      #staList[["stationid"]]=station_id
      #staList[["timePeriod"]]=paste0(startTime," to ",endTime)
      #staList[["detList"]]=detList
      
      return(detList)
    }else{
      return(NA)
    }
    
  }else{
    return(NA)
  }
  
  
}

valueFlags = list()
monthRange = timeSequence("2014-01-01","2015-06-01",by="month")
programStart = Sys.time()
for (i in 1:nrow(stations)){
  sid = stations$stationid[i]
  station = list()
  station[["stationid"]]=sid
  timeList = list()
  tc = 1
  for (j in length(monthRange):2){
    startTime = monthRange[j-1]
    endTime = monthRange[j]
    detList = valueTest(station_id = sid,detectors = detectors, startTime=startTime,endTime=endTime,threshold="high",con=con)
    time = list()
    time[["period"]]= paste0(startTime," to ",endTime)
    time[["detectors"]]=detList
    timeList[[tc]]= time
    totalTime = as.numeric(difftime(Sys.time(),programStart,units ="mins"))
    print(paste0("Checked time period ", tc," of ",length(monthRange)-1," for station # ",i," of ",nrow(stations),", ",totalTime," minutes so far"))
    tc = tc+1
  }
  station[["times"]]=timeList
  valueFlags[[i]]=station
  totalTime = as.numeric(difftime(Sys.time(),programStart,units ="mins"))
  print(paste0("------Checked station # ",i," of ",nrow(stations),", ",totalTime," minutes so far"))
}

#valueTest(station_id = 1011,detectors = detectors, startTime="2015-05-01",endTime="2015-05-31",threshold="high",con=con)
saveRDS(valueFlags,"valueFlags.rds")

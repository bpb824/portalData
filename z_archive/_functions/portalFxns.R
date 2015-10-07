### PORTAL data query/filtering Functions
publicQuery = function(dets,month){
  tableMonth= gsub("-","_",substr(month,1,7))
  query = paste0("SELECT * FROM public.loopdata_",tableMonth," WHERE (detectorid = ",dets[1])
  if(length(dets)>1){
    for (k in 2:length(dets)){
      query = paste0(query," OR detectorid = ",dets[k])
    }
  }
  query = paste0(query,")")
  return(query)
}

freewayQuery = function(dets,startTime,endTime){
  query = paste0("SELECT * FROM freeway.data WHERE (starttime >='",startTime,"T00:00:00' AND starttime <='",endTime,"T23:59:59') AND (detectorid = ",dets[1])
  if(length(dets)>1){
    for (i in 2:length(dets)){
      query = paste0(query," OR detectorid = ",dets[i])
    }
  }
  query = paste0(query,")")
  return(query) 
}

freewayQuery_time = function(dets,startTime,endTime){
  query = paste0("SELECT * FROM freeway.data WHERE (starttime >='",startTime,"' AND starttime <='",endTime,"') AND (detectorid = ",dets[1])
  if(length(dets)>1){
    for (i in 2:length(dets)){
      query = paste0(query," OR detectorid = ",dets[i])
    }
  }
  query = paste0(query,")")
  return(query) 
}

filter = function(portal){
  dets = unique(portal$detectorid)
  badRows = vector()
  for (i in 1:length(dets)){
    did =dets[i]
    sid = detectors$stationid[detectors$detectorid==did]
    detData = subset(portal,portal$detectorid==did)
    if(sid>=5000){
      bad = c(which(detData$volume>17))
      bad = unique(bad)
      badRows = c(badRows,rownames(detData[bad,]))
    }else{
      bad = c(which(detData$volume>17),
              which(detData$occupancy>95),
              which(detData$speed>100),
              which(detData$speed<5),
              which(detData$speed==0 & detData$volume>0),
              which(detData$speed>0 & detData$volume==0),
              which(detData$occupancy>0 & detData$volume==0))
      bad = unique(bad)
      badRows = c(badRows,rownames(detData[bad,]))
    }
  }
  badRows = unique(badRows)
  goodRows = rownames(portal)[!(rownames(portal) %in% badRows)]
  
  print(paste0(length(badRows)," rows discarded (",round((length(badRows)/nrow(portal))*100,1),"%)"))
  return(portal[goodRows,])
}

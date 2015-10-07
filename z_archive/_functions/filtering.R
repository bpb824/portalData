
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

goodData = function(portal){
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
              which(detData$speed<5 & detData$speed != 0),
              which(detData$speed==0 & detData$volume>0),
              which(detData$speed>0 & detData$volume==0),
              which(detData$occupancy>0 & detData$volume==0))
      bad = unique(bad)
      badRows = c(badRows,rownames(detData[bad,]))
    }
  }
  badRows = unique(badRows)
  goodRows = portal[rownames(portal)[!(rownames(portal) %in% badRows)],]
  incomplete = sum(!complete.cases(goodRows))
  good = 1-length(badRows)/nrow(portal)-incomplete/nrow(portal)
  return(good)
}


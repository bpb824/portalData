### PORTAL data query/filtering Functions
publicQuery = function(dets,startTime,endTime){
  startMonth= gsub("-","_",substr(startTime,1,7))
  endMonth= gsub("-","_",substr(startTime,1,7))
  if(startMonth == endMonth){
    query = paste0("SELECT * FROM public.loopdata_",startMonth," WHERE (starttime >='",startTime,"T00:00:00' AND starttime <='",endTime,"T23:59:59') AND (detectorid = ",dets[1])
    if(length(dets)>1){
      for (k in 2:length(dets)){
        query = paste0(query," OR detectorid = ",dets[k])
      }
    }
    query = paste0(query,")")
    return(query)
  }else{
    stop("Have not developed multiple month public query yet")
  }
  
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

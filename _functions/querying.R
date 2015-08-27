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

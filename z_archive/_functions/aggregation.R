require(plyr)
require(lubridate)

aggTime= function(unAgg, aggVars, timeCut, hod = TRUE){
  unAgg$period = cut(unAgg$starttime,breaks =timeCut)
  agg = ddply(unAgg, c(aggVars,"period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
  agg$period = as.POSIXct(agg$period)
  agg$lanenumber = factor(agg$lanenumber)
  if(hod){
    agg$hod = hour(agg$period)
    agg_hour = ddply(agg,c(aggVars,"hod"),function(X) data.frame(volume=mean(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
    return(agg_hour)
  }else{
    return(agg)
  }
}
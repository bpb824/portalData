require(RPostgreSQL)
require(DBI)
require(timeSeries)
require(plyr)
require(ggplot2)
require(lubridate)
require(rjson)

setwd("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations")

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
source("_functions/portalFxns.R")

sids = sort(unique(stations$stationid))
dataList = list()
rc=1
for (i in 1:length(sids)){
  sid = sids[i]
  dids = detectors$detectorid[detectors$stationid==sid]
  if(length(dids)>0){
    station = list()
    station[["station_id"]]=sid
    raw = dbGetQuery(con,freewayQuery(dids,startTime = "2015-05-01",endTime = "2015-07-31"))
    station[["data"]]=raw
    dataList[[rc]]=station
    print(paste0("Pulled station #",i," of ",length(sids)))
    rc=rc+1
  }else{
    
  }
}
saveRDS(dataList,"_data/freeway/outlierExploration/jja_data.rds")


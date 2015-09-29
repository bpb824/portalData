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
require(leaflet)
require(stargazer)
require(rgeos)
require(rgdal)
require(rjson)
require(reshape2)

#Set working directory
setwd("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations")

#Connect to Portal db, Make sure you VPN into PSU CECS network
db_cred = fromJSON(file="db_credentials.json")
con <- dbConnect(dbDriver("PostgreSQL"), host=db_cred$db_credentials$db_host, port= 5432, user=db_cred$db_credentials$db_user, password = db_cred$db_credentials$db_pass, dbname=db_cred$db_credentials$db_name)

#Relational Tables
stations= dbGetQuery(con,"SELECT * FROM public.stations")
detectors = dbGetQuery(con,"SELECT * FROM public.detectors")

#Load stock functions
source("_functions/querying.R")
source("_functions/aggregation.R")

# functions ---------------------------------------------------------------
missingData = function(station_id,detectors,startTime,endTime,percentile){
  dets = detectors$detectorid[detectors$stationid %in% station_id]
  lanes = sort(unique(detectors$lanenumber[detectors$detectorid %in% dets]))
  lanes = lanes[lanes!=0]
  laneList = list()
  if(length(dets)>0){
    query = freewayQuery(dets,startTime,endTime)
    rawRequest = dbGetQuery(con,query)
    if (nrow(rawRequest)==0){
      if(month(startTime)<=6){
        query = publicQuery(dets,startTime,endTime)
        rawRequest = dbGetQuery(con,query)
      }
    }
    if (nrow(rawRequest)>0){
      rawLane = join(rawRequest,detectors,by="detectorid")
      for (i in 1:length(lanes)){
        lane = lanes[i]
        sub = subset(rawLane,rawLane$lanenumber==lane)
        complete = sub[complete.cases(sub[,colnames(rawRequest)]),]
        if(nrow(complete)>0){
          agg = aggTime(complete,"lanenumber","hour",TRUE)
          volume = as.numeric(quantile(agg$volume,percentile, na.rm = TRUE))
          speed = as.numeric(quantile(agg$speed,percentile, na.rm = TRUE))
          occupancy = as.numeric(quantile(agg$occupancy,percentile, na.rm = TRUE))
          if (volume <= 1 | speed <=1 | occupancy <=0.01){
            #Flag 1 means data were very low (probably zero) values
            flag = 1
          }else{
            #Flag 0 means no flag
            flag = 0
          }
        }else{
          #Flag 2 means query turned up zero rows of complete data (only missing data rows)
          flag = 2
        }
        laneList[[lane]]=flag
      }
      result = laneList
    }else{
      #Flag 3 means query turned up zero rows of data
      for (i in 1:length(lanes)){
        lane = lanes[i]
        laneList[[lane]]=3
      }
      result = laneList
    }
  }else{
    #Flag 4 means no detectors could be found for this station id
    result = 4
  }
  return(result)
}


# Exploration -------------------------------------------------------------
oregonStationGeom = readOGR("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/_data/GIS/","oregonStations")
orStationData = oregonStationGeom@data
currentStations = subset(orStationData,is.na(orStationData$end_date))
stationVec = sort(unique(currentStations$stationid))

timeRange = timeSequence("2015-01-01","2015-07-31",by="day")

#stationList = list()
stationList=readRDS("_data/freeway/dataOutages/stationList.rds")
loopStart = Sys.time()

for (i in (length(stationList)+1):length(stationVec)){
  sid = stationVec[i]
  timeList = list()
  for (j in 1:length(timeRange)){
    time = as.character(timeRange[j])
    result = missingData(station_id = sid, detectors = detectors, startTime = time,endTime = time,percentile = 0.05)
    timeList[[time]]=result
    print(paste0("Results for time ", j, " of ", length(timeRange), " for detector ", i, " of ",length(stationVec)))
  }
  totalTime = as.numeric(difftime(Sys.time(),loopStart,units ="mins"))
  print(paste0("----Finished station ", i, " of ", length(stationVec),", ",totalTime," minutes so far"))
  stationList[[as.character(sid)]]=timeList
  saveRDS(stationList,"_data/freeway/dataOutages/stationList.rds")
}




# Mapping -----------------------------------------------------------------
stationList = readRDS("_data/freeway/dataOutages/stationList.rds")
staLocs = read.csv("_data/GIS/orStationsLoc.csv")
mapFrame = data.frame(matrix(nrow=length(stationList),ncol = 5))
colnames(mapFrame)=c("station_id","long","lat","htmlLink","numOutages")
for(i in 1:length(stationList)){
  sid = as.numeric(names(stationList[i]))
  mapFrame$station_id[i]=sid

  if(sid <5000){
    mapFrame$lat[i] = staLocs$Y[staLocs$stationid==sid]
    mapFrame$long[i] = staLocs$X[staLocs$stationid==sid]
  }else{
    mapFrame$lat[i] = staLocs$Y[staLocs$stationid==sid-4000]
    mapFrame$long[i] = staLocs$X[staLocs$stationid==sid-4000]
  }

  sta = stationList[[i]]
  numLanes = length(unique(detectors$lanenumber[detectors$stationid==sid]))
  if(numLanes >0){
    plotFrame = data.frame(matrix(nrow = length(sta),ncol = numLanes+1))
    laneNames = paste0(rep("lane",length(numLanes)),seq(1,numLanes,1))
    colnames(plotFrame)= c("day",laneNames)
    for (j in 1:length(sta)){
      date = names(sta[j])
      if(is.list(sta[[j]])){
        row = unlist(sta[[j]])
      }else{
        row = rep(sta[[j]],numLanes)
      }
      plotFrame$day[j]=date
      plotFrame[j,2:ncol(plotFrame)]=row
    }
    melted = melt(plotFrame,id.vars = c("day"))
    melted$day = as.Date(melted$day)
    flagged = subset(melted,melted$value!=0)
    mapFrame$numOutages[i]=nrow(flagged)



    portalLink=paste0("http://portal.its.pdx.edu/Portal/index.php/stations/view/id/",sid,"/")
    if(sid>=5000){
      mapFrame$htmlLink[i] = paste0("<div align='center'><b>Ramp<b><br><br/><a href='",portalLink,"' target=_blank>",stations$locationtext[stations$stationid==sid][1],"</a></div>")
    }else{
      mapFrame$htmlLink[i] = paste0("<div align='center'><b>Mainline<b><br><br/><a href='",portalLink,"' target=_blank>",stations$locationtext[stations$stationid==sid][1],"</a></div>")
    }
  }
}

mapFrame = mapFrame[complete.cases(mapFrame),]

saveRDS(mapFrame, file = "_data/freeway/dataOutages/mapFrame.rds")

pal <- colorNumeric(
  palette = colorRampPalette(c("red","yellow","green")),
  domain = c(0,100)
)

map= leaflet(mapFrame) %>% addProviderTiles("CartoDB.DarkMatter",options = providerTileOptions(noWrap = TRUE)) %>%
  setView(-122.6662589, 45.5317385, zoom = 10) %>% addCircleMarkers(lng=~long,lat=~lat,popup =~htmlLink,color =~pal(numOutages))



# Plotting ----------------------------------------------------------------
flagKey = data.frame(matrix(nrow=4,ncol=2))
colnames(flagKey)=c("value","def")
flagKey$value=1:4
flagKey$def = c("Very Low Values","Only Empty Data", "Zero Rows of Data", "Detector Unavailable")
stationList = readRDS("_data/freeway/dataOutages/stationList.rds")
plotList = list()
for(i in 1:length(stationList)){
  sid = as.numeric(names(stationList[i]))
  sta = stationList[[i]]
  numLanes = length(unique(detectors$lanenumber[detectors$stationid==sid]))
  if (numLanes>0){
    plotFrame = data.frame(matrix(nrow = length(sta),ncol = numLanes+1))
    laneNames = paste0(rep("Lane ",length(numLanes)),seq(1,numLanes,1))
    colnames(plotFrame)= c("day",laneNames)
    for (j in 1:length(sta)){
      date = names(sta[j])
      row = unlist(sta[[j]])
      plotFrame$day[j]=date
      plotFrame[j,2:ncol(plotFrame)]=row
    }
    melted = melt(plotFrame,id.vars = c("day"))
    melted$day = as.Date(melted$day)
    flagged = subset(melted,melted$value!=0)
    flagged = join(flagged,flagKey,by="value")
    flagged$value=factor(flagged$value)
    flagged$id = rownames(flagged)
    plotList[[as.character(sid)]]=flagged
  }

  #ggplot(flagged,aes(x = day, y = variable, group = value, colour=value))+geom_point()+scale_x_date()+scale_color_discrete(labels=flagKey$def)
  #flagged %>% ggvis(x=~day, y=~ variable, stroke =~value, fill =~value, key:=~id)  %>% layer_points(opacity := 0.5) %>% add_tooltip(tFunk)
}



saveRDS(plotList,"_data/freeway/dataOutages/plotList.rds")


# Table -------------------------------------------------------------------
flagKey = data.frame(matrix(nrow=4,ncol=2))
colnames(flagKey)=c("value","def")
flagKey$value=1:4
flagKey$def = c("Very Low Values","Only Empty Data", "Zero Rows of Data", "Detector Unavailable")
stationList = readRDS("_data/freeway/dataOutages/stationList.rds")

tabFrame = data.frame(matrix(nrow=length(stationList),ncol=6))
colnames(tabFrame)=c("sid","location","flag1","flag2","flag3","numOutages")
#colnames(tabFrame)=c("station_id",c("# Days w/ Very Low Values","# Days w/ Only Empty Data", "# Days w/ Zero Rows of Data"),"# Days w/ Outages (Total)")

for(i in 1:length(stationList)){
  sid = as.numeric(names(stationList[i]))
  tabFrame$sid[i]=sid
  tabFrame$location[i] = stations$locationtext[stations$stationid==sid][1]
  sta = stationList[[i]]
  numLanes = length(unique(detectors$lanenumber[detectors$stationid==sid]))
  if (numLanes>0){
    flag1 = 0
    flag2 = 0
    flag3 = 0
    for (j in 1:length(sta)){
      flags = unlist(sta[[j]])
      for (k in 1:length(flags)){
        if(flags[k]==1){
          flag1 = flag1 +1
        }else if(flags[k]==2){
          flag2 = flag2 +1
        }else if(flags[k]==3){
          flag3 = flag3 +1
        }
      }
    }
    tabFrame$flag1[i]=flag1
    tabFrame$flag2[i]=flag2
    tabFrame$flag3[i]=flag3
    tabFrame$numOutages[i]=flag1+flag2+flag3
  }
}

tabFrame = tabFrame[complete.cases(tabFrame),]

saveRDS(tabFrame,"_data/freeway/dataOutages/tabFrame.rds")



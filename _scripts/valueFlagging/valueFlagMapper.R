library(RPostgreSQL)
library(DBI)
library(timeSeries)
library(plyr)
library(ggplot2)
library(lubridate)
library(leaflet)
library(stargazer)
library(rgeos)
library(rgdal)
library(htmlwidgets)

sysName = Sys.info()["sysname"]
if (sysName == "Linux"){
  setwd(setwd("~/_ODOT_Portal/investigations"))
}else{
  setwd("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/valueFlagging/")
}

valueFlags = readRDS("valueFlags.rds")
####Connect to Portal db
####Make sure you VPN into cecs network
db_cred = fromJSON(file="/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/db_credentials.json")
con <- dbConnect(dbDriver("PostgreSQL"), host=db_cred$db_credentials$db_host, port= 5432, user=db_cred$db_credentials$db_user, password = db_cred$db_credentials$db_pass, dbname=db_cred$db_credentials$db_name)



####Relational Tables
stations= dbGetQuery(con,"SELECT * FROM public.stations")
detectors = dbGetQuery(con,"SELECT * FROM public.detectors")
stationsShp = readOGR(dsn="/Users/bblanc/OneDrive/_ODOT/_Portal/_gisData",layer ="stations")
stationsLoc=stationsShp@data

plotReport = function(stationReport){
  sid = stationReport$stationid
  subDetectors = subset(detectors,detectors$stationid==sid)
  if(nrow(subDetectors)>0){
    if(!dir.exists(as.character(sid))){
      dir.create(as.character(sid))
    }
    file.copy(from="stationReport.rmd",to=paste0(sid,"/stationReport.rmd"),overwrite = TRUE)
    saveRDS(stationReport,file = paste0(sid,"/data.rds"))
    write.csv(detectors,paste0(sid,"/detectors.csv"))
    stationName = stations$locationtext[stations$stationid==sid]
    timeList = stationReport$times
    lanes = unique(detectors$lanenumber[detectors$stationid==sid])
    flags = c("count","occupancy","highSpeed","lowSpeed","lowMaxOcc","lowAvgOcc")
    timeLimits = c("2014-01-01","2015-06-01")
    times = timeSequence(from=timeLimits[1],to =timeLimits[2],by="month")
    plotFrame = data.frame(matrix(nrow =(length(times)-1)*length(lanes)*length(flags),ncol=5))
    colnames(plotFrame)=c("startTime","endTime","lanenumber","flagType","flagged")
    plotFrame$startTime=as.character(times[1:(length(times)-1)])
    plotFrame$endTime=as.character(times[2:length(times)])
    laneVector = vector()
    flagVector = vector()
    for (k in 1:length(lanes)){
      laneVector = c(laneVector,rep(lanes[k],length(times)-1))
    }
    laneVector = rep(laneVector,length(flags))
    for (k in 1:length(flags)){
      flagVector= c(flagVector,rep(flags[k],(length(times)-1)*length(lanes)))
    }
    plotFrame$lanenumber=laneVector
    plotFrame$flagType=flagVector
    plotFrame$flagged=FALSE
    for (j in 1:length(timeList)){
      time = timeList[[j]]
      flagStart = substr(time$period,1,10)
      flagEnd = substr(time$period,15,24)
      detList = time$detectors
      if(!is.na(detList)){
        for (k in 1:length(detList)){
          laneNum = detectors$lanenumber[detectors$detectorid==detList[[k]]$detectorid]
          flagList = detList[[k]]$flags
          flagged = names(flagList[flagList==TRUE])
          plotFrame$flagged[plotFrame$startTime==flagStart &
                              plotFrame$endTime == flagEnd &
                              plotFrame$lanenumber == laneNum &
                              plotFrame$flagType %in% flagged] = TRUE
        }
      }
    }
    plotFrame$startTime=as.Date(plotFrame$startTime)
    plotFrame$endTime=as.Date(plotFrame$endTime)
    plotFrame$lanenumber=factor(plotFrame$lanenumber)
    flagFrame = plotFrame
    flagFrame[flagFrame$flagged==FALSE,c("startTime","endTime")]=NA
    theme_set(theme_grey(base_size = 50))
    png(paste0(sid,"/plot.png"), width=1600,height=1000)
    print(ggplot(flagFrame, aes(x=flagType,group = lanenumber),na.rm=TRUE)+geom_linerange(stat= "identity",aes(ymin=startTime,ymax=endTime,colour=lanenumber),size = 10,position=position_dodge(width=c(0.5,0.5)))+coord_flip()+scale_y_date(limits = as.Date(timeLimits))+xlab("Flag Type")+ylab("Time Period")+scale_colour_discrete(name="Lane Number")+ggtitle(paste0("Station ID #",sid," (",stationName,")")))
    dev.off()
    rmarkdown::render(paste0(sid,"/stationReport.rmd"),"html_document")
  }
  
}

flagged = data.frame(matrix(nrow = length(valueFlags),ncol=4))
colnames(flagged)=c("station_id","lat","lng","html")

for (i in 1:length(valueFlags)){
  stationReport = valueFlags[[i]]
  plotReport(stationReport)
  sid = stationReport$stationid
  if(nrow(subset(detectors,detectors$stationid==sid))>0){
    flagged$station_id[i]=sid
    if(sid %in% stationsLoc$stationid){
      if(sid >= 5000){
        lat = stationsLoc$lat[stationsLoc$stationid==sid-4000]
        lng = stationsLoc$lng[stationsLoc$stationid==sid-4000]
        if(length(lat)>0 & length(lng)>0){
          flagged$lat[i]=lat
          flagged$lng[i]=lng
        }else{
          flagged$lat[i]=NA
          flagged$lng[i]=NA
        }
        
      }else if(sid ==1089){
        flagged$lat[i]=45.506521
        flagged$lng[i]=-122.770808
      }else{
        lat = stationsLoc$lat[stationsLoc$stationid==sid]
        lng = stationsLoc$lng[stationsLoc$stationid==sid]
        if(length(lat)>0 & length(lng)>0){
          flagged$lat[i]=lat
          flagged$lng[i]=lng
        }else{
          flagged$lat[i]=NA
          flagged$lng[i]=NA
        }
      }
    }
    
    plot_url = paste0("http://bigtransportdata.com/_portal/valueFlagging/",sid,"/plot.png")
    report_url = paste0("http://bigtransportdata.com/_portal/valueFlagging/",sid,"/stationReport.html")
    flagged$html[i] = paste0("<div align='center'><a href='",report_url,"' target='_blank'><img src='",plot_url,"' alt='VolumePlot' height='200' width='300'></a></div>")
  }
}

flagged = flagged[complete.cases(flagged),]
redFlag = makeIcon(iconUrl = "http://bigtransportdata.com/_portal/laneNumbers/redFlag.png",iconWidth = 37,iconHeight = 41,iconAnchorX =2 ,iconAnchorY = 40)
map= leaflet() %>% addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>% setView(-122.6662589, 45.5317385, zoom = 10)
map =addMarkers(map=map,icon=redFlag,lng =flagged$lng,lat=flagged$lat,popup = flagged$html)
saveWidget(map, "valueFlagMap.html")



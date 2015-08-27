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

sysName = Sys.info()["sysname"]
if (sysName == "Linux"){
  setwd(setwd("~/_ODOT_Portal/investigations"))
}else{
  setwd("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/")
}

#valueFlagReports = readRDS("valueFlagReports.rds")
#stationFlagReports = readRDS("stationFlagReports.rds")
outageFrame = readRDS("newOutageFlags.rds")

lowVals = outageFrame$stationid[outageFrame$flag==1]
missingVals = outageFrame$stationid[outageFrame$flag==2]
noData = outageFrame$stationid[outageFrame$flag==3]
nullDetector = outageFrame$stationid[outageFrame$flag==4]
good = outageFrame$stationid[outageFrame$flag==0]
bad = outageFrame[outageFrame$flag!=0,]
bad = bad[!(duplicated(bad$stationid)),]


####Connect to Portal db
####Make sure you VPN into cecs network
db_cred = fromJSON(file="/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/db_credentials.json")
con <- dbConnect(dbDriver("PostgreSQL"), host=db_cred$db_credentials$db_host, port= 5432, user=db_cred$db_credentials$db_user, password = db_cred$db_credentials$db_pass, dbname=db_cred$db_credentials$db_name)



####Relational Tables
stations= dbGetQuery(con,"SELECT * FROM public.stations")
detectors = dbGetQuery(con,"SELECT * FROM public.detectors")


# 
# plotOutages = function(reportList){
#   for (i in 1:length(reportList)){
#     stationReport = reportList[[i]]
#     sid = stationReport$Station_ID
#     if(!is.na(sid)){
#       stationName = stations$locationtext[stations$stationid==sid]
#       outages = stationReport$outages
#       subDetectors = subset(detectors,detectors$stationid==sid)
#       lanes = unique(detectors$lanenumber[detectors$stationid==sid])
#       quants = c("speed","volume","occupancy")
#       timeLimits = c("2014-01-01","2015-06-01")
#       times = timeSequence(from=timeLimits[1],to =timeLimits[2],by="month")
#       plotFrame = data.frame(matrix(nrow =(length(times)-1)*length(lanes)*length(quants),ncol=5))
#       colnames(plotFrame)=c("startTime","endTime","lanenumber","quant","outage")
#       plotFrame$startTime=as.character(times[1:(length(times)-1)])
#       plotFrame$endTime=as.character(times[2:length(times)])
#       laneVector = vector()
#       quantVector = vector()
#       for (k in 1:length(lanes)){
#         laneVector = c(laneVector,rep(lanes[k],length(times)-1))
#       }
#       laneVector = rep(laneVector,length(quants))
#       for (k in 1:length(quants)){
#         quantVector= c(quantVector,rep(quants[k],(length(times)-1)*length(lanes)))
#       }
#       plotFrame$lanenumber=laneVector
#       plotFrame$quant=quantVector
#       plotFrame$outage=FALSE
#       for (j in 1:length(outages)){
#         outage = outages[[j]]
#         outStart = substr(outage$timePeriod,1,10)
#         outEnd = substr(outage$timePeriod,15,24)
#         outLanes = as.numeric(outage$lanes)
#         outQuants = outage$quants
#         plotFrame$outage[plotFrame$startTime==outStart &
#                            plotFrame$endTime == outEnd &
#                            plotFrame$lanenumber %in% outLanes &
#                            plotFrame$quant %in% quants] = TRUE
#       }
#       plotFrame$startTime=as.Date(plotFrame$startTime)
#       plotFrame$endTime=as.Date(plotFrame$endTime)
#       plotFrame$timeDiff = difftime(plotFrame$endTime,plotFrame$startTime)
#       plotFrame$lanenumber=factor(plotFrame$lanenumber)
#       outageFrame = subset(plotFrame,plotFrame$outage==TRUE)
#       theme_set(theme_grey(base_size = 25))
#       png(paste0(sid,"/plot.png"), width=1600,height=1000)
#       print(ggplot(outageFrame, aes(x=quant,group = lanenumber))+geom_linerange(stat= "identity",aes(ymin=startTime,ymax=endTime,colour=lanenumber),size = 10,position=position_dodge(width=c(0.5,0.5)))+coord_flip()+scale_y_date(limits = as.Date(timeLimits))+xlab("Quantity")+ylab("Time Period")+scale_colour_discrete(name="Lane Number")+ggtitle(paste0("Station ID #",sid," (",stationName,")")))
#       dev.off()
#     }
#   }
# }

# sids = vector()
# for(i in 1:length(valueFlagReports)){
#   sids[i]=valueFlagReports[[i]]$Station_ID
# }
# 
# plotReport = function(stationReport){
#   sid = stationReport$Station_ID
#   if(!dir.exists(as.character(sid))){
#     dir.create(as.character(sid))
#   }
#   file.copy(from="stationReport.rmd",to=paste0(sid,"/stationReport.rmd"),overwrite = TRUE)
#   saveRDS(stationReport,file = paste0(sid,"/data.rds"))
#   stationName = stations$locationtext[stations$stationid==sid]
#   outages = stationReport$outages
#   subDetectors = subset(detectors,detectors$stationid==sid)
#   lanes = unique(detectors$lanenumber[detectors$stationid==sid])
#   quants = c("speed","volume","occupancy")
#   timeLimits = c("2014-01-01","2015-06-01")
#   times = timeSequence(from=timeLimits[1],to =timeLimits[2],by="month")
#   plotFrame = data.frame(matrix(nrow =(length(times)-1)*length(lanes)*length(quants),ncol=5))
#   colnames(plotFrame)=c("startTime","endTime","lanenumber","quant","outage")
#   plotFrame$startTime=as.character(times[1:(length(times)-1)])
#   plotFrame$endTime=as.character(times[2:length(times)])
#   laneVector = vector()
#   quantVector = vector()
#   for (k in 1:length(lanes)){
#     laneVector = c(laneVector,rep(lanes[k],length(times)-1))
#   }
#   laneVector = rep(laneVector,length(quants))
#   for (k in 1:length(quants)){
#     quantVector= c(quantVector,rep(quants[k],(length(times)-1)*length(lanes)))
#   }
#   plotFrame$lanenumber=laneVector
#   plotFrame$quant=quantVector
#   plotFrame$outage=FALSE
#   for (j in 1:length(outages)){
#     outage = outages[[j]]
#     outStart = substr(outage$timePeriod,1,10)
#     outEnd = substr(outage$timePeriod,15,24)
#     if("ALL" %in% outage$lanes){
#       outLanes = lanes
#     }else{
#       outLanes = as.numeric(outage$lanes)
#     }
#     if( "ALL" %in% outage$quants){
#       outQuants = quants
#     }else{
#       outQuants = outage$quants
#     }
#     plotFrame$outage[plotFrame$startTime==outStart &
#                        plotFrame$endTime == outEnd &
#                        plotFrame$lanenumber %in% outLanes &
#                        plotFrame$quant %in% outQuants] = TRUE
#   }
#   plotFrame$startTime=as.Date(plotFrame$startTime)
#   plotFrame$endTime=as.Date(plotFrame$endTime)
#   #plotFrame$timeDiff = difftime(plotFrame$endTime,plotFrame$startTime)
#   plotFrame$lanenumber=factor(plotFrame$lanenumber)
#   outageFrame = plotFrame
#   outageFrame[outageFrame$outage==FALSE,c("startTime","endTime")]=NA
#   theme_set(theme_grey(base_size = 50))
#   png(paste0(sid,"/plot.png"), width=1600,height=1000)
#   print(ggplot(outageFrame, aes(x=quant,group = lanenumber),na.rm=TRUE)+geom_linerange(stat= "identity",aes(ymin=startTime,ymax=endTime,colour=lanenumber),size = 10,position=position_dodge(width=c(0.5,0.5)))+coord_flip()+scale_y_date(limits = as.Date(timeLimits))+xlab("Quantity")+ylab("Time Period")+scale_colour_discrete(name="Lane Number")+ggtitle(paste0("Station ID #",sid," (",stationName,")")))
#   dev.off()
#   rmarkdown::render(paste0(sid,"/stationReport.rmd"),"html_document")
# }

#plotOutages(stationFlagReports,"stationFlagPlots")



stationsShp = readOGR(dsn="/Users/bblanc/OneDrive/_ODOT/_Portal/_gisData",layer ="stations")
stationsLoc=stationsShp@data

stationsLoc = stationsLoc[!duplicated(stationsLoc$stationid),]

bad$color=NA
colors=brewer.pal(4,"Set1")
bad$color =colors[bad$flag]
flagTypes = c("Suspiciously Low Values","Queried Rows Only Have Missing Data","Query Resulted in Zero Rows","No Detectors found for this Station ID")

map= leaflet() %>% addProviderTiles("Stamen.TonerLite",
                                    options = providerTileOptions(noWrap = TRUE)
) %>% setView(-122.6662589, 45.5317385, zoom = 10)

mapFrame = data.frame(matrix(nrow = nrow(bad),ncol =6))
colnames(mapFrame)=c("sid","lat","lng","portalLink","html","color")

for (i in 1:nrow(bad)){
  sid = bad$stationid[i]
  if(!is.na(sid) & sid<6000){
    mapFrame$sid[i]=sid
    if(sid >= 5000){
      if(length(stationsLoc$lat[stationsLoc$stationid==sid-4000]==1)){
        mapFrame$lat[i]=stationsLoc$lat[stationsLoc$stationid==sid-4000]
      }
      if(length(stationsLoc$lng[stationsLoc$stationid==sid-4000])==1){
        mapFrame$lng[i]=stationsLoc$lng[stationsLoc$stationid==sid-4000]
      }
    }else{
      if(length(stationsLoc$lat[stationsLoc$stationid==sid]==1)){
        mapFrame$lat[i]=stationsLoc$lat[stationsLoc$stationid==sid]
      }
      if(length(stationsLoc$lng[stationsLoc$stationid==sid])==1){
        mapFrame$lng[i]=stationsLoc$lng[stationsLoc$stationid==sid]
      }
    }
    mapFrame$color[i]= bad$color[i]
    if(sid>=5000){
      pid = sid-4000
    }else{
      pid = sid
    }
    mapFrame$portalLink[i]=paste0("http://portal.its.pdx.edu/Portal/index.php/stations/view/id/",pid,"/")
    if(sid>=5000){
      mapFrame$html[i] = paste0("<div align='center'><b>Ramp<b><br><br/><a href='",mapFrame$portalLink[i],"' target=_blank>",stations$locationtext[stations$stationid==sid][1],"</a></div>")
    }else{
      mapFrame$html[i] = paste0("<div align='center'><b>Mainline<b><br><br/><a href='",mapFrame$portalLink[i],"' target=_blank>",stations$locationtext[stations$stationid==sid][1],"</a></div>")
    }
    
  }
}

mapFrame = mapFrame[complete.cases(mapFrame),]



map =addCircleMarkers(map=map,lng =mapFrame$lng,lat=mapFrame$lat,popup = mapFrame$html,color = mapFrame$color)
map =addLegend(map=map,position ="bottomright",colors = colors,values = 1:4,labels=flagTypes)
saveWidget(map, "speedFlagMap.html")
# 
# mapReports = function(reportList, stationsLoc,parentDir){
#   map= leaflet() %>% addProviderTiles("Stamen.TonerLite",
#                                       options = providerTileOptions(noWrap = TRUE)
#   ) %>% setView(-122.6662589, 45.5317385, zoom = 10)
#   
#   mapFrame = data.frame(matrix(nrow = length(reportList),ncol =6))
#   colnames(mapFrame)=c("sid","lat","lng","plotLink","reportLink","html")
#   
#   for (i in 1:length(reportList)){
#     stationReport = reportList[[i]]
#     sid = stationReport$Station_ID
#     if(!is.na(sid) & length(stationReport$outages)>0 & sid<6000){
#       plotReport(stationReport)
#       mapFrame$sid[i]=sid
#       if(sid >= 5000){
#         mapFrame$lat[i]=stationsLoc$lat[stationsLoc$stationid==sid-4000]
#         mapFrame$lng[i]=stationsLoc$lng[stationsLoc$stationid==sid-4000]
#       }else{
#         mapFrame$lat[i]=stationsLoc$lat[stationsLoc$stationid==sid]
#         mapFrame$lng[i]=stationsLoc$lng[stationsLoc$stationid==sid]
#       }
#       
#       mapFrame$plotLink[i]=paste0("http://bigtransportdata.com/",parentDir,"/",sid,"/plot.png")
#       mapFrame$reportLink[i]=paste0("http://bigtransportdata.com/",parentDir,"/",sid,"/stationReport.html")
#       mapFrame$html[i] = paste0("<div align='center'><a href='",mapFrame$reportLink[i],"'><img src='",mapFrame$plotLink[i],"' alt='VolumePlot' height='200' width='300'></a></div>")
#     }
#   }
#   
#   mapFrame = mapFrame[complete.cases(mapFrame),]
#   
#   map =addMarkers(map=map,lng =mapFrame$lng,lat=mapFrame$lat,popup = mapFrame$html)
#   saveWidget(map, "stationMap.html")
# }
# 
# setwd("/Users/bblanc/OneDrive/_Portal_ODOT/investigations/missingData/")
# mapReports(valueFlagReports,stationsLoc,parentDir = "missingData")
# setwd("/Users/bblanc/OneDrive/_Portal_ODOT/investigations/missingData_2/")
# mapReports(stationFlagReports,stationsLoc,parentDir ="missingData_2")


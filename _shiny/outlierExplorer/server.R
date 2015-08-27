library(shiny)
library(ggvis)
library(knitr)
library(stats)
library(DT)

shinyServer(function(input, output, session) {
  img.width <- 700
  img.height <- 400
  options(RCHART_HEIGHT = img.height, RCHART_WIDTH = img.width)
  opts_chunk$set(fig.width=7, fig.height=4)
  
  unfiltered = readRDS("juneAggData_raw.rds")
  filtered = readRDS("juneAggData.rds")
  data = filtered
  stationMeta = readRDS("stations.rds")
  lane = "all"
  quant = "speed"
  hour = 8
  weekdays = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
  
  observeEvent(input$quant,({
    quant = tolower(input$quant)
    if(quant=="speed"){
      binRange = c(1,10)
      binInc = 1
      histStart = 5
      binLabel = "Bin Width (mph)"
    }else if(quant == "volume"){
      binRange = c(100,1000)
      binInc = 100
      histStart = 500
      binLabel = "Bin Width (vph)"
    }else if (quant=="occupancy"){
      binRange = c(1,10)
      binInc = 1
      histStart = 5
      binLabel = "Bin Width (% of time occupied)"
    }
    updateSliderInput(session = session, inputId = "hist",label =binLabel,value = histStart,min = binRange[1],max=binRange[2],step=binInc)
  }))
  
  plotFrame = reactive({
    laneSelect = input$lane
    if(laneSelect=="All (Aggregated)"){
      lane <<- "all"
    }else{
      lane <<- laneSelect
    }
    # print(lane)
    
    if(input$filter=="Unfiltered"){
      data = unfiltered
    }else{
      data = filtered
    }
    
    hour <<-input$hour
    weekdays <<- input$weekdays
    
    quant <<- tolower(input$quant)
    #print(quant)
    #print(weekdays)
    #print(hour)
    #print(lane)
    
    pf = data.frame(matrix(nrow = length(data),ncol = 3))
    colnames(pf)=c("station_id","quantity","out")
    if(lane=="all"){
      for (i in 1:length(data)){
        if(!is.null(data[[i]])){
          sid = data[[i]]$station_id
          pf$station_id[i]=sid
          sData = data[[i]]$data
          if(is.data.frame(sData)){
            rows = sData$hour==hour & sData$dow %in% weekdays
            if(sum(rows>0)){
              sub = sData[rows,]
              if(quant == "volume"){
                q = sum(sub$volume)
              }else if (quant =="occupancy"){
                q = mean(sub$occupancy)
              }else if (quant == "speed"){
                q = weighted.mean(sub$speed,sub$volume)
              }
              pf$quantity[i]=q
            }else{
              pf$quantity[i]=NA
            }
          }else{
            pf$quantity[i]=NA
          }
        }else{
          pf$station_id[i]=NA
          pf$quantity[i]=NA
        }
      }
    }
#     
#     else if(lane=="On-Ramp"){
#       for (i in 1:length(data)){
#         if(!is.null(data[[i]])){
#           sid = data[[i]]$station_id
#           pf$station_id[i]=sid
#           if(sid>=5000){
#             sData = data[[i]]$data
#             if(is.data.frame(sData)){
#               rows = sData$hour==hour & sData$dow %in% weekdays
#               if(sum(rows)>0){
#                 sub = sData[rows,]
#                   if(quant == "volume"){
#                     q = sub$volume
#                   }else if (quant =="occupancy"){
#                     q = sub$occupancy
#                   }else if (quant == "speed"){
#                     q = sub$speed
#                   }
#                   pf$quantity[i]=q
#               }else{
#                 pf$quantity[i]=NA
#               }
#             }else{
#               pf$quantity[i]=NA
#             }
#           }else{
#             pf$quantity[i]=NA
#           }
#         }else{
#           pf$station_id[i]=NA
#           pf$quantity[i]=NA
#         }
#       }
#     }
    else{
      for (i in 1:length(data)){
        if(!is.null(data[[i]])){
          sid = data[[i]]$station_id
          pf$station_id[i]=sid
          sData = data[[i]]$data
          if(is.data.frame(sData)){
            rows = sData$hour==hour & sData$dow %in% weekdays & as.character(sData$lanenumber)==lane
            if(sum(rows)>1){
              sub = sData[rows,]
                if(quant == "volume"){
                  q = mean(sub$volume)
                }else if (quant =="occupancy"){
                  q = mean(sub$occupancy)
                }else if (quant == "speed"){
                  q = mean(sub$speed)
                }
                pf$quantity[i]=q
            }else{
              pf$quantity[i]=NA
            }
          }else{
            pf$quantity[i]=NA
          }
        }else{
          pf$station_id[i]=NA
          pf$quantity[i]=NA
        }
        
      }
    }
    pf$out = NA
    comp = pf[complete.cases(pf[,1:2]),]
    
    if(nrow(comp)>0){
      quantiles = quantile(comp$quantity,c(0.25,0.75),na.rm=TRUE)
      iqr = as.numeric(quantiles[2]-quantiles[1])
      fences <<- as.numeric(c(quantiles[1]-3*iqr,quantiles[1]-1.5*iqr,quantiles[2]+1.5*iqr,quantiles[2]+3*iqr))
      comp$out = "OK"
      comp$out[(comp$quantity <fences[2] & comp$quantity >= fences[1])|(comp$quantity <=fences[4] & comp$quantity > fences[3])]="Minor"
      comp$out[(comp$quantity <fences[1])|(comp$quantity >fences[4])]="Major"
      comp$out=factor(comp$out,ordered = TRUE)
    }
    
    return(comp)
  })
  
  plt = reactive({
    quant = input$quant
    
    #print(quant)
    
    if(quant=="Speed"){
      xAxis = "Speed (mph)"
      
    }else if(quant == "Volume"){
      xAxis = "Volume (vph)"
    }else if (quant=="Occupancy"){
      xAxis = "Occupancy (% of time)"
    }
    
    h <- plotFrame %>% ggvis(x = ~quantity,fill= ~out) %>% group_by(out) %>%
      #layer_histograms(width=input$hist) %>%
      scale_nominal("fill", label = "Outlier Status",
                    domain = c("OK", "Minor", "Major"),
                    range = c("green", "yellow", "red")) %>%
      set_options(width = img.width, height = img.height) %>% 
      add_axis("x", title = xAxis)%>%
      add_axis("y", title = "Frequency") 
    
    return(h)
  })
  
  densityPlot = reactive({
    quant = input$quant
    
    #print(quant)
    
    if(quant=="Speed"){
      xAxis = "Speed (mph)"
      
    }else if(quant == "Volume"){
      xAxis = "Volume (vph)"
    }else if (quant=="Occupancy"){
      xAxis = "Occupancy (% of time)"
    }
    
    h <- plotFrame %>% ggvis(x = ~quantity) %>%
      set_options(width = img.width, height = img.height) %>% 
      add_axis("x", title = xAxis)%>%
      add_axis("y", title = "Probability Density") %>%
      layer_densities(
        adjust = input$bandwidth,kernel="gaussian")
    
    return(h)
  })
  
  plt %>% bind_shiny("ggvis", "ggvis_ui")
  
  densityPlot %>% bind_shiny("ggvis_d", "ggvis_ui_d")
  
  output$mytable =  renderDataTable({
    df = plotFrame()
    quantity = input$quant
    if(quantity=="Volume"){
      qLab = "Volume (vph)"
    }else if (quantity =="Speed"){
      qLab = "Speed (mph)"
    }else if (quantity=="Occupancy"){
      qLab ="Occupancy (%)"
    }
    colnames(df) = c("Station ID",qLab,"Outlier Status")
    locations = vector()
    for (i in 1:nrow(df)){
      locations[i]= stationMeta$locationtext[stationMeta$stationid==df$`Station ID`[i]][1]
    }
    
    createLink <- function(val) {
      sprintf('<a href="http://portal.its.pdx.edu/Portal/index.php/stations/view/id/%s/" target="_blank" class="btn btn-primary">%s</a>',val,val)
    }
    
    links = createLink(df$`Station ID`)
    
    outTable = cbind(links,locations,df[,2:3])
    colnames(outTable)= c("Station ID","Location",qLab,"Outlier Status")
    datatable(outTable,escape = FALSE) %>% formatStyle("Outlier Status",backgroundColor = styleEqual(c("OK","Minor","Major"),c("white","yellow","red")))
    })
  
  output$sumTable = renderDataTable({
    df = plotFrame()
    statSum = as.data.frame(as.matrix(summary(df$quantity)))
    colnames(statSum)="Value"
    ff = data.frame(matrix(nrow=4,ncol=1))
    colnames(ff)="Value"
    ff$Value= fences
    rownames(ff)=c("Lower Outer Fence","Lower Inner Fence","Upper Inner Fence","Upper Outer Fence")
    statSum=rbind(statSum,ff)
    #print(statSum)
    #print(rownames(statSum)[order(statSum$Value)])
    final=as.data.frame(cbind(rownames(statSum)[order(statSum$Value)] ,(statSum$Value[order(statSum$Value)])))
    colnames(final)=c("Statistic","Value")
    datatable(final) %>% formatStyle("Statistic",backgroundColor = styleEqual(c("Lower Outer Fence","Lower Inner Fence","Upper Inner Fence","Upper Outer Fence"),c("red","yellow","yellow","red")))
  })
  
})


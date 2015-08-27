library(lubridate)
library(shiny)
library(ggvis)
library(knitr)
library(plyr)

eb = read.csv("eastboundJuly2015.csv",stringsAsFactors = FALSE)
eb$time = as.POSIXct(strptime(eb$starttime, format ="%m/%d/%Y %H:%M"))

eb$period = cut(eb$time,breaks="1 hour")
eb$hour = hour(eb$period)
eb$tt_min = eb$traveltime/60

wb = read.csv("westboundJuly2015.csv",stringsAsFactors = FALSE)
wb$time = as.POSIXct(strptime(wb$starttime, format ="%m/%d/%Y %H:%M"))

wb$period = cut(wb$time,breaks="1 hour")
wb$hour = hour(wb$period)
wb$tt_min = wb$traveltime/60


shinyServer(function(input,output,session){
  
  ##Eastboung
  eb_hplot = reactive({
    hourSelect = input$hour
    binSelect = input$bin
    daySelect = input$weekdays
    eb$weekday = weekdays(eb$time,abbreviate = TRUE)
    
    sub = subset(eb,eb$hour==hourSelect & eb$weekday %in% daySelect)
    
    if(nrow(sub)>0){
      
      percentBins = quantile(sub$tt_min,probs = seq(0,1.0,0.05))
      binNums = data.frame(matrix(nrow=length(percentBins),ncol=2))
      colnames(binNums)=c("name","num")
      binNums$name[1:20]= paste0(names(percentBins)[1:20],"-",names(percentBins)[2:21])
      binNums$name[21]= "100%"
      binNums$num=as.numeric(rownames(binNums))
      
      for (i in 1:nrow(sub)){
        obs = sub$tt_min[i]
        
        if(obs==max(percentBins[length(percentBins)])){
          binNames = "95%-100%"
        }else if(obs ==min(percentBins[1])){
          binNames ="0%-5%"
        }
        else{
          lower = names(percentBins[percentBins == max(percentBins[obs >=percentBins])])
          upper = names(percentBins[percentBins == min(percentBins[obs <percentBins])])
          binNames = paste0(lower,"-",upper)
        }
        
        sub$pBin[i]= binNames
        sub$percentileBin[i] = binNums$num[binNums$name == binNames]
      }
      
      sub$pBin=factor(sub$pBin)
      
      tFunk = function(df){
        paste0("Percentile: ",binNums$name[df$percentileBin])
      }
      
      colRamp = colorRampPalette(colors=c("white","red"))
      
      statSum = as.data.frame(as.matrix(summary(sub$tt_min)))
      colnames(statSum)="Value"
      
      output$summary_eb = renderDataTable({
        statSum
      })
      
      h <- sub %>% ggvis(x = ~tt_min, fill=~percentileBin) %>%
        scale_numeric("fill",label="Percentile Bin", domain=c(1,20),range =colRamp(20)[c(1,20)])%>% group_by(percentileBin)  %>%
        layer_histograms(width=binSelect,closed = "left") %>%
        add_axis("x", title = "Travel Time (minutes)")%>%
        add_axis("y", title = "Frequency") %>% add_tooltip(tFunk) %>% 
        set_options(height = 480, width = "100%")%>% add_legend("fill",title="Percentile Bin Number")
      
      return(h)
    }else{
      empty = data.frame(nrow=0,ncol = 2)
      colnames(empty)=c("tt_min","percentileBin")
      output$summary =renderDataTable({
        empty
      })
      h = empty %>% ggvis(x = ~tt_min, fill=~percentileBin) %>%
        scale_numeric("fill", domain=c(1,20),range =colRamp(20)[c(1,20)])%>% group_by(percentileBin)  %>%
        layer_histograms(width=binSelect) %>%
        add_axis("x", title = "Travel Time (minutes)")%>%
        add_axis("y", title = "Frequency") %>% add_tooltip(tFunk) %>% 
        set_options(height = 480, width = "100%") %>% add_legend("fill",title="Percentile Bin Number")
    }
    
    return(h)
    
  })
  
  eb_hplot %>% bind_shiny("ggvis_eb", "ggvis_ui_eb")
  
  ##Westbound
  wb_hplot = reactive({
    hourSelect = input$hour
    binSelect = input$bin
    daySelect = input$weekdays
    wb$weekday = weekdays(wb$time,abbreviate = TRUE)
    
    sub = subset(wb,wb$hour==hourSelect & wb$weekday %in% daySelect)
    
    if(nrow(sub)>0){
      
      percentBins = quantile(sub$tt_min,probs = seq(0,1.0,0.05))
      binNums = data.frame(matrix(nrow=length(percentBins),ncol=2))
      colnames(binNums)=c("name","num")
      binNums$name[1:20]= paste0(names(percentBins)[1:20],"-",names(percentBins)[2:21])
      binNums$name[21]= "100%"
      binNums$num=as.numeric(rownames(binNums))
      
      for (i in 1:nrow(sub)){
        obs = sub$tt_min[i]
        
        if(obs==max(percentBins[length(percentBins)])){
          binNames = "95%-100%"
        }else if(obs ==min(percentBins[1])){
          binNames ="0%-5%"
        }
        else{
          lower = names(percentBins[percentBins == max(percentBins[obs >=percentBins])])
          upper = names(percentBins[percentBins == min(percentBins[obs <percentBins])])
          binNames = paste0(lower,"-",upper)
        }
        
        sub$pBin[i]= binNames
        sub$percentileBin[i] = binNums$num[binNums$name == binNames]
      }
      
      sub$pBin=factor(sub$pBin)
      
      tFunk = function(df){
        paste0("Percentile: ",binNums$name[df$percentileBin])
      }
      
      colRamp = colorRampPalette(colors=c("white","red"))
      
      statSum = as.data.frame(as.matrix(summary(sub$tt_min)))
      colnames(statSum)="Value"
      
      output$summary_wb = renderDataTable({
        statSum
      })
      
      h <- sub %>% ggvis(x = ~tt_min, fill=~percentileBin) %>%
        scale_numeric("fill",label="Percentile Bin", domain=c(1,20),range =colRamp(20)[c(1,20)])%>% group_by(percentileBin)  %>%
        layer_histograms(width=binSelect,closed = "left") %>%
        add_axis("x", title = "Travel Time (minutes)")%>%
        add_axis("y", title = "Frequency") %>% add_tooltip(tFunk) %>% 
        set_options(height = 480, width = "100%")%>% add_legend("fill",title="Percentile Bin Number")
      
      return(h)
    }else{
      empty = data.frame(nrow=0,ncol = 2)
      colnames(empty)=c("tt_min","percentileBin")
      output$summary =renderDataTable({
        empty
      })
      h = empty %>% ggvis(x = ~tt_min, fill=~percentileBin) %>%
        scale_numeric("fill",label="Percentile Bin", domain=c(1,20),range =colRamp(20)[c(1,20)])%>% group_by(percentileBin)  %>%
        layer_histograms(width=binSelect) %>%
        add_axis("x", title = "Travel Time (minutes)")%>%
        add_axis("y", title = "Frequency") %>% add_tooltip(tFunk) %>% 
        set_options(height = 480, width = "100%")%>% add_legend("fill",title="Percentile Bin Number")
    }
    
    return(h)
    
  })
  
  wb_hplot %>% bind_shiny("ggvis_wb", "ggvis_ui_wb")
  
  
}
  
)
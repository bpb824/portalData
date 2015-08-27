library(lubridate)
library(shiny)
library(shinyapps)
library(ggvis)
library(knitr)

setwd("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/bluetooth/")

blue = read.csv("july2015.csv",stringsAsFactors = FALSE)
blue$time = as.POSIXct(strptime(blue$starttime, format ="%m/%d/%Y %I:%M %p"))

blue$period = cut(blue$time,breaks="1 hour")
blue$hour = hour(blue$period)
blue$tt_min = blue$traveltime/60

shinyApp(
  ui = fluidPage(
    titlePanel(title="Travel time histogram for July 2015 heading from Powell & 8th to Powell & 77th"),
    sidebarLayout(
      sidebarPanel(sliderInput("bin","Bin Width",min=0.5,max=5,value =1,step = 0.5),
                   sliderInput("hour","Hour of day",min=1,max=24,value=8, step = 1)),
      mainPanel(
        uiOutput("ggvis_ui"),ggvisOutput("ggvis")
      )
    )
  ),
  
  server = function(input,output,session){
    
    hplot = reactive({
      hourSelect = input$hour
      binSelect = input$bin
      
      sub = subset(blue,blue$hour==hourSelect)
      
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
        sub$pBinNum[i] = binNums$num[binNums$name == binNames]
      }
      
      sub$pBin=factor(sub$pBin)
      
      tFunk = function(df){
        paste0("Percentile: ",binNums$name[df$pBinNum])
      }
      
      colRamp = colorRampPalette(colors=c("white","red"))
      
      h <- sub %>% ggvis(x = ~tt_min, fill=~pBinNum) %>%
        scale_numeric("fill",label="Percentile Bin", domain=c(1,20),range =colRamp(20)[c(1,20)])%>% group_by(pBinNum)  %>%
        layer_histograms(width=binSelect) %>%
        add_axis("x", title = "Travel Time (minutes)")%>%
        add_axis("y", title = "Frequency") %>% add_tooltip(tFunk)
      
      return(h)
      
    })
    
    hplot %>% bind_shiny("ggvis", "ggvis_ui")
    
  }
)


###Applying ODOT ITS Bluetooth data filter

east = read.csv("eastboundJuly2015.csv")
west = read.csv("westboundJuly2015.csv")

dist = 3.6 #miles

east$speed_mph = dist/(east$traveltime/3600)
west$speed_mph = dist/(west$traveltime/3600)

tt_filter= function(data,dist){
  data$speed_mph = dist/(data$traveltime/3600)
  maxFilter = floor((dist/1)*3600)
  minFilter = floor((dist/(35+15))*3600)
  upper = mean(data$traveltime)+1.65*sd(data$traveltime)
  data = subset(data,data$traveltime >=minFilter & data$traveltime <=maxFilter & data$traveltime <= upper)
  
}



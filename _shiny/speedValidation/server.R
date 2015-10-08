require(lubridate)
require(shiny)
require(ggvis)
require(knitr)
require(plyr)

#Read in speed data
speeds = readRDS("processedSpeedData.rds")
locVec = vector()
for (i in 1:length(speeds)){
  locVec[i]=paste0(speeds[[i]]$highway," @ ",speeds[[i]]$crossStreet)
}

#Set up labels
dataLabs = data.frame(matrix(nrow=3,ncol=2))
colnames(dataLabs)= c("variable","label")
dataLabs$variable = c("observed","portal","hist")
dataLabs$label = c("Observed","Same-time PORTAL Data","10-Week Historical Average")

shinyServer(function(input,output,session){

  #Add legend image to output
  output$legend <- renderImage({
    # Return a list containing the filename
    list(src = "legend.png")
  }, deleteFile = FALSE)

  #Function for extracting data from list
  extract = function(index,agg){
    speed = speeds[[index]]
    data = speed$melted
    data = data[complete.cases(data),]
    data$period = cut(as.POSIXct(data$time),paste0(agg," min"))
    agged = ddply(data,c("period","variable"),summarise,value=mean(value))
    agged$time=as.POSIXct(agged$period)
    agged = join(agged,dataLabs, by="variable")
    return(agged)
  }

  #Function for adding title to plot
  add_title <- function(vis, ..., x_lab = "X units", title = "Plot Title")
  {
    add_axis(vis, "x", title = x_lab, properties = axis_props(labels = list(angle=270, align = "right")),title_offset = 50) %>%
      add_axis("x", orient = "top", ticks = 0, title = title,
               properties = axis_props(
                 axis = list(stroke = "white"),
                 labels = list(fontSize = 0)
               ), ...)
  }

  #Create plot 1 object reactively
  plt1= reactive({
    indices = which(locVec==input$loc)
    agg=input$agg
    agged = extract(indices[1],agg)
    aggedOther = extract(indices[2],agg)

    lane = paste0("Lane ",speeds[[indices[1]]]$lane)

    ranges = c(range(agged$value),range(aggedOther$value))
    dmn = c(min(ranges),max(ranges))

    p = agged %>% ggvis(x=~time,y=~value, fill=~label,stroke=~label) %>% group_by(variable) %>% layer_smooths(se=TRUE) %>%
      layer_points() %>% add_axis("y",title="Speed (mph)")  %>% hide_legend(scales = c("stroke","fill")) %>%
      set_options(height = 400, width = "100%") %>% scale_numeric("y",domain= dmn, nice = TRUE)%>%
      add_title(title = lane, x_lab = "Time")
    return(p)
  })

  #Create plot 2 object reactively
  plt2= reactive({
    indices = which(locVec==input$loc)
    agg=input$agg
    agged = extract(indices[2],agg)
    aggedOther = extract(indices[1],agg)

    lane = paste0("Lane ",speeds[[indices[2]]]$lane)

    ranges = c(range(agged$value),range(aggedOther$value))
    dmn = c(min(ranges),max(ranges))

    p = agged %>% ggvis(x=~time,y=~value, fill=~label,stroke=~label) %>% group_by(variable) %>% layer_smooths(se=TRUE) %>%
      layer_points() %>% add_axis("y",title="Speed (mph)") %>% hide_legend(scales = c("stroke","fill")) %>%
      set_options(height = 400, width = "100%") %>% scale_numeric("y",domain= dmn, nice = TRUE)%>%add_title(title = lane, x_lab = "Time")

    return(p)
  })

  #Print plot objects to shiny app
  plt1 %>% bind_shiny("ggvis_1", "ggvis_ui_1")
  plt2 %>% bind_shiny("ggvis_2", "ggvis_ui_2")

}

)

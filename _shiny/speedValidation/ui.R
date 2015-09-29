library(lubridate)
library(shiny)
library(ggvis)
library(knitr)
library(DT)

speeds = readRDS("processedSpeedData.rds")
locVec = vector()
for (i in 1:length(speeds)){
  locVec[i]=paste0(speeds[[i]]$highway," @ ",speeds[[i]]$crossStreet)
}
locVec = unique(locVec)


shinyUI(
  fluidPage(
    fixedRow(
      column(8,h1("Speed Validation Explorer")),
      column(4,imageOutput("legend", height ="10px",inline = TRUE))
    ),
    fixedRow(column(6,
                    uiOutput("ggvis_ui_1"),ggvisOutput("ggvis_1")
                    ),
             column(6,
                    uiOutput("ggvis_ui_2"),ggvisOutput("ggvis_2")
             )
    ),
    fixedRow(column(12,
                    wellPanel(selectInput("loc","Location",choices=locVec,selected=locVec[1]),
                              sliderInput("agg","Aggregation (minutes)",min=1,max=5,step=1,value=2))     
    )
    )
    
  )
)
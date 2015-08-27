library(lubridate)
library(shiny)
library(ggvis)
library(knitr)
library(DT)

shinyUI(
  fluidPage(
    titlePanel(title="Travel times for July 2015 between Powell & 8th and Powell & 77th"),
    fixedRow(column(12,
      tabsetPanel(
        tabPanel(title="Eastbound-Histogram",
                 uiOutput("ggvis_ui_eb"),ggvisOutput("ggvis_eb")
        ),
        tabPanel(title="Eastbound-Statistics",
                 dataTableOutput("summary_eb")
        ),
        tabPanel(title="Westbound-Histogram",
                 uiOutput("ggvis_ui_wb"),ggvisOutput("ggvis_wb")
        ),
        tabPanel(title="Westbound-Statistics",
                 dataTableOutput("summary_wb")
        )
      )
    )),
    fixedRow(column(12,
                    wellPanel(sliderInput("bin","Bin Width (minutes)",min=0.5,max=5,value =1,step = 0.5),
                    sliderInput("hour","Hour of Day",min=0,max=23,value=8, step = 1),
                    checkboxGroupInput("weekdays","Days of Week",choices=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),selected =c("Mon","Tue","Wed","Thu","Fri"),inline=TRUE ))     
                    )
    )
      
  )
)
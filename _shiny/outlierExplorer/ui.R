library(shiny)
library(ggvis)
library(knitr)
library(stats)
library(DT)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("PORTAL Interactive Explorer (June 2015 Data)"),
  
  fixedRow(column(10,offset = 1,
         tabsetPanel(
           tabPanel("histogram",uiOutput("ggvis_ui"),ggvisOutput("ggvis")),
           tabPanel("density",
                    sliderInput("bandwidth","Bandwidth",0.1,2,1,0.1),
                    uiOutput("ggvis_ui_d"),ggvisOutput("ggvis_d")),
           tabPanel("table",dataTableOutput("mytable")),
           tabPanel("summary",dataTableOutput("sumTable"))
         )
  )),
  
  fixedRow(
    
    column(10,offset = 1, wellPanel(
      selectInput(inputId="filter",label="Data Filter",choices=c("Filtered","Unfiltered"),selected="Filtered"),
      sliderInput(inputId="hour",label="Hour of Day",min = 0,max=23,value = 8),
      checkboxGroupInput("weekdays","Days of Week",choices=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),selected =c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),inline=TRUE ),
      selectInput(inputId="lane",label="Lane Number",choices = c("1","2","3","4","All (Aggregated)"),selected="All (Aggregated)"),
      selectInput(inputId="quant",label = "Plot Quantity",choices=c("Volume","Speed","Occupancy"),selected="Speed"),
      sliderInput(inputId="hist",label = "Bin Width (mph)",min = 1,max = 10,value = 5))
    )),
  
  fixedRow(column(10,offset =1,
                  wellPanel(tags$h1("Outlier Definitions"),tags$ul(tags$li("Minor outliers lie outside the 'inner fence', which is defined by  the interval [1st Quartile - 1.5*IQR, 3rd Quartile +1.5*IQR]"),
                          tags$li("Major outliers lie outside the 'outer fence', which is defined by the interval [1st Quartile - 3*IQR, 3rd Quartile +3*IQR]"))
          ))
  )
))
library(shiny)
library(ggvis)
library(knitr)

shinyUI(fluidPage(
  fixedRow(column(width = 8,
    wellPanel(sliderInput("bin","Bin Width (mph)",min=1,max=10,value=5,step=1))
    )),
  fixedRow(column(width=8, 
                  uiOutput("ggvis_ui"),
                  ggvisOutput("ggvis"),
                  wellPanel(tags$ul(tags$li("Minor outliers lie outside the 'inner fence' with is defined by  the interval [1st Quartile - 1.5*IQR, 3rd Quartile +1.5*IQR]"),
                          tags$li("Major outliers lie outside the 'outer fence' with is defined by the interval [1st Quartile - 3*IQR, 3rd Quartile +3*IQR]"))      
                  )))

))
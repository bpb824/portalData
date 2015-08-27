library(shiny)
library(ggvis)
library(knitr)

shinyUI(fluidPage(
  mainPanel(
    uiOutput("ggvis_ui"),
    ggvisOutput("ggvis"),
    tags$ul(tags$li("Minor outliers lie outside the 'inner fence' with is defined by  the interval [1st Quartile - 1.5*IQR, 3rd Quartile +1.5*IQR]"),
            tags$li("Major outliers lie outside the 'outer fence' with is defined by the interval [1st Quartile - 3*IQR, 3rd Quartile +3*IQR]"))
  )
  
  
))
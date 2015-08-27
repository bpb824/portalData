library(devtools)
library(rCharts)
library(htmlwidgets)
library(ggvis)
library("knitr")

img.width <- 450
img.height <- 450
options(RCHART_HEIGHT = img.height, RCHART_WIDTH = img.width)
opts_chunk$set(fig.width=6, fig.height=4)

setwd("/Users/bblanc/OneDrive/_ODOT/_Portal/investigations/laneNumbers/")
laneCheck = readRDS("laneNumberCheck.rds")
laneCheck$flag=FALSE
laneCheck$flag[laneCheck$pDiff_10 <0]=TRUE
laneFrame = laneCheck[complete.cases(laneCheck),]

hist(laneCheck$pDiff_10)

rPlot(x = "bin(pDiff_10,3)", y = "count(pDiff_10)", data = laneFrame, type = "bar",ylab="Count")

tooltipFunc <- function(index, value) {
  function(x) {
    if (is.null(x)) return(NULL)
    else {
      strwrap(
        paste(index[ (value >= x$xmin_ ) & (value <= x$xmax_) ],
              collapse=', '),
        width = 30)
    }
  }
}

h <- laneFrame %>% ggvis(x = ~pDiff_10) %>% layer_histograms(width=input_slider(min=1,max=10,step=1)) %>% 
  set_options(width = img.width, height = img.height) %>% 
  add_tooltip(tooltipFunc(laneFrame$stationid, laneFrame$pDiff_10), 'hover') %>%
  add_axis("x", title = "10th Percentile Difference in Speed Between Lanes 1 and 2")%>%
  add_axis("y", title = "Frequency")

saveWidget(h,"laneNumbers.html")


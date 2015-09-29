library(shiny)
library(ggvis)
library(knitr)
library(stats)

shinyServer(function(input, output, session) {
  laneCheck = readRDS("laneNumberCheck.rds")
  laneCheck$flag=FALSE
  laneCheck$flag[laneCheck$pDiff_10 <0]=TRUE
  laneFrame = laneCheck[complete.cases(laneCheck),]
  quants = quantile(laneFrame$pDiff_10,c(0.25,0.75))
  iqr = as.numeric(quants[2]-quants[1])
  fences = as.numeric(c(quants[1]-3*iqr,quants[1]-1.5*iqr,quants[2]+1.5*iqr,quants[2]+3*iqr))
  laneFrame$out = "OK"
  laneFrame$out[(laneFrame$pDiff_10 <fences[2] & laneFrame$pDiff_10 >= fences[1])|(laneFrame$pDiff_10 <=fences[4] & laneFrame$pDiff_10 > fences[3])]="Minor"
  laneFrame$out[(laneFrame$pDiff_10 <fences[1])|(laneFrame$pDiff_10 >fences[4])]="Major"
  laneFrame$out=factor(laneFrame$out,ordered = TRUE)
  
  tooltipFunc <- function(index, value, out) {
    function(x) {
      if (is.null(x)) return(NULL)
      else {
        strwrap(
          paste(index[ (value >= x$xmin_ ) & (value <= x$xmax_) & out == x$out],
                collapse=', '),
          width = 30)
      }
    }
  }
  
  h = reactive({
    laneFrame %>% ggvis(x = ~pDiff_10,fill= ~out) %>% group_by(out) %>%
      layer_histograms(width=input$bin) %>%
      scale_nominal("fill", label = "Outlier Status",
                    domain = c("OK", "Minor", "Major"),
                    range = c("green", "yellow", "red")) %>%
      add_tooltip(tooltipFunc(laneFrame$stationid, laneFrame$pDiff_10,laneFrame$out), 'hover') %>%
      add_axis("x", title = "5th Percentile Difference in Speed Between Lanes 1 and 2 (mph)")%>%
      add_axis("y", title = "Frequency") %>% set_options(height = 300, width = "100%")
  }) 
  h %>% bind_shiny("ggvis", "ggvis_ui")
})


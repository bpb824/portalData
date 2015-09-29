library(shiny)
library(leaflet)
library(RColorBrewer)
library(DT)

plotList = readRDS("plotList.rds")
mapFrame = readRDS("mapFrame.rds")
tabFrame = readRDS("tabFrame.rds")

timePoints = as.Date(c("2015-01-01","2015-07-31"))

function(input, output, session) {

  pal <- colorNumeric(
    palette = c("Green","Yellow","Red"),
    domain = c(0,max(mapFrame$numOutages))
  )

  output$leaf <- renderLeaflet({
    leaflet(mapFrame) %>% addProviderTiles("CartoDB.DarkMatter",options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-122.6662589, 45.5317385, zoom = 9) %>% addCircleMarkers(lng=~long,lat=~lat,popup =~htmlLink,color =~pal(numOutages),fillOpacity = 0.6,stroke=FALSE,layerId=~station_id) %>%
      addLegend("bottomright", pal = pal, values = ~numOutages,
                title = "Number of Outages",
                opacity = 1
      )
  })

  add_title <- function(vis, ..., x_lab = "X units", title = "Plot Title")
  {
    add_axis(vis, "x", orient = "top", ticks = 0, title = title,
             properties = axis_props(
               axis = list(stroke = "white"),
               labels = list(fontSize = 0)
             ), ...)
  }

  plt = reactive({
    markerID = input$leaf_marker_click$id
    if(!is.null(markerID)){
      sid = input$leaf_marker_click$id
      plotFrame = plotList[[as.character(sid)]]

      tFunk = function(x){
        if(is.null(x)) return(NULL)
        return(as.character(plotFrame$day[plotFrame$id == x$id]))
      }

      plotFrame %>% ggvis(x=~day, y=~ variable, stroke =~def, fill =~def, key:=~id)  %>%
        layer_points(opacity := 0.5) %>% add_tooltip(tFunk) %>%
        set_options(height = 200, width = "100%") %>%
          scale_nominal(property="fill", label = "Outage Flag", domain = c("Very Low Values","Only Empty Data", "Zero Rows of Data", "Detector Unavailable"), range = brewer.pal(4,"Set1")) %>%
        scale_nominal(property="stroke", label = "Outage Flag", domain = c("Very Low Values","Only Empty Data", "Zero Rows of Data", "Detector Unavailable"), range = brewer.pal(4,"Set1")) %>%
        add_axis("y", title = "Lane Number", title_offset=50)%>%
        add_axis("x", title = "")%>% scale_datetime(property="x",domain = timePoints,nice="month", expand=0)%>% add_title(title = tabFrame$location[tabFrame$sid == sid])
    }
  })

  observeEvent(input$leaf_marker_click,{
    plt %>% bind_shiny("ggvis", "ggvis_ui")
  })

  output$outageData =  renderDataTable({
    colnames(tabFrame)=c("Station","Location",c("# Days w/ Very Low Values","# Days w/ Only Empty Data", "# Days w/ Zero Rows of Data"),"# Days w/ Outages (Total)")

    createLink <- function(val) {
      sprintf('<a href="http://portal.its.pdx.edu/Portal/index.php/stations/view/id/%s/" target="_blank" class="btn btn-primary">%s</a>',val,val)
    }

    links = createLink(tabFrame$Station)

    tabFrame$Station = links
    datatable(tabFrame,escape = FALSE, options = list(lengthMenu = c(5, 10, 50), pageLength = 5))
  })

}

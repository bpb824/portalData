library(shiny)
library(leaflet)
library(ggvis)

fluidPage(
  titlePanel(title = "",windowTitle = "Data Outage Explorer"),
  fixedRow(
    leafletOutput("leaf", width = "100%",height = 400)
    ),
  p("Click a point on the map to plot data outages"),
  fixedRow(
    uiOutput("ggvis_ui"),ggvisOutput("ggvis")
  )
)
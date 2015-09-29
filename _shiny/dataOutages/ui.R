library(shiny)
library(leaflet)
library(ggvis)
library(DT)

fluidPage(
  titlePanel(title = "",windowTitle = "Data Outage Explorer"),
  tabsetPanel(
    tabPanel("Outage Explorer",
             fixedRow(
               leafletOutput("leaf", width = "100%",height = 300)
             ),
             p("Click a point on the map to plot data outages"),
             fixedRow(
               uiOutput("ggvis_ui"),ggvisOutput("ggvis")
             )
    ),
    tabPanel("Outage Table",
      dataTableOutput("outageData")
    )
  )
  
)
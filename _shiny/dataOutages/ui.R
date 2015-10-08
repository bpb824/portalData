require(shiny)
require(leaflet)
require(ggvis)
require(DT)

fluidPage(
  titlePanel(title = "",windowTitle = "Data Outage Explorer"),

  #layout tab panel for map, plot, and table
  tabsetPanel(
    tabPanel("Outage Explorer",
             #Map
             fixedRow(
               leafletOutput("leaf", width = "100%",height = 300)
             ),
             #User help text
             p("Click a point on the map to plot data outages"),
             #Plot
             fixedRow(
               uiOutput("ggvis_ui"),ggvisOutput("ggvis")
             )
    ),
    #Table
    tabPanel("Outage Table",
      dataTableOutput("outageData")
    )
  )

)

# SERVER



server <- function(input, output, session) {

## Interactive Map ----

# Create the map
output$map <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%
    setView(lng = -76.00, lat = 38.50, zoom = 8) %>%
    addProviderTiles(providers$CartoDB.Positron)
})

# A reactive expression that returns the set of zips that are
# in bounds right now
zipsInBounds <- reactive({
  if (is.null(input$map_bounds))
    return(siteData02[FALSE,])
  bounds <- input$map_bounds
  latRng <- range(bounds$north, bounds$south)
  lngRng <- range(bounds$east, bounds$west)

  subset(siteData02,
         latitude >= latRng[1] & latitude <= latRng[2] &
           longitude >= lngRng[1] & longitude <= lngRng[2]
  )
})

# This observer is responsible for maintaining the circles and legend,
# according to the variables the user has chosen to map to color and size.
observe({

  # Set colors based on all sites so that colors stay the same
  colorData <- siteData02[["Num_Events"]]
  pal       <- colorBin("Accent", colorData, 7, pretty = FALSE)
  radius    <- 1500
  siteData02$myCol <- pal(colorData)

  # Filter data to map based on user selections
  toPlot <- siteData02 %>%
    filter(.
           , boo.CBP144 %in% input$CBPCoreMap
           , Num_Events >= input$minEpisodesMap
           , Num_Events <= input$maxEpisodesMap
           , year_min <= input$minYearMap
           , year_max >= input$maxYearMap
    )

  # add locations from "toPlot" to map
  leafletProxy("map", data = toPlot) %>%
    clearShapes() %>%
    addCircles(~Longitude, ~Latitude, radius=radius, layerId=~MonitoringLocation,
               stroke=TRUE, weight=0.75, color="black", fillOpacity=0.4, fillColor=toPlot$myCol) %>%
    addLegend("bottomleft", pal=pal, values=colorData, title="Events [#]",
              layerId="colorLegend")
})

# Show a popup at the given location
showSitePopup <- function(MonitoringLocation, Latitude, Longitude) {
  selectedZip <- df02[df02$MonitoringLocation == MonitoringLocation,]
  content <- as.character(tagList(
    tags$h4("Site: ", as.character(selectedZip$MonitoringLocation)),
    tags$br(),
    sprintf("CBP Site: %s", as.character(selectedZip$boo.CBP144)),
    tags$br(),
    sprintf("Year Range: %s | %s", as.integer(selectedZip$year_min),as.integer(selectedZip$year_max)),
    tags$br(),
    sprintf("Events [#]: %s", as.integer(selectedZip$Num_Events)),
    tags$br(),
    sprintf("10th|90th Depth [m]: %s | %s", as.integer(selectedZip$TotalDepth_10),as.integer(selectedZip$TotalDepth_90)),
    tags$br(),
    sprintf("10th|90th Sample Depth [m]: %s | %s", as.integer(selectedZip$maxSampleDepth_10),as.integer(selectedZip$maxSampleDepth_90)),
    tags$br()
  ))
  leafletProxy("map") %>% addPopups(Longitude, Latitude, content, layerId = MonitoringLocation)
}

# When map is clicked, show a popup with city info
observe({
  leafletProxy("map") %>% clearPopups()
  event <- input$map_shape_click
  if (is.null(event))
    return()

  isolate({
    showSitePopup(event$id, event$lat, event$lng)
  })
})

## Data Explorer ----
observe({
  if (is.null(input$goto))
    return()
  isolate({
    map <- leafletProxy("map")
    map %>% clearPopups()
    dist <- 0.5
    zip <- input$goto$zip
    lat <- input$goto$lat
    lng <- input$goto$lng
    showSitePopup(zip, lat, lng)
    map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  })
})

# filter "cleantable02" based on user selections
output$ziptable <- DT::renderDataTable({
  df <- cleantable02 %>%
    filter(.
           , CBP %in% input$CBPCoreTable
           , Num_Events >= input$minEpisodesTable
           , Num_Events <= input$maxEpisodesTable
           , Year_min <= input$minYearTable
           , Year_max >= input$maxYearTable
    ) %>%
    mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude
                          , '" data-long="', Longitude
                          , '" data-zip="', site
                          , '"><i class="fa fa-crosshairs"></i></a>', sep=""))
  action <- DT::dataTableAjax(session, df, outputId = "ziptable")

  DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)

})## SERVER ~ end

}  # server

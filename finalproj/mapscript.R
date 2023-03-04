library(htmltools)

data = read_excel('public_housing.xlsx')

head(data)

data2 = filter(data, COUNTY_NAME == "Allegheny")

data2$LONGITUDE = as.numeric(data2$LONGITUDE)
data2$LATITUDE = as.numeric(data2$LATITUDE)


pal <- colorBin(
  palette = "RdYlBu",
  domain = data2$INSPECTION_SCORE, 4, pretty = FALSE)

leaflet(data2) %>%
  addProviderTiles(providers$CartoDB) %>%
  addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, radius = 1.5, 
                   color = ~pal(INSPECTION_SCORE), 
                   label = ~htmlEscape(paste('Inspection Score:',data2$INSPECTION_SCORE)),
                   clusterOptions = markerClusterOptions()) %>%
  addLegend(position = "topright" , pal = pal,
            values = data2$INSPECTION_SCORE, title = "Inspection Score")


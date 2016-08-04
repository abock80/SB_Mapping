library(leaflet)
library(RColorBrewer)

#*********************************************
# STEP ONE GENERATE LEAFLET MAP - from slider
pal<-colorNumeric(
  palette = "Blues",
  domain=perChange
)

pal2 <- colorQuantile("YlGn", NULL, n = 10)

# design choice - segments or buffered polygons
popup <- paste0("<strong>Name: </strong>", 
                finalSegs@data$seg_id)

leaflet(finalSegs) %>% addTiles() %>% addPolylines(color=~pal2(perChange),weight=3,popup=~popup) %>%
  addLegend("bottomright",pal=pal2,values=~perChange,title="Change in Streamflow(%), 2055")


# explore setMapBounds, fitBounds, setView
# add more elegance to popups
# find better basemap
#*********************************************
# STEP TWO GENERATE PLOT FOR SINGLE SITE
#Fut MM, base MM - for OF Site
#say the user selects OF WShed and segid
# select OFS, seg13


# STEP THREE GENERATE/FORMAT OUTPUT FILE
library(leaflet)

colorRamp<-rev(colorspace::diverge_hsv(10, h = c(240, 0), s = 1, v = 1, power = 1,
                           gamma = NULL, fixup = TRUE, alpha = 1))

ibr<-round(quantile(sfMeans,0:10/10,na.rm=TRUE),6)
# the base verions of "cut" fails to include the max number
#symb<-cut(sfMeans, breaks=levels(factor(ibr)), include.lowest=TRUE, right = T)
symb<-Hmisc::cut2(sfMeans, g=10)

pal<-colorNumeric(
  palette = "Blues",
  domain=sfMeans
)

pal2 <- colorQuantile("YlGn", NULL, n = 10)

popup <- paste0("<strong>Name: </strong>", 
                GF_segs@data$seg_id)


#leaflet(GF_segs) %>% addTiles() %>% addPolylines(color=~colorRamp[symb],weight=2)
leaflet(GF_segs) %>% addTiles() %>% addPolylines(color=~pal2(sfMeans),weight=2,popup=~popup)
# explore setMapBounds, fitBounds, setView
# add more elegance to popups

# R libraries required - sbtools, sp, gdalUtils, RCurl, RColorBrewer, maps, mapdata
# functions are a part of package specificed (sbtools::item_get - item_get is function in sbtools library)
# Functions on line 49-50 need to have the libraries loaded (maps,mapdata) to access the databases 
library(maps)
library(mapdata)

#*******************************************
## for Kathy's data link to GeoFab
## get the WFS
#layer<-sbtools::item_get_wfs("57114f7be4b0ef3b7ca554e8")
## find the projection of the shapefile
#print(sp::proj4string(layer))
## check to see if item is in WGS84
## re-project layer to decimal degrees, WGS84
# layer_dd<-sp::spTransform(layer,"+init=epsg:4326")

#*****************************************************************************
# set up plot options and make example plot with one data series
# color palette from RcolorBrewer
colPal<-RColorBrewer::brewer.pal(4,"Set1")
# Map sites2$M2p25
# hard breaks for symbology
#fixedBreaks=c(-30,-15,0,15,30)
fixedBreaks=c(min(data2$FiftyFives), quantile(data2$FiftyFives,.25),median(data2$FiftyFives),quantile(data2$FiftyFives,.75),max(data2$FiftyFives))
symb<-cut(data2$FiftyFives,breaks=fixedBreaks,include.lowest=TRUE,right=TRUE)

op<-par(family="serif")

par(mar=c(5.1,4.1,4.1,8.1))

sp::plot(layer_dd,col=colPal[symb],border=FALSE)
map('state','montana',add=TRUE)
map('rivers',add=TRUE,col=4,lwd=4)
mtext("Some Title",cex=2,line=0)
box(which="plot",lty="solid")

cutsChar<-as.character(symb)
cuts<-as.numeric(levels(factor (fixedBreaks)))
mapLegend = c(paste(cuts[1]," to ",cuts[2],sep=""),paste(cuts[2]," to ",cuts[3],sep=""),paste(cuts[3]," to ",cuts[4],sep=""),
              paste(cuts[4]," to ",cuts[5],sep=""))
#par(xpd=NA,font=21)
par(xpd=TRUE)
legend("topright",inset=c(-0.3,0), legend=mapLegend, fill=colPal,col=colPal,title="Title (units)",horiz=F,bty="y")

par(op)

#****************************************************
##Other plotting Options
#png("d:/abock/temp/test.png",height=1000,width=1000)
#map('state','montana')
## need to convert sites2 to spatial object
#plot(layer,col=colPal[symb],pch=1,add=TRUE)
#dev.off()
#browseURL("d:/abock/temp/test.png")


## plotting options 1 - png,devoff, browseURL
## plotting options 2 - x11, devoff
#x11()
#map('state','montana')
#map('rivers',add=TRUE,col=4)
#plot(layer,col=colPal[symb],pch=1,add=TRUE)
#dev.off()

#****************************************************************
#GDAL troubleshooting
#http://trac.osgeo.org/osgeo4w/
#https://stat.ethz.ch/pipermail/r-sig-geo/2016-January/023872.html

#layer<-item_get_wfs("56699c83e4b08895842a1cee")
#basins_wgs <- spTransform(layer, "+init=epsg:4326")

#**************************
## 7/12 - This RCurl will not work on non-public items
## Retrieve the baseline data for a single site
#base <- RCurl::getURL(sbfiles$url[baseLine])
#base2 <- read.csv(text=base)
#names(base2)

#******************************
#**************************************************************
## If item is an internal item, so we must authenticate to access
# sbtools::authenticate_sb("abock@usgs.gov")

## how to download data that is non-public with authentication
#data<-sbtools::item_file_download(test_item,dest_dir="d:/abock/temp")

## option 2 for non-public data, autheticate with req
##http://stackoverflow.com/questions/23286900/rcurl-basic-authentication-with-api-key
#library(httr)
#req <- GET(sbfiles$url[1], 
#           authenticate("Username:abock@usgs.gov", "Password:Jay22Heyward!!", type = "basic"))
#stop_for_status(req)
#content(req)
#data2 <- content(req)
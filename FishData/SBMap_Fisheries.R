# R libraries required - sbtools, sp, gdalUtils, RCurl, RColorBrewer, maps, mapdata
# functions are a part of package specificed (sbtools::item_get - item_get is function in sbtools library)
# Functions on line 49-50 need to have the libraries loaded (maps,mapdata) to access the databases 
library(maps)
library(mapdata)

# get the SB item information
# this is a dummy page for Roy's data
#57115024e4b0ef3b7ca554f3
test_item<-sbtools::item_get("57115024e4b0ef3b7ca554f3")
names(test_item)
parent<-sbtools::item_get(test_item$parentId)
sbtools::item_list_children(parent)
sbtools::item_list_files(test_item)

# get the WFS
layer<-sbtools::item_get_wfs("57115024e4b0ef3b7ca554f3")

# get the data
# downloads to a local directory
#item_file_download(test_item,dest_dir="d:/abock/temp")
sbfiles<-sbtools::item_list_files(test_item)
print(sbfiles)
sites <- RCurl::getURL(sbfiles$url[1])
data <- RCurl::getURL(sbfiles$url[2])
sites2 <- read.csv(text=sites)
data2 <- read.csv(text=data)

#proper nomenclature of lat and long
names(sites2) <- sub("Long", "x", names(sites2))
names(sites2) <- sub("Lat", "y", names(sites2))
sp::coordinates(sites2)<-~x+y
sp::proj4string(sites2)<-sp::CRS("+init=epsg:4326")

# set up plot options
# color palette
#colPal<-c("#00A4DE","#5DFC21","#FFD701","#FF3300")
#RColorBrewer::display.brewer.pal(4,"Set1")
colPal<-RColorBrewer::brewer.pal(4,"Set1")
# Map sites2$M2p25
#fixedBreaks=c(-30,-15,0,15,30)
fixedBreaks=c(min(sites2$M2p25), quantile(sites2$M2p25,.25),median(sites2$M2p25),quantile(sites2$M2p25,.75),max(sites2$M2p25))
symb<-cut(sites2$M2p25,breaks=fixedBreaks,include.lowest=TRUE,right=TRUE)

op<-par(family="serif")

map('state','montana')
map('rivers',add=TRUE,col=4,lwd=4)
sp::plot(layer,col=colPal[symb],pch=16,add=TRUE)
mtext("Some Title",cex=2,line=0)
box(which="plot",lty="solid")

cutsChar<-as.character(symb)
cuts<-as.numeric(levels(factor (fixedBreaks)))
mapLegend = c(paste(cuts[1]," to ",cuts[2],sep=""),paste(cuts[2]," to ",cuts[3],sep=""),paste(cuts[3]," to ",cuts[4],sep=""),
              paste(cuts[4]," to ",cuts[5],sep=""))

#par(xpd=NA,font=21)
legend("bottomleft", legend=mapLegend, fill=colPal,col=colPal,title="Title (units)",horiz=F,bty="y")
#
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
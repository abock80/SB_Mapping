# XML data parser - This program parses the metadata, looks up variables, tries to match variables with the
# names in the CSV
# R libraries required - sbtools, sp, gdalUtils, RCurl, RColorBrewer, maps, mapdata
# functions are a part of package specificed (sbtools::item_get - item_get is function in sbtools library)
# Functions on line 49-50 need to have the libraries loaded (maps,mapdata) to access the databases 
library(maps)
library(mapdata)
library(XML)

# get the SB item information
# this is a dummy page for Roy's data
#57115024e4b0ef3b7ca554f3
test_item<-sbtools::item_get("57115024e4b0ef3b7ca554f3")
names(test_item)

# get the data
# downloads to a local directory
#item_file_download(test_item,dest_dir="d:/abock/temp")
sbfiles<-sbtools::item_list_files(test_item)
print(sbfiles)
# xmlParse a file if it is an xml file (metadata)
data <- xmlParse(RCurl::getURL(sbfiles$url[grep(".xml",unlist(sbfiles))]),asText=T)
attrInfo<-unlist(xmlElementsByTagName(xmlRoot(data)[["eainfo"]], "attr", recursive = TRUE))

# create a loop to go into attrInfo, pull out attrlabel, def, and units


#**************************DEVELOPMENT CODE
## get the attribute label nodes, these should match what are in the shapefile and excel spreadsheets
#attList<-getNodeSet(data,"//attrlabl")
#unitList<-getNodeSet(data,"//attrunit")

## get list of metadata nodes
#entinfo<-xmlRoot(data)["eainfo"]
#attrInfo<-unlist(xmlElementsByTagName(xmlRoot(data)[[5]], "attr", recursive = TRUE))

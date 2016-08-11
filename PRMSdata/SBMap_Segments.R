setwd("d:/abock/CDI_Mapping/SB_Mapping/PRMSdata")
source("func.R")

#**********************************************************************************************************
# functions are a part of package specificed (sbtools::item_get - item_get is function in sbtools library)

# get the SB item information
# kathy's data on my page - 57114f7be4b0ef3b7ca554e8
# This is the SB id for the CDI Project - 56f419c5e4b0f59b85e0bc5d
# This is the SB id for Kathy's PRMS Project - 5522bdf7e4b027f0aee3d039
# This is the child SB id for Kathy's Streamflow data - 55b69c6ae4b09a3b01b5f653
PRMS_item<-sbtools::item_get("5522bdf7e4b027f0aee3d039")
names(PRMS_item)
# The PRMS streamflow data is a child ("Appendix 2") of Kathy's data
children<-sbtools::item_list_children(PRMS_item)
# We need the ScienceBase Id of Kathy's streamflow da"ta, 
# To do this we extract the "tail" of the URL of the child item
sFlow_SBid<-tail(unlist(strsplit(children[[2]]$link$url,"/")),1)
#Create object for the SFlow data
child_item<-sbtools::item_get(sFlow_SBid)

#********************************************************************************************************
#********************************************************************************************************
# List the streamflow files from Appendix2
sbFiles<-sbtools::item_list_files(child_item)
print(sbFiles$fname)


# find files based on grep of basin name, year, and gcm name
baseLine<-grep(paste(c("1982","BASELINE"),collapse="_"),sbFiles$fname)
# one GCM for all sites
#future<-grep(paste(c("2055","GFDL"),collapse="_"),sbFiles$fname)
# all GCMs for all sites
future_All_2030<-grep(paste(c("2030"),collapse="_"),sbFiles$fname)
future_All_2055<-grep(paste(c("2055"),collapse="_"),sbFiles$fname)
future_All_2080<-grep(paste(c("2080"),collapse="_"),sbFiles$fname)

# Hardcoded wastershed and GCM names for column names
msites<-c("OF","RW","LD","MM","JD","CD","BT")
# only 2 gcms for 2030, 3 for 2055, 2 for 2080
gcms<-c("ECHAM5","GENMON","GFDL")

baseData<-sb2DF(baseLine,msites)
avgBase<-colMeans(baseData)

cNames2030<-unlist(lapply(msites,function(x) paste(x,"_",gcms[1:2],sep="")))
futData2030<-sb2DF(future_All_2030,cNames2030)
Avg2030<-avGCM(futData2030,gcms[1:2],"2030")
dep2030<-sweep(Avg2030,1,avgBase)*100
#MM2030
#Sea2030

cNames2055<-unlist(lapply(msites,function(x) paste(x,"_",gcms,sep="")))
futData2055<-sb2DF(future_All_2055,cNames2055)
Avg2055<-avGCM(futData2055,gcms,"2055")
dep2055<-sweep(Avg2055,1,avgBase)*100
#MM2055
#Sea2055

cNames2080<-unlist(lapply(msites,function(x) paste(x,"_",gcms[1:2],sep="")))
futData2080<-sb2DF(future_All_2080,cNames2080)
Avg2080<-avGCM(futData2080,gcms[1:2],"2080")
dep2080<-sweep(Avg2080,1,avgBase)*100
#MM2070
#Sea2070

AvgAll<-cbind(Avg2030,Avg2055,Avg2080)
depAll<-cbind(dep2030,dep2055,dep2080)

#Get the means and change from baseline for each feature
perChange<-((AvgAll$MEAN_2030-avgBase)/avgBase)*100
#********************************************************************************************************
#********************************************************************************************************
# Might need to make this a reactive function

# Operations to retrieve data for a single segment and multiple GCMs using O'Fallon Creek as an example
#baseLine_OF<-grep(paste(c("OF","1982","BASELINE"),collapse="_"),sbFiles$fname)
baseLine_BT<-baseData[,grep("seg.17_BT",names(baseData))]
#future_OF<-grep(paste(c("OF","2055"),collapse="_"),sbFiles$fname)
future_BT<-futData2030[,grep("seg.17_BT",names(futData2030))]
future_BT$means<-rowMeans(future_BT)
dep_BT<-sweep(future_BT,1,baseLine_BT)*100

# get mean monthly data
FutMM <- aggregate(dep_BT, list(data.table::month(as.Date(rownames(dep_BT)))), mean,na.rm=T)

#*******************************************************************************************
#*******************************************************************************************
# Mapping Components
# WFS for R10U - John "Dell" Long - pySB
# subset of shapefiles on the project page
# separate call to get subset of features from full WFS
# use HRUs instead of segments or incremental contributing areas
# change line segment width by value
layer<-sbtools::item_get_wfs("571559c2e4b0ef3b7ca864c7")
# need to order the basin names and segs like they are in line 41 sbmaps_segments
segMap<-read.csv("d:/abock/CDI_Mapping/SB_Mapping/PRMSdata/Streamsegments_Qchange_Buffer.csv",header=T,row.names=1)
segMap<-segMap[-c(186,187),]
# order by POI_ID
segMap<-segMap[with(segMap,order(POI_ID)),]

# subset GF to just the segments we are intereted in
GF_layer<-layer[which(layer@data$POI_ID %in% segMap$POI_ID),]

#GFsegs_buffer<-rgeos::gBuffer(GF_segs,byid=FALSE,width=100,capStyle="ROUND",joinStyle="ROUND")
finalSegs<-sp::spTransform(GF_layer,"+init=epsg:4326")
# This is the reprojection to WGS84 web mercator
#finalSegs<-sp::spTransform(GF_layer,"+init=epsg:3857")
finalSegs<-finalSegs[with(finalSegs@data,order(POI_ID)),]

# gets the xy points of each line
res <- lapply(slot(finalSegs, "lines"), function(x) lapply(slot(x, "Lines"),
                                                     function(y) slot(y, "coords")))
#
test<-lapply(res, function(x) c(max(unlist(x)),min(unlist(x))))
Lats<-unlist(test)[c(TRUE,FALSE)]
Longs<-unlist(test)[c(FALSE,TRUE)]


#************JUNK*********************************
#************JUNK*********************************
# # retrieve baseline data for all sites
# baseFlow<-lapply(sbFiles$url[baseLine],RCurl::getURL)
# # function to open all files as data frames
# baseAll<-lapply(baseFlow,function(i){
#   read.csv(text=unlist(i),row.names=1)
# })
# # Add column names 
# baseAll<-mapply(colN,baseAll,msites,SIMPLIFY=F)
# # bind the data frames together
# baseData<-dplyr::bind_cols(baseAll)
# # Add timestamps as row names
# row.names(baseData)<-rownames(baseAll[[1]])

# # Perform the same tasks, but for the GCM data
# futFlow<-lapply(sbFiles$url[future],RCurl::getURL)
# futAll<-lapply(futFlow,function(i){
#   read.csv(text=unlist(i),row.names=1)
# })
# futAll<-mapply(colN,futAll,msites,SIMPLIFY=F)
# futData<-dplyr::bind_cols(futAll)
# rownames(futData)<-rownames(futAll[[1]])

## remove segments 32 and 34 from the data frames (MM river)
#baseData<-baseData[,!colnames(baseData) %in% c("seg.32_MM","seg.34_MM")]
#futData<-futData[,!colnames(futData) %in% c("seg.32_MM","seg.34_MM")]
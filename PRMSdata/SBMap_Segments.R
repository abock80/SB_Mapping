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
#*******************************************************************************************
#*******************************************************************************************
# Mapping Components
# WFS for R10U 
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
finalSegs<-finalSegs[order(finalSegs$POI_ID),]
# Add the basin name, code, and segment number
# so that they can be used in Shiny
finalSegs@data$Basin<-segMap$Basin
finalSegs@data$Code<-segMap$Code
finalSegs@data$Nseg<-segMap$Nseg
finalSegs<-finalSegs[order(finalSegs$Nseg),]

# gets the xy points of each line
res <- lapply(slot(finalSegs, "lines"), function(x) lapply(slot(x, "Lines"),
                                                           function(y) slot(y, "coords")))
test<-lapply(res, function(x) c(max(unlist(x)),min(unlist(x))))
Lats<-unlist(test)[c(TRUE,FALSE)]
Longs<-unlist(test)[c(FALSE,TRUE)]
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
#msites<-c("OF","BT","JD","CD","MM","LD","RW")
# only 2 gcms for 2030, 3 for 2055, 2 for 2080
gcms<-c("ECHAM5","GENMON","GFDL")

baseData<-sb2DF(baseLine,msites)
avgBase<-colMeans(baseData)

#cNames2030<-unlist(lapply(msites,function(x) paste(x,"_",gcms[1:2],sep="")))
cNames2030<-unlist(lapply(msites,function(x) paste(gcms[1:2],"_",x,sep="")))
futData2030<-sb2DF(future_All_2030,cNames2030)
Avg2030<-avGCM(futData2030,gcms[1:2],"2030")
#dep2030<-sweep(Avg2030,1,avgBase)*100
dep2030<-(sweep(Avg2030,1,avgBase)/avgBase)*100

#cNames2055<-unlist(lapply(msites,function(x) paste(x,"_",gcms,sep="")))
cNames2055<-unlist(lapply(msites,function(x) paste(gcms,"_",x,sep="")))
futData2055<-sb2DF(future_All_2055,cNames2055)
Avg2055<-avGCM(futData2055,gcms,"2055")
#dep2055<-sweep(Avg2055,1,avgBase)*100
dep2055<-(sweep(Avg2055,1,avgBase)/avgBase)*100

#cNames2080<-unlist(lapply(msites,function(x) paste(x,"_",gcms[1:2],sep="")))
cNames2080<-unlist(lapply(msites,function(x) paste(gcms[1:2],"_",x,sep="")))
futData2080<-sb2DF(future_All_2080,cNames2080)
Avg2080<-avGCM(futData2080,gcms[1:2],"2080")
#dep2080<-sweep(Avg2080,1,avgBase)*100
dep2080<-(sweep(Avg2080,1,avgBase)/avgBase)*100

depAll<<-cbind(dep2030,dep2055,dep2080)
rownames(depAll)<-colnames(baseData)
#depAll<-depAll[order(row.names(depAll)),]

#********************************************************************************************************
#********************************************************************************************************
# Seasonal aggregation
baseSeas<-t(seasonalMeans_Base(baseData))
Seas2030<-seasonalMeans(futData2030,baseSeas)
depS2030<-avGCM_Seas(Seas2030,gcms[1:2],2030)
Seas2055<-seasonalMeans(futData2055,baseSeas)
depS2055<-avGCM_Seas(Seas2055,gcms,2055)
Seas2080<-seasonalMeans(futData2080,baseSeas)
depS2080<-avGCM_Seas(Seas2080,gcms[1:2],2080)
depSeason<<-cbind(depS2030,depS2055,depS2080)


## Mean monthly aggregation
baseData$date<-rownames(baseData)
baseData_zoo<-zoo::read.zoo(baseData,index.column=186)
baseData_MM<-t(aggregate(baseData_zoo,function(x) cycle(zoo::as.yearmon(x)),na.rm=T))
MM2030<-t(MeanMonthly(futData2030,baseData_MM))
depMM2030<-avGCM_MM(MM2030,gcms[1:2],2030)
MM2055<-t(MeanMonthly(futData2055,baseData_MM))
depMM2055<-avGCM_MM(MM2055,gcms,2055)
MM2080<-t(MeanMonthly(futData2080,baseData_MM))
depMM2080<-avGCM_MM(MM2080,gcms[1:2],2080)

depMM<<-cbind(depMM2030,depMM2055,depMM2080)

dFrame<<-cbind(depAll,depSeason,depMM)

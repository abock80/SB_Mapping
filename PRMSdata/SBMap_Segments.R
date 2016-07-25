# function to add the column names for each site or the GCM names to the dataframe
colN<-function(DF,site){
  print(site)
  colnames(DF)<-paste(colnames(DF),"_",site,sep="")
  return (DF)
}

# current function for seasonal means
# need to test out
seasonalMeans<-function(DF){
  TS<-xts::as.xts(DF)
  print(TS)
  win<-TS[.indexmon(TS) %in% c(1,2,3)]
  spr<-colMeans(TS[.indexmon(TS) %in% c(4,5,6)])
  sum<-colMeans(TS[.indexmon(TS) %in% c(7,8,9)])
  fall<-colMeans(TS[.indexmon(DF) %in% c(10,11,12)])
  seas<-cbind(win,spr,sum,fall)
  colnames(seas)<-c("win","spr","sum","fall")
  return(seas)
}

print("yowzer!!")
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
future<-grep(paste(c("2055","GFDL"),collapse="_"),sbFiles$fname)
# all GCMs for all sites
future_All<-grep(paste(c("2055"),collapse="_"),sbFiles$fname)

# Hardcoded wastershed and GCM names for column names
msites<-c("OF","RW","LD","MM","JD","CD","BT")
gcms<-c("ECHAM5","GENMON","GFDL")

# retrieve baseline data for all sites
baseFlow<-lapply(sbFiles$url[baseLine],RCurl::getURL)
# function to open all files as data frames
baseAll<-lapply(baseFlow,function(i){
  read.csv(text=unlist(i),row.names=1)
})
# Add column names 
baseAll<-mapply(colN,baseAll,msites,SIMPLIFY=F)
# bind the data frames together
baseData<-dplyr::bind_cols(baseAll)
# Add timestamps as row names
row.names(baseData)<-rownames(baseAll[[1]])

# Perform the same tasks, but for the GCM data
futFlow<-lapply(sbFiles$url[future],RCurl::getURL)
futAll<-lapply(futFlow,function(i){
  read.csv(text=unlist(i),row.names=1)
})
futAll<-mapply(colN,futAll,msites,c(1:7),SIMPLIFY=F)
futData<-dplyr::bind_cols(futAll)
rownames(futData)<-rownames(futAll[[1]])

# remove segments 32 and 34 from the data frames (MM river)
baseData<-baseData[,!colnames(baseData) %in% c("seg.32_MM","seg.34_MM")]
futData<-futData[,!colnames(futData) %in% c("seg.32_MM","seg.34_MM")]

#Get the means and change from baseline for each feature
baseMeans<-colMeans(baseData)
futMeans<-colMeans(futData)
perChange<-((futData-baseData)/baseData)*100
#********************************************************************************************************
#********************************************************************************************************
# Operations to retrieve data for a single segment and multiple GCMs using O'Fallon Creek as an example
baseLine_OF<-grep(paste(c("OF","1982","BASELINE"),collapse="_"),sbFiles$fname)
future_OF<-grep(paste(c("OF","2055"),collapse="_"),sbFiles$fname)

# retrieve baseflow data for all sites
baseOF <- RCurl::getURL(sbFiles$url[baseLine_OF])
baseOF2 <- read.csv(text=baseOF)
names(baseOF2)

futOF<-lapply(sbFiles$url[future_OF],RCurl::getURL)
futAllOF<-lapply(futOF,function(i){
  read.csv(text=unlist(i),row.names=1)
})
sNames<-unique(colnames(futAllOF[[1]]))
futAllOF2<-mapply(colN,futAllOF,gcms,c(1:3),SIMPLIFY=F)
futDataOF<-dplyr::bind_cols(futAllOF2)
futDataOF$date<-rownames(futAllOF[[1]])

# get mean monthly data into a zoo series
baseZoo<-zoo::read.zoo(baseOF2,index.column=1,format="%Y-%m-%d")
futZoo<-zoo::read.zoo(futDataOF,index.column=length(colnames(futDataOF)),format="%Y-%m-%d")

futAll<-mapply(colN,futAll,msites,c(1:7),SIMPLIFY=F)

# get mean monthly data
options(warn=-1)
FutMM <- aggregate(futDataOF, list(data.table::month(as.Date(futDataOF$date))), mean,na.rm=T)
baseMM <- aggregate(baseOF2, list(data.table::month(as.Date(futDataOF$date))), mean, na.rm=T)
sCols<-FutMM[grep("seg",names(FutMM),value=TRUE)]
fut_byMonth<-data.frame(matrix(unlist(lapply(sNames,function(x) rowMeans(FutMM[grep(paste(x,"_",sep=""),names(FutMM),value=TRUE)]))),
                               nrow=12,byrow=F))
options(warn=0)
#*******************************************************************************************
#*******************************************************************************************
# Mapping Components
# WFS for R10U
layer<-sbtools::item_get_wfs("571559c2e4b0ef3b7ca864c7")
# need to order the basin names and segs like they are in line 41 sbmaps_segments
segMap<-read.csv("d:/abock/CDI_Mapping/SB_Mapping/PRMSdata/Streamsegments_Qchange_Buffer.csv",header=T,row.names=1)
segMap<-segMap[-c(186,187),]
# order by POI_ID
segMap<-segMap[with(segMap,order(POI_ID)),]

#GFsegs_buffer<-rgeos::gBuffer(GF_segs,byid=FALSE,width=100,capStyle="ROUND",joinStyle="ROUND")
finalSegs<-sp::spTransform(layer,"+init=epsg:4326")
finalSegs<-finalSegs[with(finalSegs@data,order(POI_ID)),]
#Need to join segments to sites, and order them
finalSegs_joined<-dplyr::inner_join(finalSegs@data,segMap,by="POI_ID")

#Need to join segments to sites, and order them




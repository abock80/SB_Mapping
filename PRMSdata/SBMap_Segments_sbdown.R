# this is a function to add the column names for each site to the dataframe
colN<-function(DF,site,num){
  print(num)
  print(site)
  colnames(DF)<-paste(colnames(DF),"_",site,sep="")
  return (DF)
}



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

test<-seasonalMeans(baseOF2)

# get seasonal data
baseTS<-as.matrix(xts::as.xts(baseOF2))
win<-colMeans(as.numeric(baseTS[.indexmon(baseTS) %in% c(1,2,3)]))
spr<-colMeans(baseTS[.indexmon(baseTS) %in% c(4,5,6)])
sum<-colMeans(baseTS[.indexmon(baseTS) %in% c(7,8,9)])
fall<-colMeans(baseTS[.indexmon(baseTS) %in% c(10,11,12)])


# if sb is down for maintanence do this:
setwd("d:/abock/CDI_Mapping/PRMS_Data")
sbFiles<-list.files("Data")

# find files based on grep of basin name, year, and gcm name
baseLine<-grep(paste(c("1982","BASELINE"),collapse="_"),sbFiles)
future<-grep(paste(c("2055","GFDL"),collapse="_"),sbFiles)
future_All<-grep(paste(c("2055"),collapse="_"),sbFiles)

# order to stich together files
# - O'Fallon (OF), Redwater River (RW), Little Dry Creek (LD), Middle Musselshell (MM),
# Judith (JD), Cottonwood Creek (CD), Belt Creek (BT)
msites<-c("OF","RW","LD","MM","JD","CD","BT")
gcms<-c("ECHAM5","GENMON","GFDL")

# retrieve baseline data for all sites
baseAll<-lapply(sbFiles[baseLine],function(i){
  read.csv(paste("Data/",(i),sep=""),row.names=1)
})

# this exeuctes the function
baseAll<-mapply(colN,baseAll,msites,c(1:7),SIMPLIFY=F)
baseData<-dplyr::bind_cols(baseAll)
row.names(baseData)<-rownames(baseAll[[1]])

# retrieve future flow data for all sites
futAll<-lapply(sbFiles[future],function(i){
  read.csv(paste("Data/",(i),sep=""),row.names=1)
})
futAll<-mapply(colN,futAll,msites,c(1:7),SIMPLIFY=F)
futData<-dplyr::bind_cols(futAll)
rownames(futData)<-rownames(futAll[[1]])

# remove segments 32 and 34 from the map (MM river)
baseData<-baseData[,!colnames(baseData) %in% c("seg.32_MM","seg.34_MM")]
futData<-futData[,!colnames(futData) %in% c("seg.32_MM","seg.34_MM")]

#Means
baseMeans<-colMeans(baseData)
futMeans<-colMeans(futData)
sfMeans<-head(baseMeans-futMeans)
#*****************************************************************************
# Operations for a single segment and multiple GCMs using O'Fallon Creek as an example
baseLine_OF<-grep(paste(c("OF","1982","BASELINE"),collapse="_"),sbFiles)
future_OF<-grep(paste(c("OF","2055"),collapse="_"),sbFiles)

# retrieve baseflow data for all sites
baseOF2 <- read.csv(paste("Data/",sbFiles[baseLine_OF],sep=""),row.names=1)
baseOF2$date<-rownames(baseOF2)
names(baseOF2)

futAllOF<-lapply(sbFiles[future_OF],function(i){
  read.csv(paste("Data/",(i),sep=""),row.names=1)
})
sNames<-unique(colnames(futAllOF[[1]]))
# look to use the .id argument for bind_cols to identity GCM's with their specific columns
futAllOF2<-mapply(colN,futAllOF,gcms,c(1:3),SIMPLIFY=F)
futDataOF<-dplyr::bind_cols(futAllOF2)
futDataOF$date<-rownames(futAllOF[[1]])

baseZoo<-zoo::read.zoo(baseOF2,index.column=length(colnames(baseOF2)),format="%Y-%m-%d")
futZoo<-zoo::read.zoo(futDataOF,index.column=length(colnames(futDataOF)),format="%Y-%m-%d")
testerino<-sapply(baseZoo,function(x) aggregate(x,as.yearqtr,mean))


hoho<-xts::as.xts(baseOF2)
win<-hoho[.indexmon(hoho) %in% c(1,2,3)]
spr<-hoho[.indexmon(hoho) %in% c(4,5,6)]
sum<-hoho[.indexmon(hoho) %in% c(7,8,9)]
fall<-hoho[.indexmon(hoho) %in% c(10,11,12)]

# get mean monthly data
options(warn=-1)
FutMM <- aggregate(futDataOF, list(data.table::month(as.Date(futDataOF$date))), mean,na.rm=T)
baseMM <- aggregate(baseOF2, list(data.table::month(as.Date(futDataOF$date))), mean, na.rm=T)
sCols<-FutMM[grep("seg",names(FutMM),value=TRUE)]
fut_byMonth<-data.frame(matrix(unlist(lapply(sNames,function(x) rowMeans(FutMM[grep(paste(x,"_",sep=""),names(FutMM),value=TRUE)]))),
                               nrow=12,byrow=F))
options(warn=0)



#******************************************
GF_segs<-rgdal::readOGR("d:/abock/CDI_Mapping/PRMS_Data","nsegment")
# need to order the basin names and segs like they are in line 41 sbmaps_segments
segMap<-read.csv("d:/abock/CDI_Mapping/SB_Mapping/PRMSdata/Streamsegments_Qchange_Buffer.csv",header=T,row.names=1)
segIDs<-head(segMap$POI_ID,-2)
# remove dummy rows
segMap<-segMap[-c(186,187),]
# order by POI_ID
segMap<-segMap[with(segMap,order(POI_ID)),]

#GFsegs_buffer<-rgeos::gBuffer(GF_segs,byid=FALSE,width=100,capStyle="ROUND",joinStyle="ROUND")
finalSegs<-sp::spTransform(GF_segs,"+init=epsg:4326")
finalSegs<-finalSegs[with(finalSegs@data,order(POI_ID)),]
#Need to join segments to sites, and order them
finalSegs_joined<-dplyr::inner_join(finalSegs@data,segMap,by="POI_ID")

# idea, dissolve all segs by name, create Wshed title??
# but where to get the wshed boundaries
# or better get hrus that contribute to segments and dissolve into one wshed



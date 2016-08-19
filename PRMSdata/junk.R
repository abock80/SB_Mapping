test1<-MM2030[,grep("ECHAM5",colnames(MM2030))]



yo<-Seas2030[grepl("OF_ECHAM5",rownames(Seas2030)),]




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

## Mean monthly aggregation
#futData2030$date<-rownames(baseData)
#futData2030_zoo<-zoo::read.zoo(futData2030,index.column=371)
#futData2030_MM<-t(aggregate(test2,function(x) cycle(zoo::as.yearmon(x)),na.rm=T))


#trymeout<-do.call(rbind,replicate((dim(MM2030)[1]/dim(baseData_MM)[1]),baseData_MM,simplify=F))

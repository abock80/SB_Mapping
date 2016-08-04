# Produce Dataframe from sbfiles
sb2DF<-function(fileList,col_Names){
  Flow<-lapply(sbFiles$url[fileList],RCurl::getURL)
  # function to open all files as data frames
  All<-lapply(Flow,function(i){
    read.csv(text=unlist(i),row.names=1)
  })
  # Add column names 
  All<-mapply(colN,All,col_Names,SIMPLIFY=F)
  print(colnames(All))
  # bind the data frames together
  bData<-dplyr::bind_cols(All)
  # Add timestamps as row names
  row.names(bData)<-rownames(All[[1]])
  # remove columns for seg32 and 34 of the MM river
  bData<-bData[,-grep(paste(c("seg.32_MM","seg.34_MM"),collapse="|"),colnames(bData))]
  return(bData)
}

# function to add the column names for each site or the GCM names to the dataframe
colN<-function(DF,site){
  print(site)
  colnames(DF)<-paste(colnames(DF),"_",site,sep="")
  return (DF)
}

# function to average GCMS
avGCM<-function(DF,gcms,yr){
  gcmList<-lapply(gcms,function(x) DF[,grep(x,colnames(DF))])
  #gcmList<-lapply(gcms[1:2],function(x) futData2030[,grep(x,colnames(futData2030))])
  mean.dat <- data.frame(matrix(unlist(lapply(gcmList, function (x) lapply(x, mean, na.rm=TRUE))),nrow=185,byrow=T))
  mean.dat<-cbind(mean.dat,rowMeans(mean.dat))
  colnames(mean.dat)<-paste(c(gcms,"MEAN"),yr,sep="_")
  return(mean.dat)
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
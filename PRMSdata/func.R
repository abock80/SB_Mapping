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
  mean.dat <- data.frame(matrix(unlist(lapply(gcmList, function (x) lapply(x, mean, na.rm=TRUE))),nrow=185,byrow=F))
  colnames(mean.dat)<-paste(gcms,"ann",yr,sep="_")
  mean.dat<-cbind(mean.dat,rowMeans(mean.dat))
  colnames(mean.dat)<-paste(c(gcms,"Mean"),"ann",yr,sep="_")
  return(mean.dat)
}

# current function for seasonal means
# need to test out
seasonalMeans_Base<-function(DF){
  TS<-xts::as.xts(DF)
  #print(TS)
  win<-colMeans(DF[xts::.indexmon(TS) %in% c(1,2,3),])
  spr<-colMeans(DF[xts::.indexmon(TS) %in% c(4,5,6),])
  sum<-colMeans(DF[xts::.indexmon(TS) %in% c(7,8,9),])
  fall<-colMeans(DF[xts::.indexmon(TS) %in% c(10,11,12),])
  seas<-rbind(win,spr,sum,fall)
  rownames(seas)<-c("win","spr","sum","fall")
  return(seas)
}

# current function for seasonal means
# need to test out
seasonalMeans<-function(DF,DFCC){
  TS<-xts::as.xts(DF)
  #print(TS)
  win<-colMeans(DF[xts::.indexmon(TS) %in% c(1,2,3),])
  winBase<-rep(DFCC[,"win"],length(win)/length(DFCC[,"win"]))
  winDep<-(win-winBase)*100
  
  spr<-colMeans(DF[xts::.indexmon(TS) %in% c(4,5,6),])
  sprBase<-rep(DFCC[,"spr"],length(win)/length(DFCC[,"spr"]))
  sprDep<-(spr-sprBase)*100
  
  sum<-colMeans(DF[xts::.indexmon(TS) %in% c(7,8,9),])
  sumBase<-rep(DFCC[,"sum"],length(win)/length(DFCC[,"sum"]))
  sumDep<-(sum-sumBase)*100
  
  fall<-colMeans(DF[xts::.indexmon(TS) %in% c(10,11,12),])
  fallBase<-rep(DFCC[,"fall"],length(win)/length(DFCC[,"fall"]))
  fallDep<-(fall-fallBase)*100
  
  seas<-rbind(winDep,sprDep,sumDep,fallDep)
  rownames(seas)<-c("win","spr","sum","fall")
  return(seas)
}

# function to average GCMS
avGCM_Seas<-function(DF,gcms,yr){
  seas<-c("win","spr","sum","fall")
  gcmList<-lapply(gcms,function(x) DF[,grep(x,colnames(DF))])
  #gcmList<-lapply(gcms[1:2],function(x) futData2030[,grep(x,colnames(futData2030))])
  #mean.dat <- data.frame(matrix(unlist(lapply(gcmList, function (x) lapply(x, mean, na.rm=TRUE))),nrow=185,byrow=T))
  mean.dat<-dplyr::bind_cols(lapply(gcmList,function(x) data.frame(matrix(unlist(x),nrow=185,byrow=T))))
  colnames(mean.dat)<-unlist(lapply(gcms,paste,seas,yr,sep="_"))
  gcmMeans<-data.frame(matrix(unlist(lapply(seas,function(x) rowMeans(mean.dat[,grepl(x,colnames(mean.dat))]))),nrow=185,byrow=T))
  colnames(gcmMeans)<-paste("Mean",seas,yr,sep="_")
  mean.dat<-cbind(mean.dat,gcmMeans)
  return(mean.dat)
}

MeanMonthly<-function(DF,DFCC){
  DF$date<-rownames(DF)
  DF_zoo<-zoo::read.zoo(DF,index.column=dim(DF)[2])
  DF_MM<-t(aggregate(DF_zoo,function(x) cycle(zoo::as.yearmon(x)),na.rm=T))
  Base_MM<-do.call(rbind,replicate((dim(DF_MM)[1]/dim(DFCC)[1]),DFCC,simplify=F))
  print(dim(Base_MM))
  print(dim(DF_MM))
  Dep_MM<-(DF_MM-Base_MM)*100
  Dep_MM<-Dep_MM[,order(as.numeric(colnames(Dep_MM)))]
  print(colnames(Dep_MM))
  return(Dep_MM)
}

# function to average GCMS
avGCM_MM<-function(DF,gcms,yr){
  mths<-c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  gcmList<-lapply(gcms,function(x) DF[,grep(x,colnames(DF))])
  #mean.dat <- data.frame(matrix(unlist(lapply(gcmList, function (x) lapply(x, mean, na.rm=TRUE))),nrow=185,byrow=T))
  mean.dat<-dplyr::bind_cols(lapply(gcmList,function(x) data.frame(matrix(unlist(x),nrow=185,byrow=T))))
  colnames(mean.dat)<-unlist(lapply(gcms,paste,mths,yr,sep="_"))
  gcmMeans<-data.frame(matrix(unlist(lapply(mths,function(x) rowMeans(mean.dat[,grepl(x,colnames(mean.dat))]))),nrow=185,byrow=T))
  mean.dat<-cbind(mean.dat,gcmMeans)
  colnames(mean.dat)<-unlist(lapply(c(gcms,"Mean"),paste,mths,yr,sep="_"))
  return(mean.dat)
}

# Produce Dataframe from sbfiles
sb2DF<-function(fileList,col_Names){
  print(fileList)
  print(col_Names)
  Flow<-lapply(sbFiles$url[fileList],RCurl::getURL)
  # function to open all files as data frames
  All<-lapply(Flow,function(i){
    read.csv(text=unlist(i),row.names=1)
  })
  # Add column names 
  All<-mapply(colN,All,col_Names,SIMPLIFY=F)
  print(colnames(All))
  print(class(All))
  # bind the data frames together
  #bData<-dplyr::bind_cols(All)
  #print(dim(bData))
  #print(class(bData))
  ## Add timestamps as row names
  #row.names(bData)<-rownames(All[[1]])
  #print(rownames(bData))
  #print(colnames(bData))
  # remove columns for seg32 and 34 of the MM river
  #bData<-bData[,-grep(paste(c("seg.32_MM","seg.34_MM"),collapse="|"),colnames(bData))]
  return(All)
}

# dplyr version problems
baseData<-sb2DF(baseLine,msites)
hoho<-do.call("rbind",baseData)

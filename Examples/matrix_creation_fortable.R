values2<-values[,grep("GFDL",colnames(values))]

v1<-values2[,grep("2030",colnames(values2))]
v2<-values2[,grep("2055",colnames(values2))]
v3<-values2[,grep("2080",colnames(values2))]
if (length(v1)>0){
  test<-t(mapply(c,v1,v2,v3))
}else{
  test<-matrix(v2,nrow=17)
}

test<-matrix(v2,nrow=17)

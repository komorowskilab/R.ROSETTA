rosResults<-function(path, roc){
  
  statsTab=read.table(paste0(path,"/logMain.txt"),fill=T)
  if(roc)
  {
  stats=statsTab[((dim(statsTab)[1]-16):dim(statsTab)[1]),]
  }else{
  stats=statsTab[((dim(statsTab)[1]-4):dim(statsTab)[1]),]
  }
  
  
  stats2=as.data.frame(as.matrix(stats)[,c(1,3)])
  colnames(stats2)<-c("Measure","Value")
  
  ##MCC
  cts<-statsTab[which(statsTab$V2=="|"),]
  if(length(which(cts$V1=="Undefined"))!=0){
  cts<-cts[-which(cts$V1=="Undefined"),]
  }
  noc<-unique(as.numeric(as.character(cts$V1)))

  MCC=c()

for(i in seq(1, dim(cts)[1], by = length(noc))){
if(length(noc) == 2){
  
  TP=as.numeric(as.character(cts$V3[i]))
  FN=as.numeric(as.character(cts$V3[i+1]))
  FP=as.numeric(as.character(cts$V4[i]))
  TN=as.numeric(as.character(cts$V4[i+1]))
  MCC[i]<-((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
    
}else{
  
  MCC=NaN
  ##in progress##
  #TP=cts$V3[i]
  #FN=cts$V3[i+1]
  #FP=cts$V4[i]
  #TN=cts$V4[i+1]
  
}
  statsMCC=data.frame("MCC.mean",mean(MCC, na.rm=TRUE))
  colnames(statsMCC)<-c("Measure","Value")
}
  
  stats2=rbind(stats2,statsMCC)
  
  
  
  return(stats2)

}

rosResults<-function(path, roc){
  
  statsTab=read.table(paste0(path,"/logMain.txt"),fill=T)
  if(roc)
  {
  stats=statsTab[((dim(statsTab)[1]-16):dim(statsTab)[1]),]
  }else{
  stats=statsTab[((dim(statsTab)[1]-4):dim(statsTab)[1]),]
  }
  stats2=as.data.frame(as.matrix(stats)[,c(1,3)])
  colnames(stats2)=c("Measure","Value")
  return(stats2)

}
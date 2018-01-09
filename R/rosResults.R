rosResults<-function(path){
  
  statsTab=read.table(paste0(path,"/logMain.txt"),fill=T)
  stats=statsTab[((dim(statsTab)[1]-11):dim(statsTab)[1]),]
  stats2=as.data.frame(as.matrix(stats)[,c(1,3)])
  colnames(stats2)=c("Measure","Value")
  return(stats2)

}
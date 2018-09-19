dfToCsv <- function(df, fname="data", tempDirNam, disc, firstRow=T)
{
  
  if(disc){
  
  vecHead=unname(sapply(df, class))
  vecHead[vecHead=="numeric"]<-"Float(6)"
  vecHead[vecHead=="integer"]<-"Integer"
  vecHead[vecHead=="logical"]<-"String"
  vecHead[vecHead=="factor"]<-"String"
  vecHead[vecHead=="character"]<-"String"
  fRow=vecHead
  
  }else
  {
  vecHead=unname(sapply(df, class))
  vecHead[vecHead=="numeric"]<-"Integer"
  vecHead[vecHead=="integer"]<-"Integer"
  vecHead[vecHead=="logical"]<-"String"
  vecHead[vecHead=="factor"]<-"String"
  vecHead[vecHead=="character"]<-"String"
  fRow=vecHead
  }
  
  fRow=data.frame(t(fRow))
  colnames(fRow)<-colnames(df)
  dfc<-data.frame(trimws(as.matrix(format(df, digits=6))))
  #OUTPUT_DT_TRAIN=rbind(dfc,fRow)
  #OUTPUT_DT_TRAIN=rbind(OUTPUT_DT_TRAIN[dim(OUTPUT_DT_TRAIN)[1],],OUTPUT_DT_TRAIN[-dim(OUTPUT_DT_TRAIN)[1],])
  
  #write.csv(OUTPUT_DT_TRAIN,file=paste0(tempDirNam,"/data/",fname,".csv"),quote = F, row.names = F)
  
    if(.Platform$OS.type=="unix")
  {
  write.table(fRow,file=paste0(tempDirNam,"/data/",fname,".csv"),sep="\t",quote = F, row.names = F)
  write.table(dfc,file=paste0(tempDirNam,"/data/",fname,".csv"),sep="\t",quote = F,col.names = F, row.names = F, append = TRUE)}else{
  write.table(fRow,file=paste0(tempDirNam,"\\data\\",fname,".csv"),sep="\t",quote = F, row.names = F)
  write.table(dfc,file=paste0(tempDirNam,"\\data\\",fname,".csv"),sep="\t",quote = F,col.names = F, row.names = F, append = TRUE)  
      }
  #write.table(fRow,file=paste0(tempDirNam,"/data/",fname,".csv"),sep=",",quote = F, row.names = F)
  
  #write.table(dfc,file=paste0(tempDirNam,"/data/",fname,".csv"),sep=",",quote = F,col.names = F, row.names = F, append = TRUE)
  
  }

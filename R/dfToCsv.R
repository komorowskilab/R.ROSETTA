dfToCsv <- function(df, fname="data", tempDirNam, disc, firstRow=T)
{
  
  if(disc){
  fRow=c(rep("Float(6)",dim(df)[2]-1),"String")
  }else
  {
  fRow=c(rep("Integer",dim(df)[2]-1),"String")
  }
  fRow=data.frame(t(fRow))
  colnames(fRow)<-colnames(df)
  OUTPUT_DT_TRAIN=rbind(df,fRow)
  OUTPUT_DT_TRAIN=rbind(OUTPUT_DT_TRAIN[dim(OUTPUT_DT_TRAIN)[1],],OUTPUT_DT_TRAIN[-dim(OUTPUT_DT_TRAIN)[1],])

  write.table(fRow,file=paste0(tempDirNam,"/data/",fname,".csv"),sep="\t",quote = F, row.names = F)
  write.table(format(df, digits=6),file=paste0(tempDirNam,"/data/",fname,".csv"),sep="\t",quote = F,col.names = F, row.names = F, append = TRUE)
  
  }
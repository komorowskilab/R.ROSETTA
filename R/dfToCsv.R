dfToCsv <- function(df, fname="data", tempDirNam, disc)
{
  
  if(disc == FALSE){
  vecHead <- unname(sapply(df, class))
  vecHead[vecHead=="numeric"]<-"Float(4)"
  vecHead[vecHead=="integer"]<-"Integer"
  vecHead[vecHead=="logical"]<-"String"
  vecHead[vecHead=="factor"]<-"String"
  vecHead[vecHead=="character"]<-"String"
  fRow <- vecHead
  
  }else{
  vecHead <- unname(sapply(df, class))
  vecHead[vecHead=="numeric"] <- "Integer"
  vecHead[vecHead=="integer"] <- "Integer"
  vecHead[vecHead=="logical"] <- "String"
  vecHead[vecHead=="factor"] <- "String"
  vecHead[vecHead=="character"] <- "String"
  fRow <- vecHead
  }
  
  fRow <- data.frame(t(fRow))
  
  df[,which(vecHead=="Float(4)")] <- round(df[,which(vecHead=="Float(4)")], digits = 4)
  
  cn <- data.frame(t(colnames(df)))
  newDF <- as.data.frame(apply(format(df, digits = 4, justify = "none"), 2, function(x)gsub('\\s+', '',x)))
  
  
  #dfc[,which(vecHead=="Float(6)")] <- trimws(as.matrix(format(round(df[,which(vecHead=="Float(6)")], digits = 6), nsmall = 6)))
  df[,which(vecHead=="Integer")] <- trimws(df[,which(vecHead=="Integer")])
  df[,which(vecHead=="String")] <- trimws(df[,which(vecHead=="String")])
  

    if(.Platform$OS.type=="unix")
  {
  #write.table(fRow,file=paste0(tempDirNam,"/data/",fname,".csv"), sep=",", row.names=F, quote=F, na="")
  #write.table(dfc,file=paste0(tempDirNam,"/data/",fname,".csv"), sep=",", row.names=F, col.names = F, quote=F, na="", append = TRUE)
  #write.csv(dfAll,file=paste0(tempDirNam,"/data/",fname,".csv"), row.names=F, quote=F, na="")
      
      write.table(cn, paste0(tempDirNam,"/data/",fname,".csv"), na="Undefined", sep="\t", row.names=F, append=FALSE, quote=FALSE, col.names=FALSE)
      write.table(fRow, paste0(tempDirNam,"/data/",fname,".csv"), na="Undefined", sep="\t", row.names=F, append=TRUE, quote=FALSE, col.names=FALSE)
      write.table(newDF, paste0(tempDirNam,"/data/",fname,".csv"), na="Undefined", sep="\t", row.names=F, append=TRUE, quote=FALSE, col.names=FALSE)
      
      
  }else{
  #write.table(fRow,file=paste0(tempDirNam,"\\data\\",fname,".csv"),sep="\t",quote = F, row.names = F)
  #write.table(dfc,file=paste0(tempDirNam,"\\data\\",fname,".csv"),sep="\t",quote = F,col.names = F, row.names = F, append = TRUE)  
      write.table(cn, paste0(tempDirNam,"\\data\\",fname,".csv"), na="Undefined", sep="\t", row.names=F, append=FALSE, quote=FALSE, col.names=FALSE)
      write.table(fRow, paste0(tempDirNam,"\\data\\",fname,".csv"), na="Undefined", sep="\t", row.names=F, append=TRUE, quote=FALSE, col.names=FALSE)
      write.table(newDF, paste0(tempDirNam,"\\data\\",fname,".csv"), na="Undefined", sep="\t", row.names=F, append=TRUE, quote=FALSE, col.names=FALSE)
          
    
    }

  }

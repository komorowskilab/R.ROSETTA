maskAttribute <- function(attributeVec, filePath)
{
  beg="begin"
  nd="end"
  nodes="nodes * Undefined"
  mc="make-complement"
  mat=data.frame()
  

  for(i in 1:length(attributeVec)){

  line1=paste(beg,attributeVec[i],sep=" ")
  line2=paste(" ",nodes,sep="")
  line3=paste(" ",mc,sep="")
  line4=paste(nd,attributeVec[i],sep=" ")
  line5=""
  
  mat=rbind(mat,data.frame(line1))
  mat=rbind(mat,setNames(data.frame(line2), names(data.frame(line1))))
  mat=rbind(mat,setNames(data.frame(line3), names(data.frame(line1))))
  mat=rbind(mat,setNames(data.frame(line4), names(data.frame(line1))))
  mat=rbind(mat,setNames(data.frame(line5), names(data.frame(line1))))
  }
  
  write.table(mat,paste0(filePath,"/maIDG.txt"),row.names = F,col.names = F,quote = F)
}

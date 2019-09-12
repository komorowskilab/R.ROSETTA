saveLineByLine <- function(rules, path, discrete=FALSE, filterByPval=FALSE, pval=0.05)
{
  if(discrete)
  {
    vec<-as.character(as.matrix(rules["features"]))
    lst1<-sapply(vec, function(x) strsplit(x, ","))
    vec2<-as.character(as.matrix(rules["levels"]))
    lst2<-sapply(vec2, function(x) strsplit(x, ","))
    newLst<-mapply(paste,collapse=",",sep="=",lst1,lst2)
    lst5<-as.character(unname(newLst))
    if(filterByPval){
      dflbl<-cbind(lst5,rules["decision"],rules["accuracyRHS"],rules["supportRHS"],row.names = NULL)[which(rules["pValue"]<pval),]
    }else
    {
      dflbl<-cbind(lst5,rules["decision"],rules["accuracyRHS"],rules["supportRHS"],row.names = NULL)
    }
  }
  else{
    
lst33<-as.character(as.matrix(rules$levels))
vec<-as.character(as.matrix(rules$features))
lst1<-sapply(vec, function(x) strsplit(x, ","))
lst2<-strsplit(unlist(lst33), ",")
newLst<-mapply(paste,collapse=",",sep="=",lst1,lst2)
lst5<-as.character(unname(newLst))
            
      if(filterByPval){
        dflbl<-cbind(lst5,rules["decision"],rules["accuracyRHS"],rules["supportRHS"],rules["coverageRHS"],row.names = NULL)[which(rules["pValue"]<pval),]
      }else
      {
        dflbl<-cbind(lst5,rules["decision"],rules["accuracyRHS"],rules["supportRHS"],rules["coverageRHS"],row.names = NULL)
      }
  }
  write.table(dflbl, path, col.names = F, row.names = F, sep="\t", quote = F)
}

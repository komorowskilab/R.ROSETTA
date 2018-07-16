saveLineByLine <- function(rules, path, discrete=FALSE, filterByPval=FALSE, pval=0.05)
  
{
  
  if(discrete)
  {
    vec=as.character(as.matrix(rules["FEATURES"]))
    lst1=sapply(vec, function(x) strsplit(x, ","))
    vec2=as.character(as.matrix(rules["CUTS_COND"]))
    lst2=sapply(vec2, function(x) strsplit(x, ","))
    newLst=mapply(paste,collapse=",",sep="=",lst1,lst2)
    lst5=as.character(unname(newLst))
    if(filterByPval){
      dflbl=cbind(lst5,rules["DECISION"],rules["ACC_RHS"],rules["SUPP_RHS"],row.names = NULL)[which(rules["PVAL"]<pval),]
    }else
    {
      dflbl=cbind(lst5,rules["DECISION"],rules["ACC_RHS"],rules["SUPP_RHS"],row.names = NULL)
    }
  }
  
  else{
      lst33=rules["DISC_CLASSES"]
      
      vec=as.character(as.matrix(rules["FEATURES"]))
      lst1=sapply(vec, function(x) strsplit(x, ","))
      #vec2=as.character(lst33)
      lst2=strsplit(unlist(lst33), ",")
      newLst=mapply(paste,collapse=",",sep="=",lst1,lst2)
      lst5=as.character(unname(newLst))
      if(filterByPval){
        dflbl=cbind(lst5,rules["DECISION"],rules["ACC_RHS"],rules["SUPP_RHS"],row.names = NULL)[which(rules["PVAL"]<pval),]
      }else
      {
        dflbl=cbind(lst5,rules["DECISION"],rules["ACC_RHS"],rules["SUPP_RHS"],row.names = NULL)
      }
  }
  write.table(dflbl, path, col.names = F, row.names = F, sep="\t", quote = F)
  
}

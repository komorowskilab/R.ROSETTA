saveLineByLine <- function(rls, path, discrete=FALSE, geneExprData=TRUE, filterByPval=FALSE, pval=0.05)
  
{
  discretized=discrete
  if(discretized)
  {
      vec=as.character(as.matrix(rls["FEATURES"]))
      lst1=sapply(vec, function(x) strsplit(x, ","))
      vec2=as.character(as.matrix(rls["CUTS_COND"]))
      lst2=sapply(vec2, function(x) strsplit(x, ","))
      newLst=mapply(paste,collapse=",",sep="=",lst1,lst2)
      lst5=as.character(unname(newLst))
      if(filterByPval){
      dflbl=cbind(lst5,rls["DECISION"],rls["ACC_RHS"],rls["SUPP_RHS"],row.names = NULL)[which(rls["PVAL"]<pval),]
      }else
      {
      dflbl=cbind(lst5,rls["DECISION"],rls["ACC_RHS"],rls["SUPP_RHS"],row.names = NULL)
      }
      }
  
  else{
    if(geneExprData)
      {
      lst11=lapply(rls["CUTS_COND"], function(x) gsub("value>cut", "3", x, fixed = T))
      lst22=lapply(lst11, function(x) gsub("cut<value<cut", "2", x, fixed = T))
      lst33=lapply(lst22, function(x) gsub("value<cut", "1", x, fixed = T))

      vec=as.character(as.matrix(rls["FEATURES"]))
      lst1=sapply(vec, function(x) strsplit(x, ","))
      #vec2=as.character(lst33)
      lst2=strsplit(unlist(lst33), ",")
      newLst=mapply(paste,collapse=",",sep="=",lst1,lst2)
      lst5=as.character(unname(newLst))
      if(filterByPval){
        dflbl=cbind(lst5,rls["DECISION"],rls["ACC_RHS"],rls["SUPP_RHS"],row.names = NULL)[which(rls["PVAL"]<pval),]
      }else
      {
        dflbl=cbind(lst5,rls["DECISION"],rls["ACC_RHS"],rls["SUPP_RHS"],row.names = NULL)
      }
    }
    else{
      
      if(filterByPval){
        dflbl=cbind(rls["FEATURES"],rls["DECISION"],rls["ACC_RHS"],rls["SUPP_RHS"],row.names = NULL)[which(rls["PVAL"]<pval),]
      }else
      {
        dflbl=cbind(rls["FEATURES"],rls["DECISION"],rls["ACC_RHS"],rls["SUPP_RHS"],row.names = NULL)
      }
    }
  }
  write.table(dflbl, path, col.names = F, row.names = F, sep="\t", quote = F)

}

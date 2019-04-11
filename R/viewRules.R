viewRules <- function(rules, setDiscLabels=FALSE, newDiscLabels=c("down","medium", "up")){
  
  out_rules=c()
  out_length=c()
  for(i in 1:dim(rules)[1]){
    ftrs=unlist(strsplit(as.character(rules$FEATURES)[i], ","))
    if(setDiscLabels){
      dicl<-unlist(strsplit(as.character(rules$DISC_CLASSES)[i], ","))
      if(is.na(dicl)[1]){
        dicl<-unlist(strsplit(as.character(rules$CUTS_COND)[i], ","))  
      }
      dicl<-newDiscLabels[as.numeric(dicl)]
    }else{
      dicl<-unlist(strsplit(as.character(rules$DISC_CLASSES)[i], ","))
      if(is.na(dicl)[1]){
        dicl<-unlist(strsplit(as.character(rules$CUTS_COND)[i], ","))  
      }
    }
    
    perc=unlist(strsplit(as.character(rules$PERC_SUPP_RHS)[i], ","))
    decs=unlist(as.character(rules$DECISION))[i]
    
    out_length<-rbind(out_length,length(ftrs))
    
    out_rules<-rbind(out_rules,paste0("IF ",paste(paste0(ftrs,paste("(",dicl,")",sep="")), collapse =" AND ")," THEN ",decs))
  }
  out_rules<-as.data.frame(out_rules)
  out_rules<-data.frame(out_rules, out_length, rules$ACC_RHS, rules$SUPP_RHS,  rules$PVAL)
  colnames(out_rules)<-c("rule","length","accuracy","support","pvalue")
  
  return(out_rules)
}

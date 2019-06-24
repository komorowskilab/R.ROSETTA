viewRules <- function(rules, setLabels=FALSE, labels=c("down","medium", "up")){
  out_rules<-out_length<-c()
  
  for(i in 1:dim(rules)[1]){
    ftrs=unlist(strsplit(as.character(rules$features)[i], ","))
    if(setLabels){
      dicl<-unlist(strsplit(as.character(rules$levels)[i], ","))
      if(is.na(dicl)[1]){
        dicl<-unlist(strsplit(as.character(rules$cuts)[i], ","))  
      }
      dicl<-labels[as.numeric(dicl)]
    }else{
      dicl<-unlist(strsplit(as.character(rules$levels)[i], ","))
      if(is.na(dicl)[1]){
        dicl<-unlist(strsplit(as.character(rules$cuts)[i], ","))  
      }
    }
    perc<-unlist(strsplit(as.character(rules$supportRatioRHS)[i], ","))
    decs<-unlist(as.character(rules$decision))[i]
    out_length<-rbind(out_length,length(ftrs))
    out_rules<-rbind(out_rules,paste0("IF ",paste(paste0(ftrs,paste("(",dicl,")",sep="")), collapse =" AND ")," THEN ",decs))
  }
  out_rules<-as.data.frame(out_rules)
  out_rules<-data.frame(out_rules, out_length, rules$accuracyRHS, rules$supportRHS,  rules$pValue)
  colnames(out_rules)<-c("rule","length","accuracy","support","pValue")
  
  return(out_rules)
}

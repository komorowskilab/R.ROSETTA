getFeatures <- function(df, rls, pval=0.05){

newRls=rls
decis2=newRls$DECISION
decsCounts=table(as.character(df[,length(df)]))

lstOut=list()
for(i in 1:length(table(df[,length(df)]))){
  t11=unlist(strsplit(as.character(newRls$FEATURES)[newRls$PVAL<pval & newRls$DECISION==names(decsCounts)[i]],","))
  
  t1=table(t11)
  dft1=data.frame(t11, 1:length(t11))
  dft11=aggregate(dft1[,2],list(dft1[,1]),mean)
  dft11[,2]<-dft11[,2]/unname(t1)
  lstOut[[i]]=dft11[order(dft11[,2]),1]
}

names(lstOut)<-names(decsCounts)
return(lstOut)

}

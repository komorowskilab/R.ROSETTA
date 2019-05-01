randDecisionTable <- function(nFeatures=180, nObjects=120, nOutcome=2, discrete=F, nDiscStates=3, containStrings=F, nStrings=10, nClassStrings=3, unbalanced=F, pUnbalancedClass=0.8){
  
  # # # # # # # # # # # # # # # # # # # #
  # # # # # # create decision # # # # # #
  # # # # # # # # # # # # # # # # # # # #
  
  p=1/nOutcome
  crn=nOutcome-1
  class_rest=c()
  
  if(unbalanced){
    class_1_on=round(pUnbalancedClass*nObjects)
    class_rest_on=nObjects-class_1_on
    class_1=rep("dec_1", class_1_on)
    rpcrn=round(class_rest_on/crn)
    for(i in 1:crn){
    class_rest=c(class_rest,rep(paste0("dec_",i+1),rpcrn))
    }
    
      
  }else{
    class_1_on=round(p*nObjects)
    class_rest_on=nObjects-class_1_on
    class_1=rep("dec_1", class_1_on)
    
    rpcrn=round(class_rest_on/crn)
    for(i in 1:crn){
      class_rest=c(class_rest,rep(paste0("dec_",i+1),rpcrn))
    }
  }
  decision=c(class_1, class_rest)[1:nObjects]
  # # # # # # # # # # # # # # # # # # # #
  # # # # # # # # # # # # # # # # # # # #
  
  
  if(discrete){
    dt=data.frame(replicate(nFeatures,sample(1:nDiscStates,nObjects,rep=TRUE)))
    if(containStrings){
    dt[,(length(dt)-nStrings+1):length(dt)]<-data.frame(replicate(nStrings,paste0("S",sample(1:nClassStrings,nObjects,rep=TRUE))))
    }
    
  }else{
    dt=data.frame(replicate(nFeatures,runif(nObjects,0,1)))
    if(containStrings){
      dt[,(length(dt)-nStrings+1):length(dt)]<-data.frame(replicate(nStrings,paste0("S",sample(1:nClassStrings,nObjects,rep=TRUE))))
    }
  }
  nams_dt=paste0(rep("F", nFeatures),1:nFeatures)
  colnames(dt) <- nams_dt 
  decisionTable <- cbind(dt, decision)
  
return(decisionTable)
}
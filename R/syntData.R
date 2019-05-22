syntData <- function(nFeatures=c(10,5,3,2,2), R=c(0.01,0.05,0.3,0.5,0.65), nObjects=120, nOutcome=2, unbalanced=F, pUnbalancedClass=0.8, seed=1){
  
  set.seed(seed)
  
  #nofs=10
  props=nFeatures
  nout=nOutcome
  nobs=nObjects
  
  ##### decision making
  p=1/nout
  crn=nout-1
  class_rest=c()
  
  if(unbalanced){
    class_1_on=round(pUnbalancedClass*nObjects)
    class_rest_on=nObjects-class_1_on
    class_1=rep(1, class_1_on)
    rpcrn=round(class_rest_on/crn)
    for(i in 1:crn){
      class_rest=c(class_rest,rep(i+1,rpcrn))
    }
    
    
  }else{
    class_1_on=round(p*nobs)
    class_rest_on=nobs-class_1_on
    class_1=rep(1, class_1_on)
    
    rpcrn=round(class_rest_on/crn)
    for(i in 1:crn){
      class_rest=c(class_rest,rep(i+1,rpcrn))
    }
  }
  decision=as.numeric(c(class_1, class_rest)[1:nobs])
  #####
  #create correlation matrix
  adf=data.frame(decision)
  for(i in 1:length(props)){
    
    vecLen=length(which(diag(props[i]+1)==0))/2
    Rs=rep(R[i],vecLen)
    class(Rs) <- 'dist'
    attr(Rs,'Size') <- props[i]+1
    Mr=as.matrix(Rs)+diag(props[i]+1)
    U = t(chol(Mr))
    
    rand_dt = matrix(rnorm(props[i]*nobs,0,1), nrow=props[i], ncol=nobs)
    rand_dt <- rbind(decision, rand_dt)
    X = U %*% rand_dt
    
    if(props[i]==1){
      newX =  as.data.frame(X[-1,])
    }else{
      newX = as.data.frame(t(X[-1,]))  
    }
    
    colnames(newX)<-paste0("F",1:props[i],"_G",i,"_R",R[i])
    
    adf=data.frame(adf, newX)
    
  }
  decision=as.factor(paste0("D",decision))
  adf2=cbind(adf[,-1],decision)
  
  return(adf2)
}
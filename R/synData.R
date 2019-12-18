synData <- function(nFeatures=c(10,5,3,2,2), rf=c(0.2,0.2,0.2,0.2,0.2), rd=c(0.4,0.5,0.6,0.7,0.8), nObjects=120, nOutcome=2, distribution="uniform", unbalanced=F, pUnbalancedClass=0.8, discrete=F, levels=4, labels=c("A","C","G","T"), binProb=0.5, seed=1){
  
  set.seed(seed)
  
  #nofs=10
  props <- nFeatures
  nout <- nOutcome
  nobs <- nObjects
  
  ##### decision making
  p <- 1/nout
  crn <- nout-1
  class_rest <- c()
  
  if(unbalanced){
    class_1_on <- round(pUnbalancedClass*nObjects)
    class_rest_on <- nObjects-class_1_on
    class_1 <- rep(1, class_1_on)
    rpcrn <- round(class_rest_on/crn)
    
    for(i in 1:crn){
      class_rest <- c(class_rest,rep(i+1, rpcrn))
    }
    
  }else{
    class_1_on <- round(p*nobs)
    class_rest_on <- nobs-class_1_on
    class_1 <- rep(1, class_1_on)
    
    rpcrn <- round(class_rest_on/crn)
    for(i in 1:crn){
      class_rest <- c(class_rest,rep(i+1, rpcrn))
    }
  }
  decision <- as.numeric(c(class_1, class_rest)[1:nobs])
  #####
  #create correlation matrix
  adf <- data.frame(decision)
  
  for(i in 1:length(props)){
    
    # between-features correlation
    vecLen <- length(which(diag(props[i]+1)==0))/2
    Rs <- c(rep(rd[i], props[i]), rep(rf[i], vecLen-props[i]))
    class(Rs) <- 'dist'
    attr(Rs,'Size') <- props[i]+1
    Mr <- as.matrix(Rs) + diag(props[i]+1)
    U  <-  suppressWarnings(t(chol(Mr, pivot = TRUE)))
    
    rescale <- function(x) (x-min(x))/(max(x) - min(x)) * (levels-1)+1
    
    if(discrete){
      
      if(distribution == "uniform"){
        rand_dt <- matrix(runif(props[i]*nobs, 0, levels-1), nrow=props[i], ncol=nobs)
        rand_dt <- rbind(decision, rand_dt)
        X <- U %*% rand_dt
        
        if(props[i] == 1){
          newX <- as.data.frame(apply(X[-1,], 1, function(x) rescale(x)))
        }else{
          newX <- as.data.frame(apply(X[-1,], 1, function(x) rescale(x)))
        }
        
      }
      if(distribution == "binomial"){
        rand_dt <- matrix(rbinom(props[i]*nobs, levels-1, binProb), nrow=props[i], ncol=nobs)
        rand_dt <- rbind(decision, rand_dt)
        X <- U %*% rand_dt
        
        if(props[i] == 1){
          newX <- as.data.frame(apply(X[-1,], 1, function(x) rescale(x)))
        }else{
          newX <- as.data.frame(apply(X[-1,], 1, function(x) rescale(x)))
        }
      }
      
    }else{
      if(distribution == "uniform"){
        rand_dt <- matrix(runif(props[i]*nobs,-1,1), nrow=props[i], ncol=nobs)
        rand_dt <- rbind(decision, rand_dt)
        X <- U %*% rand_dt
        
        if(props[i] == 1){
          newX <- as.data.frame(X[-1,])
        }else{
          newX <- as.data.frame(t(X[-1,]))  
        }
      }
      if(distribution == "normal"){
        rand_dt <- matrix(rnorm(props[i]*nobs,0,1), nrow=props[i], ncol=nobs)
        rand_dt <- rbind(decision, rand_dt)
        X <- U %*% rand_dt
        
        if(props[i] == 1){
          newX <- as.data.frame(X[-1,])
        }else{
          newX <- as.data.frame(t(X[-1,]))  
        }
      }
    }
    
    colnames(newX) <- paste0("f",i,".",1:props[i],"_rf",rf[i],"_rd",rd[i])
    adf <- data.frame(adf, newX)
    
  }
  
  
  if(discrete){
    decision <- as.character(paste0("D",decision))
    adf2 <- as.data.frame(cbind(sapply(round(adf[,-1]), function(i) labels[i]), decision))
  }else{
    decision <- as.factor(paste0("D",decision))
    adf2 <- cbind(adf[,-1], decision)
  }

  
  return(adf2)
}

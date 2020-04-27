mergeRBMs <- function(rbms, defClass=autcon$decision, fun="mean", pAdjust = TRUE, pAdjustMethod = "bonferroni"){
  
  #join into one data.frame
  seleFrame <- lapply(rbms, subset, select=c(features, levels, decision, supportLHS, supportRHS))
  mergSeleFrame <- do.call("rbind", seleFrame)
  nOrder <- lapply(strsplit(mergSeleFrame$features, ","), order)
  newFeatures <- strsplit(mergSeleFrame$features, ",")
  newLevels <- strsplit(mergSeleFrame$levels, ",")
  
  #reorder features and levels
  for(i in 1:length(nOrder)){
  newFeatures[[i]] <- newFeatures[[i]][nOrder[[i]]]
  newLevels[[i]] <- newLevels[[i]][nOrder[[i]]]
  }
  
  mergSeleFrame$features <- unlist(lapply(newFeatures, function(x) paste(x, sep=",", collapse=",")))
  mergSeleFrame$levels <- unlist(lapply(newLevels, function(x) paste(x, sep=",", collapse=",")))
  
  if(fun == "mean"){
    mergSeleFrameAgg <- aggregate(. ~ features + levels + decision, data = mergSeleFrame, mean)
  }
  
  if(fun == "sum"){
    mergSeleFrameAgg <- aggregate(. ~ features + levels + decision, data = mergSeleFrame, sum)
  }

  mergSeleFrameAgg$supportLHS <- round(mergSeleFrameAgg$supportLHS)
  mergSeleFrameAgg$supportRHS <- round(mergSeleFrameAgg$supportRHS)

  # rule statistics #
  # p-value for rules
  PVAL <- c()
  RISK_PVAL <- c()
  CONF_INT <- c()
  REL_RISK <- c()
  totClass <- c()
  
  for(i in 1:length(mergSeleFrameAgg$decision)){
    
    R1 <- unname(table(defClass))[which(names(table(defClass))==mergSeleFrameAgg$decision[1])]
    totClass[i] <- R1
    k <- round(mergSeleFrameAgg$supportRHS[i])-1 # P(X > k-1) <-> P(X >= k) ### 

    N <- sum(table(defClass)) # total number
    R2 <- N-R1 # num of samples for the rest samples - total black balls
    # the number of decisions/objects/patients
    C1 <- mergSeleFrameAgg$supportLHS[i] #number of balls drawn 
    PVAL[i] <- phyper(q=k, m=R1, n=R2, k=C1, lower.tail = FALSE)  # calculate pvalue from phypergeometric
    
    # risk ratio
    ge1 <- mergSeleFrameAgg$supportRHS[i]
    # The number of disease occurence among exposed cohort.
    ge2 <- mergSeleFrameAgg$supportLHS[i] - mergSeleFrameAgg$supportRHS[i] ## LHS > RHS
    # The number of disease occurence among non-exposed cohort.
    gt1 <- R1
    # The number of individuals in exposed cohort group.
    gt2 <- R2
    # The number of individuals in non-exposed cohort group.
    invisible(capture.output(rr <- fmsb::riskratio(ge1, ge2, gt1, gt2)))
    ints <- rr$conf.int[1:2]
    ints[is.na(ints)] <- -Inf
    CONF_INT[i] <- paste(as.character(round(ints, digits=3)), collapse =":") #rr 95% confidence intervals
    RISK_PVAL[i] <- rr$p.value #rr p-value
    REL_RISK[i] <- rr$estimate #risk ratio estimate
  }  
  
  mergSeleFrameAgg$accuracyRHS <- mergSeleFrameAgg$supportRHS/mergSeleFrameAgg$supportLHS
  mergSeleFrameAgg$coverageLHS <- mergSeleFrameAgg$supportLHS/totClass
  mergSeleFrameAgg$coverageRHS <- mergSeleFrameAgg$supportRHS/totClass
  if(pAdjust){
    mergSeleFrameAgg$pValue <- p.adjust(PVAL, method = pAdjustMethod)
  }else{
    mergSeleFrameAgg$pValue <- PVAL
  }
  mergSeleFrameAgg$riskRatio <- REL_RISK
  
  if(pAdjust){
    mergSeleFrameAgg$pValueRiskRatio <- p.adjust(RISK_PVAL, method = pAdjustMethod)
  }else{
    mergSeleFrameAgg$pValueRiskRatio <- RISK_PVAL
  }
  mergSeleFrameAgg$confIntRiskRatio <- CONF_INT
  
  mergSeleFrameAgg <- mergSeleFrameAgg[order(mergSeleFrameAgg$pValue, decreasing = F),]
  rownames(mergSeleFrameAgg) <- NULL
  return(mergSeleFrameAgg)
}

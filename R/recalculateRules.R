recalculateRules <- function(dt, rules, discrete=FALSE, pAdjust=TRUE, pAdjustMethod="bonferroni"){

  rl2 <- strsplit(as.character(rules$features), ",", fixed = T)
  
  if(discrete){
    cnd2 <- strsplit(as.character(rules$levels), ",", fixed = T)
  }else{
    cnd2 <- strsplit(as.character(rules$cuts), ",", fixed = T)
  }
  
  dec2 <- as.character(rules$decision)
  objs <- rownames(dt)
  feats <- colnames(dt)
  cuts <- rules[,grep('cut', colnames(rules), value=TRUE)][,-1]
  
  ### functions ###
  less2Vec <- function(x,y){(x-y) <= 0}
  more2Vec <- function(x,y){(x-y) >= 0}
  eqal2Vec <- function(x,y){(x-y) == 0}
  ### ### ### ### ###
  
  if(discrete){
    ####discretized
    for(j in 1:dim(rules)[1]){
      
      cnds <- cnd2[[j]]
      cndsLen <- length(cnds)
      vec3 <- list()
      
      for(i in 1:cndsLen){
        vec3[[i]] <- as.numeric(as.data.frame(dt[,match(rl2[[j]], feats)])[,i] == cnd2[[j]][i])
        #ifelse(length(vec4) == 0, vec4 <- vec3, vec4 <- vec3 & vec4)
      }
      outLst[[j]] <- rownames(dt)[which(rowSums(do.call(cbind, vec3)) == cndsLen)] ##LHS
      dt2 <- dt[which(grepl(dec2[j], dt[,length(dt)])),]
      outLst2[[j]] <- intersect(rownames(dt2), outLst[[j]]) ##RHS
    } 
  }else{
    outLst <- outLst2 <- list()
    
    for(j in 1:dim(rules)[1]){
      cnds <- cnd2[[j]]
      cnds[cnds == "value>cut"] <- 1
      cnds[cnds == "value<cut"] <- 1
      cnds[cnds == "cut<value<cut"] <- 2
      cnds <- as.numeric(cnds)
      cndsLen <- length(cnds)
      cndsCS <- cumsum(cnds)
      vec4 <- c()
      
      for(i in 1:cndsLen){
        
        if(cnd2[[j]][i] == "value>cut"){
          vec3 <- more2Vec(as.data.frame(dt[,match(rl2[[j]], feats)])[,i], as.numeric(cuts[j,][cndsCS[i]]))
          ifelse(length(vec4) == 0, vec4 <- vec3, vec4 <- vec3 & vec4)
        }
        
        if(cnd2[[j]][i] == "value<cut"){
          vec3 <- less2Vec(as.data.frame(dt[,match(rl2[[j]], feats)])[,i], as.numeric(cuts[j,][cndsCS[i]]))
          ifelse(length(vec4) == 0, vec4 <- vec3, vec4 <- vec3 & vec4)
        }
        
        if(cnd2[[j]][i] == "cut<value<cut"){
          vec1 <- less2Vec(as.data.frame(dt[,match(rl2[[j]], feats)])[,i], as.numeric(cuts[j,][cndsCS[i]]))
          vec2 <- more2Vec(as.data.frame(dt[,match(rl2[[j]], feats)])[,i], as.numeric(cuts[j,][cndsCS[i]-1]))
          vec3 <- vec1 == vec2
          ifelse(length(vec4) == 0, vec4 <- vec3, vec4 <- vec3 & vec4)
        }
      }
      
      outLst[[j]] <- rownames(dt)[which(vec4)] ##LHS
      dt2 <- dt[which(grepl(dec2[j], dt[,length(dt)])),]
      outLst2[[j]] <- intersect(rownames(dt2),outLst[[j]])
    }
  }
  
  objectsPerRuleLHS <- unlist(lapply(outLst, function(x) paste(x, collapse = ",")))
  objectsPerRuleRHS <- unlist(lapply(outLst2, function(x) paste(x, collapse = ",")))
  newSupportLHS <- unlist(lapply(outLst, function(x) length(x)))
  newSupportRHS <- unlist(lapply(outLst2, function(x) length(x)))
  newAccuracy <- newSupportRHS/newSupportLHS
  
  ## p-value for rules calculation ##
  PVAL <- c()
  RISK_PVAL <- c()
  CONF_INT <- c()
  REL_RISK <- c()
  
  for(i in 1:length(newSupportLHS)){
    # total support adjusted by accuracy
    k <- round(newSupportRHS[i])-1 # P(X > k-1) <-> P(X >= k)
    # num of samples for current decision - total white balls
    R1 <- unname(table(dt[,length(dt)])[names(table(dt[,length(dt)]))==as.character(rules$decision[i])])
    # num of samples for the rest samples - total black balls
    N <- dim(dt)[1]
    R2 <- N-R1
    # the number of decisions/objects/patients
    C1 <- newSupportLHS[i]   # exposed
    C2 <- newSupportLHS[i]-k # non-exposed
    
    PVAL[i] <- phyper(q=k, m=R1, n=R2, k=C1, lower.tail = FALSE)  # calculate pvalue from phypergeometric
    
    # risk ratio
    ge1 <- newSupportRHS[i]
    #The number of disease occurence among exposed cohort.
    ge2 <- newSupportLHS[i] - newSupportRHS[i] ## LHS > RHS
    #The number of disease occurence among non-exposed cohort.
    gt1 <- R1
    #The number of individuals in exposed cohort group.
    gt2 <- R2
    #The number of individuals in non-exposed cohort group.
    
    invisible(capture.output(rr <- fmsb::riskratio(ge1, ge2, gt1, gt2)))
    
    ints <- rr$conf.int[1:2]
    ints[is.na(ints)] <- -Inf
    
    CONF_INT[i] <- paste(as.character(round(ints, digits=3)), collapse =":") #rr 95% confidence intervals
    RISK_PVAL[i] <- rr$p.value #rr p-value
    REL_RISK[i] <- rr$estimate #risk ratio estimate
  }
  
  if(pAdjust){
    PVAL <- p.adjust(PVAL, method=pAdjustMethod)
    RISK_PVAL <- p.adjust(RISK_PVAL, method=pAdjustMethod)
  }
  
  decsCounts <- table(dt[,length(dt)])
  
  numClass <- rep(0,length(rules$decision))
  for(i in 1:length(table(dt[,length(dt)]))){
    numClass[which(rules$decision == names(table(dt[,length(dt)]))[i])] <- unname(table(dt[,length(dt)]))[i]
  }
  
  percSuppLHS <- round(newSupportLHS/length(as.character(dt[,length(dt)])), digits=5)                         
  percSuppRHS <- round(newSupportRHS/numClass, digits=5)                         
  
  if(discrete){
    newDT <- data.frame(as.character(rules$features),as.character(rules$levels),as.character(rules$decision),newSupportLHS,newSupportRHS,newAccuracy,percSuppLHS,percSuppRHS,PVAL,  REL_RISK,RISK_PVAL, CONF_INT, objectsPerRuleLHS,objectsPerRuleRHS)
    colnames(newDT) <- c("features","levels","decision","supportLHS","supportRHS","accuracyRHS","coverageLHS", "coverageRHS", "pValue", "riskRatio", "pValueRiskRatio", "confIntRiskRatio", "supportSetLHS", "supportSetRHS")
    newDT2 <- newDT[order(newDT$pValue),]
    rownames(newDT2) <- NULL
  }else{
    newDT <- data.frame(as.character(rules$features),as.character(rules$levels),as.character(rules$decision),newSupportLHS,newSupportRHS,newAccuracy,percSuppLHS,percSuppRHS,rules$cuts,cuts,PVAL,  REL_RISK,RISK_PVAL, CONF_INT, objectsPerRuleLHS,objectsPerRuleRHS)
    colnames(newDT) <- c("features","levels","decision","supportLHS","supportRHS","accuracyRHS", "coverageLHS", "coverageRHS", "cuts",colnames(cuts), "pValue", "riskRatio", "pValueRiskRatio", "confIntRiskRatio", "supportSetLHS", "supportSetRHS")
    newDT2 <- newDT[order(newDT$pValue),]
    rownames(newDT2) <- NULL
  }
  i <- sapply(newDT2, is.factor)
  newDT2[i] <- lapply(newDT2[i], as.character)
  return(newDT2)
}
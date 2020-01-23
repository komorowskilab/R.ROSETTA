getFeatures <- function(rules, filter = F, filterType = "support", thr = 10){
  
  decis2 <- rules$decision
  decsCounts <- unique(decis2)
  vec0 <- lstOut1 <- lstOut2 <- lstOut3 <- list()
  
  for(i in 1:length(decsCounts)){
    
    if(filter){
      if(filterType == "support"){
        t11 <- unlist(strsplit(as.character(rules$features)[rules$supportRHS > thr & rules$decision == decsCounts[i]],","))
        t12 <- unlist(strsplit(as.character(rules$levels)[rules$supportRHS > thr & rules$decision == decsCounts[i]],","))
      }
      if(filterType == "accuracy"){
        t11 <- unlist(strsplit(as.character(rules$features)[rules$accuracyRHS > thr & rules$decision == decsCounts[i]],","))
        t12 <- unlist(strsplit(as.character(rules$levels)[rules$accuracyRHS > thr & rules$decision == decsCounts[i]],","))
      }
      if(filterType == "pvalue"){
        t11 <- unlist(strsplit(as.character(rules$features)[rules$pValue < thr & rules$decision == decsCounts[i]],","))
        t12 <- unlist(strsplit(as.character(rules$levels)[rules$pValue < thr & rules$decision == decsCounts[i]],","))
      }
    }else{
      t11 <- unlist(strsplit(as.character(rules$features)[rules$decision == decsCounts[i]],","))
      t12 <- unlist(strsplit(as.character(rules$levels)[rules$decision == decsCounts[i]],","))
    }
    
    t1 <- table(t11)
    dft1 <- data.frame(names(t1), unname(t1)/length(t11)*100)
    
    for(j in 1:length(t1)){
      perc <- unname(round(table(t12[which(t11 %in% names(t1)[j])])/sum(table(t12[which(t11 %in% names(t1)[j])]))*100))
      vec0[[j]] <- paste(unlist(paste(sort(unique(t12[which(t11 %in% names(t1)[j])])), paste0(perc,"%)"), sep="(")), collapse = ",")
    }
    
    lstOut1[[i]] <- as.character(dft1[order(dft1$Freq, decreasing = T), 1])
    lstOut2[[i]] <- as.numeric(dft1[order(dft1$Freq, decreasing = T), 3])
    lstOut3[[i]] <- unlist(vec0)[order(dft1$Freq, decreasing = T)]
  }
  names(lstOut1) <- decsCounts
  names(lstOut2) <- decsCounts
  names(lstOut3) <- decsCounts
  
  return(list(features = lstOut1, frequencies = lstOut2, levels = lstOut3))
  
}
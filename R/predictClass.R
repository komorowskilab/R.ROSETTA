predictClass <- function(dt, rules, discrete=FALSE, normalize=TRUE, normalizeMethod="rss", validate=FALSE, defClass){
  
  dec2 <- as.character(rules$decision)
  decs2 <- unique(dec2)
  objs <- rownames(dt)
  feats <- colnames(dt)
  ruleVotes <- list()
  
  change_expr <- function(expr){
    if(any(str_count(expr, "<")==2)){
      vals <- unlist(str_split(expr[[which(str_count(expr, "<")==2)]], "<"))
      expr[[which(str_count(expr, "<")==2)]] <- paste0(vals[1],"<",vals[2]," & ",vals[2],"<",vals[3])
    }
    return(expr)
  }
  
  if(discrete){ #ONLY DISCRETE DATA
    
    cuts <- strsplit(as.character(rules$levels),",",fixed = T)
    
 for (j in 1:dim(dt)[1]) {
  str_l <- list()
  object <- dt[j, ]
  for (i in 1:length(cuts)) {
    if (length(as.character(unname(object[match(unlist(strsplit(rules[i, ]$features, ",")), colnames(object))]))) == 0) {
      str_l[[i]] <- NA
    }
    else {
      str <- paste0("'", paste0(as.character(as.matrix(unname(object[match(unlist(strsplit(rules[i, ]$features, ",")), colnames(object))]))), collapse = ","), 
                    "'", "==", "'", paste0(cuts[[i]], collapse = ","), "'")
      str_l[[i]] <- eval(parse(text = str))
    }
  }
  ruleVotes[[j]] <- t(as.matrix(table(factor(rules$decision[which(unlist(str_l) == TRUE)], levels = unique(rules$decision)))))
}
    
  }else{
    
    cuts <- rules[,grep('cut', colnames(rules), value=TRUE)][,-1]
    cuts_cond <- rules$cuts
    
    for(j in 1:dim(dt)[1]){
      
      str_l <- list()
      object <- dt[j,]
      
      for(i in 1:length(cuts_cond)){
        
        if(str_detect(cuts_cond[i], "cut")){ # MIXED OR NON-DISCRETE RULES
          
          ##cuts
          n_cuts <- str_count(cuts_cond[i], "cut")
          cut_parts <- unlist(strsplit(as.character(cuts_cond[i]), "(?<=cut)", perl = TRUE))
          cut_replacements <- paste0("(", as.character(unname(cuts[i,]))[1:n_cuts], ")")
          for (k in seq_along(cut_parts)) {
            if (k <= length(cut_replacements)) {
              cut_parts[k] <- str_replace(cut_parts[k], "cut", cut_replacements[k])
            }
          }
          str <- paste0(cut_parts, collapse = "")
          
          ##values
          n_vals <- str_count(cuts_cond[i], "value")
          val_parts <- unlist(strsplit(str, "(?<=value)", perl = TRUE))
          val_replacements <- paste0("(", as.character(unname(object[which(colnames(object) %in% unlist(strsplit(as.character(rules[i,]$features), ",")))]))[1:n_vals], ")")
          for (k in seq_along(val_parts)) {
            if (k <= length(val_replacements)) {
              val_parts[k] <- str_replace(val_parts[k], "value", val_replacements[k])
            }
          }
          str <- paste0(val_parts, collapse = "")
          ##discrete
          key_words <- c("discrete", "cut")
          matches <- str_c(key_words, collapse ="|")
          strs_n <- which(unlist(str_extract_all(cuts_cond[i], matches)) == "discrete")
          
          if(length(strs_n) == 0){
            
            expr <- unlist(str_split(unlist(str), ","))
            str_l[[i]] <- eval(parse(text=unlist(lapply(expr, change_expr))))
            
          }else{
            n_disc <- str_count(cuts_cond[i], "discrete")
            disc_val <- as.character(unname(object[1,which(colnames(object[1,]) %in% unlist(strsplit(as.character(rules[i,]$features),",")))]))[which(unlist(str_split(cuts_cond[i], ","))=="discrete")]
            
            disc_parts <- unlist(strsplit(str, "(?<=discrete)", perl = TRUE))
            disc_replacements <- paste0(disc_val, "==", as.character(unname(cuts[i,]))[strs_n])
            for (k in seq_along(disc_parts)) {
              if (k <= length(disc_replacements)) {
                disc_parts[k] <- str_replace(disc_parts[k], "discrete", disc_replacements[k])
              }
            }
            str <- paste0(disc_parts, collapse = "")
            expr <- unlist(str_split(unlist(str),","))
            
            str_l[[i]] <- eval(parse(text = unlist(lapply(expr, change_expr))))
          }
          
        }else{ ## ONLY DISCRETE RULES
          n_cuts <- str_count(cuts_cond[i], "discrete")
          disc_val <- as.character(unname(dn111[1,which(colnames(dn111[1,]) %in% unlist(strsplit(as.character(rules[i,]$features),",")))]))[which(unlist(str_split(cuts_cond[i], ",")) == "discrete")]
          
          disc_parts2 <- unlist(strsplit(cuts_cond[i], "(?<=discrete)", perl = TRUE))
          disc_replacements2 <- paste0(disc_val, "==", as.character(unname(cuts[i,]))[1:n_cuts])
          for (k in seq_along(disc_parts2)) {
            if (k <= length(disc_replacements2)) {
              disc_parts2[k] <- str_replace(disc_parts2[k], "discrete", disc_replacements2[k])
            }
          }
          str <- paste0(disc_parts2, collapse = "")
          
          expr <- unlist(str_split(unlist(str),","))
          str_l[[i]] <- eval(parse(text = unlist(lapply(expr, change_expr))))
        }
        
      }
      
      ruleVotes[[j]] <- t(as.matrix(table(factor(rules$decision[which(unlist(str_l)==TRUE)], levels = unique(rules$decision)))))
      
    }
    
  }
  
  if(length(unlist(ruleVotes))==0){
    stop("Not able to calculate votes. Values do not correspond to cuts. Empty vector produced.")
  }
  
  ruleVotesDf <- do.call(rbind, ruleVotes)
  
  ### VOTES NORMALZIATION PART ###
  
  if(normalize){
    if(normalizeMethod == "median"){
      ruleVotesDf <- sweep(ruleVotesDf, 2, apply(ruleVotesDf, 2, median), "/")
    }
    
    if(normalizeMethod=="mean"){
      ruleVotesDf <- sweep(ruleVotesDf, 2, apply(ruleVotesDf, 2, mean), "/")
    }
    
    if(normalizeMethod=="max"){
      ruleVotesDf <- sweep(ruleVotesDf, 2, apply(ruleVotesDf, 2, max), "/")
    }
    
    if(normalizeMethod=="rss"){ #root sum square
      fun <- function(x){sqrt(sum(x)^2)}
      ruleVotesDf <- sweep(ruleVotesDf, 2, apply(ruleVotesDf, 2, fun), "/")
    }
    
    if(normalizeMethod=="rulnum"){
      ruleVotesDf <- sweep(ruleVotesDf, 2, t(as.matrix(table(dec2))), "/")
    }
  }
  
  
  if(validate){ ### with validation
    
    newDecs <- colnames(ruleVotesDf)[apply(ruleVotesDf, 1, which.max)]
    outListVotes <- data.frame(ruleVotesDf,as.character(defClass),newDecs)
    colnames(outListVotes) <- c(colnames(ruleVotesDf),"currentClass","predictedClass")
    rownames(outListVotes) <- rownames(dt)
    
    acc <- c()
    for(i in 1:length(newDecs)){
      acc[i] <- c((as.character(defClass[i]) == newDecs[i]) | (newDecs[i] == as.character(defClass[i])))
    }
    
    return(list(out=outListVotes, accuracy=length(which(acc))/length(acc)))
    
  }else{
    newDecs <- colnames(ruleVotesDf)[apply(ruleVotesDf, 1, which.max)]
    outListVotes <- data.frame(ruleVotesDf,newDecs)
    colnames(outListVotes) <- c(decs2,"predictedClass")
    rownames(outListVotes) <- rownames(dt)
    return(list(out=outListVotes))
  }
  
}

library(stringr)
library(dplyr)
library(tibble)

evaluate_expr <- function(expr){
  expr_result <- TRUE
  for (i in 1:length(expr)){
    expr_i <- eval(expr[i])
    if (is.na(expr_i)){
      return(FALSE)
    }
    expr_result <- expr_result && expr_i
  }
  return(isTRUE(expr_result))
}

predictClass <- function(dt, rules, discrete=FALSE, normalize=TRUE, normalizeMethod="rss",
                         validate=FALSE, defClass, weighted=TRUE){
  
  if(weighted && !"accuracyRHS" %in% colnames(rules)){
    stop("weighted=TRUE requires an 'accuracyRHS' column in `rules`. Use weighted=FALSE otherwise.")
  }
  if(validate && missing(defClass)){
    stop("validate=TRUE requires `defClass`.")
  }
  
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
  
  # Replace the k-th occurrence of `token` with the k-th replacement
  replace_tokens <- function(string, token, replacements){
    parts <- unlist(strsplit(string, paste0("(?<=", token, ")"), perl = TRUE))
    for (k in seq_along(parts)) {
      if (k <= length(replacements)) {
        parts[k] <- str_replace(parts[k], token, replacements[k])
      }
    }
    paste0(parts, collapse = "")
  }
  
  # Count-based votes: one-row data.frame, one column per decision
  count_votes <- function(fired){
    as.data.frame(t(as.matrix(table(factor(rules$decision[fired], levels = unique(rules$decision))))))
  }
  
  # Accuracy-weighted votes: sum of accuracyRHS of fired rules per decision
  weighted_votes <- function(fired){
    levs <- unique(as.character(rules$decision))
    votes <- sapply(levs, function(l){
      idx <- fired[as.character(rules$decision[fired]) == l]
      sum(as.numeric(as.character(rules$accuracyRHS[idx])))
    })
    out <- as.data.frame(t(votes))
    colnames(out) <- levs
    out
  }
  
  get_votes <- function(fired){
    if(weighted) weighted_votes(fired) else count_votes(fired)
  }
  
  if(discrete){ #ONLY DISCRETE DATA
    
    cuts <- strsplit(as.character(rules$levels), ",", fixed = TRUE)
    
    for (j in 1:dim(dt)[1]) {
      str_l <- list()
      object <- dt[j, ]
      for (i in 1:length(cuts)) {
        idx <- match(unlist(strsplit(rules[i, ]$features, ",")), colnames(object))
        if (length(as.character(unname(object[idx]))) == 0) {
          str_l[[i]] <- NA
        } else {
          str <- paste0("'", paste0(as.character(as.matrix(unname(object[idx]))), collapse = ","),
                        "'", "==", "'", paste0(cuts[[i]], collapse = ","), "'")
          str_l[[i]] <- eval(parse(text = str))
        }
      }
      fired <- which(unlist(str_l) == TRUE)
      ruleVotes[[j]] <- get_votes(fired)
    }
    
  }else{
    
    cuts <- rules[,grep('cut', colnames(rules), value=TRUE)][,-1]
    cuts_cond <- rules$cuts
    
    for(j in 1:dim(dt)[1]){
      
      str_l <- list()
      object <- dt[j,]
      
      for(i in 1:length(cuts_cond)){
        
        feat_names <- unlist(strsplit(as.character(rules[i,]$features), ","))
        
        if(str_detect(cuts_cond[i], "cut")){ # MIXED OR NON-DISCRETE RULES
          
          ## cuts
          n_cuts <- str_count(cuts_cond[i], "cut")
          cut_repl <- paste0("(", as.character(unname(cuts[i,]))[1:n_cuts], ")")
          str <- replace_tokens(as.character(cuts_cond[i]), "cut", cut_repl)
          
          ## values
          n_vals <- str_count(cuts_cond[i], "value")
          val_repl <- paste0("(", as.character(unname(object[which(colnames(object) %in% feat_names)]))[1:n_vals], ")")
          str <- replace_tokens(str, "value", val_repl)
          
          ## discrete
          key_words <- c("discrete", "cut")
          matches <- str_c(key_words, collapse = "|")
          strs_n <- which(unlist(str_extract_all(cuts_cond[i], matches)) == "discrete")
          
          if(length(strs_n) > 0){
            disc_val <- as.character(unname(object[1, which(colnames(object) %in% feat_names)]))[which(unlist(str_split(cuts_cond[i], ",")) == "discrete")]
            disc_repl <- paste0(disc_val, "==", as.character(unname(cuts[i,]))[strs_n])
            str <- replace_tokens(str, "discrete", disc_repl)
          }
          
          expr <- unlist(str_split(unlist(str), ","))
          str_l[[i]] <- evaluate_expr(parse(text = unlist(lapply(expr, change_expr))))
          
        }else{ ## ONLY DISCRETE RULES
          
          n_disc <- str_count(cuts_cond[i], "discrete")
          disc_val <- as.character(unname(object[1, which(colnames(object) %in% feat_names)]))[which(unlist(str_split(cuts_cond[i], ",")) == "discrete")]
          disc_repl <- paste0(disc_val, "==", as.character(unname(cuts[i,]))[1:n_disc])
          str <- replace_tokens(as.character(cuts_cond[i]), "discrete", disc_repl)
          
          expr <- unlist(str_split(unlist(str), ","))
          str_l[[i]] <- evaluate_expr(parse(text = unlist(lapply(expr, change_expr))))
        }
      }
      
      fired <- which(unlist(str_l) == TRUE)
      ruleVotes[[j]] <- get_votes(fired)
    }
  }
  
  if(length(unlist(ruleVotes))==0){
    stop("Not able to calculate votes. Values do not correspond to cuts. Empty vector produced.")
  }
  
  ruleVotesDf <- do.call(dplyr::bind_rows, ruleVotes)
  ruleVotesDf[is.na(ruleVotesDf)] <- 0
  
  ### VOTES NORMALIZATION PART ###
  
  if(normalize){
    if(normalizeMethod == "median"){
      ruleVotesDf <- sweep(ruleVotesDf, 2, apply(ruleVotesDf, 2, median), "/")
    }
    
    if(normalizeMethod == "mean"){
      ruleVotesDf <- sweep(ruleVotesDf, 2, apply(ruleVotesDf, 2, mean), "/")
    }
    
    if(normalizeMethod == "max"){
      ruleVotesDf <- sweep(ruleVotesDf, 2, apply(ruleVotesDf, 2, max), "/")
    }
    
    if(normalizeMethod == "rss"){ # root sum square
      fun <- function(x){sqrt(sum(x^2))}
      ruleVotesDf <- sweep(ruleVotesDf, 2, apply(ruleVotesDf, 2, fun), "/")
    }
    
    if(normalizeMethod == "rulnum"){
      ruleVotesDf <- sweep(ruleVotesDf, 2, as.numeric(table(dec2)[colnames(ruleVotesDf)]), "/")
    }
    
    # 0/0 (class never voted for) gives NaN/Inf; treat as no vote
    ruleVotesDf[is.na(ruleVotesDf)] <- 0
    ruleVotesDf[sapply(ruleVotesDf, is.infinite)] <- 0
  }
  
  newDecs <- colnames(ruleVotesDf)[apply(ruleVotesDf, 1, which.max)]
  
  if(validate){ ### with validation
    
    outListVotes <- data.frame(ruleVotesDf, as.character(defClass), newDecs)
    colnames(outListVotes) <- c(colnames(ruleVotesDf), "currentClass", "predictedClass")
    rownames(outListVotes) <- rownames(dt)
    
    acc <- as.character(defClass) == newDecs
    
    return(list(out=outListVotes, accuracy=sum(acc)/length(acc)))
    
  }else{
    outListVotes <- data.frame(ruleVotesDf, newDecs)
    colnames(outListVotes)[ncol(outListVotes)] <- "predictedClass"
    rownames(outListVotes) <- rownames(dt)
    return(list(out=outListVotes))
  }
}

# #TEST
# library(R.ROSETTA)
# ros <- rosetta(autcon)
# rules <- ros$main
# pred <- predictClass(autcon, rules, weighted = FALSE)
# pred <- predictClass(autcon, rules, discrete = F, weighted = TRUE,
#                      validate = TRUE, defClass = autcon$decision)

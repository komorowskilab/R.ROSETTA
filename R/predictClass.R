predictClass <- function(dt, rules, discrete=FALSE, normalize=TRUE, normalizeMethod="rss", validate=FALSE, valiDec){
  
  dec2<-as.character(rules$decision)
  decs2<-unique(dec2)
  objs<-rownames(dt)
  feats<-colnames(dt)
  
  if(discrete){
    cuts<-strsplit(as.character(rules$levels),",",fixed = T)
    
    standardVoter <- function(object){
      ##values
      for(i in 1:length(cuts)){
        str <- paste0(as.character(unname(object[which(colnames(object) %in% unlist(strsplit(rules[i,]$features,",")))])),"==",cuts[[i]])
        str <- eval(parse(text=str))
      }
      return(t(as.matrix(table(rules$decision[which(unlist(str_l)==TRUE)]))))
    }
    
  }else{
    cuts<-rules[,grep('cut', colnames(rules), value=TRUE)][,-1]
    cuts_cond <- rules$cuts
    standardVoter <- function(object){
      
      str_l <- list()
      
      for(i in 1:length(cuts_cond)){
        
        if(str_detect(cuts_cond[i],"cut")){
          ##cuts
          n_cuts <- str_count(cuts_cond[i], "cut")
          str <- str_replace_all(unlist(strsplit(cuts_cond[i], "(?<=cut)", perl = TRUE)), rep("cut", n_cuts), as.character(unname(cuts[i,]))[1:n_cuts])  
          str <- paste0(str,collapse="")
          
          ##values
          n_vals <- str_count(cuts_cond[i], "value")
          str <- str_replace_all(unlist(strsplit(str, "(?<=value)", perl = TRUE)), rep("value", n_vals), as.character(unname(object[which(colnames(object) %in% unlist(strsplit(rules[i,]$features,",")))]))[1:n_vals])  
          str <- paste0(str,collapse="")
          
          ##discrete
          key_words <- c("discrete","cut")
          matches <- str_c(key_words, collapse ="|")
          strs_n <- which(unlist(str_extract_all(cuts_cond[i], matches))=="discrete")
          
          if(length(strs_n)==0){
            expr <- unlist(str_split(unlist(str),","))
            
            change_expr <- function(expr){
              if(any(str_count(expr, "<")==2)){
                vals <- unlist(str_split(expr[[which(str_count(expr, "<")==2)]], "<"))
                expr[[which(str_count(expr, "<")==2)]] <- paste0(vals[1],"<",vals[2]," & ",vals[2],"<",vals[3])
                
              }
              return(expr)
            }
            
            str_l[[i]] <- eval(parse(text=unlist(lapply(expr, change_expr))))
          }else{
            n_disc <- str_count(cuts_cond[i], "discrete")
            disc_val <- as.character(unname(dn111[1,which(colnames(dn111[1,]) %in% unlist(strsplit(rules[i,]$features,",")))]))[which(unlist(str_split(cuts_cond[i], ","))=="discrete")]
            
            str <- str_replace_all(unlist(strsplit(str, "(?<=discrete)", perl = TRUE)), rep("discrete", n_disc), paste0(disc_val,"==",as.character(unname(cuts[i,]))[strs_n]))
            str <- paste0(str,collapse="")
            expr <- unlist(str_split(unlist(str),","))
            
            change_expr <- function(expr){
              if(any(str_count(expr, "<")==2)){
                vals <- unlist(str_split(expr[[which(str_count(expr, "<")==2)]], "<"))
                expr[[which(str_count(expr, "<")==2)]] <- paste0(vals[1],"<",vals[2]," & ",vals[2],"<",vals[3])
                
              }
              return(expr)
            }
            
            str_l[[i]] <- eval(parse(text=unlist(lapply(expr, change_expr))))
          }
          
        }else{
          n_cuts <- str_count(cuts_cond[i], "discrete")
          disc_val <- as.character(unname(dn111[1,which(colnames(dn111[1,]) %in% unlist(strsplit(rules[i,]$features,",")))]))[which(unlist(str_split(cuts_cond[i], ","))=="discrete")]
          
          str <- str_replace_all(unlist(strsplit(cuts_cond[i], "(?<=discrete)", perl = TRUE)), rep("discrete", n_cuts), paste0(disc_val,"==",as.character(unname(cuts[i,]))[1:n_cuts]))  
          str <- paste0(str,collapse="")
          
          expr <- unlist(str_split(unlist(str),","))
          
          change_expr <- function(expr){
            if(any(str_count(expr, "<")==2)){
              vals <- unlist(str_split(expr[[which(str_count(expr, "<")==2)]], "<"))
              expr[[which(str_count(expr, "<")==2)]] <- paste0(vals[1],"<",vals[2]," & ",vals[2],"<",vals[3])
              
            }
            return(expr)
          }
          
          str_l[[i]] <- eval(parse(text=unlist(lapply(expr, change_expr))))
        }
        
      }
      return(t(as.matrix(table(rules$decision[which(unlist(str_l)==TRUE)]))))
    }
    
  }
  
  dt_list<- split(dt, seq(nrow(dt)))
  ruleVotes <- lapply(dt_list, standardVoter)
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
    outListVotes <- data.frame(ruleVotesDf,as.character(valiDec),newDecs)
    colnames(outListVotes) <- c(colnames(ruleVotesDf),"currentClass","predictedClass")
    rownames(outListVotes) <- rownames(dt)
    
    acc <- c()
    for(i in 1:length(newDecs)){
      acc[i] <- c(grepl(as.character(valiDec[i]), newDecs[i]) | grepl(newDecs[i], as.character(valiDec[i])))
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
recalculateRules<-function(dt, rules, discrete=FALSE, pAdjust=TRUE, pAdjustMethod="BH"){
discretized=discrete
rl2=strsplit(as.character(rules$FEATURES),",",fixed = T)
cnd2=strsplit(as.character(rules$CUTS_COND),",",fixed = T)
dec2=as.character(rules$DECISION)
objs=rownames(dt)
feats=colnames(dt)
cuts=rules[,grep('CUT_', colnames(rules), value=TRUE)]
less2Vec<-function(x,y){ (x-y)<=0}
more2Vec<-function(x,y){ (x-y)>=0}
eqal2Vec<-function(x,y){ (x-y)==0}

if(discretized)
{
  ####discretized
  outLst=list()
  outLst2=list()

  for(j in 1:dim(rules)[1]){
    cnds=cnd2[[j]]
    #cnds<-as.numeric(cnds)
    cndsLen=length(cnds)
    
    vec4=c()
    for(i in 1:cndsLen){
      
      
      vec3=eqal2Vec(as.data.frame(dt[,which(feats %in% rl2[[j]])])[,i], as.numeric(cuts[j,i]))
      
      if(length(vec4)==0)
      {
        vec4=vec3
      }else{
        vec4=vec3 & vec4
      }
      
    }
    
    outLst[[j]]=rownames(dt)[which(vec4)] ##LHS
    dt2=dt[which(grepl(dec2[j], dt[,length(dt)])),]
    outLst2[[j]]=intersect(rownames(dt2),outLst[[j]])
    } 
}else{
outLst=list()
outLst2=list()
for(j in 1:dim(rules)[1]){

cnds=cnd2[[j]]
cnds[cnds == "value>cut"] <- 1
cnds[cnds == "value<cut"] <- 1
cnds[cnds == "cut<value<cut"] <- 2
cnds<-as.numeric(cnds)
cndsLen=length(cnds)
cndsCS=cumsum(cnds)
vec4=c()
for(i in 1:cndsLen){

  if(cnd2[[j]][i]=="value>cut")
  {
    vec3=more2Vec(as.data.frame(dt[,which(feats %in% rl2[[j]])])[,i],as.numeric(cuts[j,][cndsCS[i]]))
    
    if(length(vec4)==0)
    {
    vec4=vec3
    }else{
    vec4=vec3 & vec4
    }
  }
  if(cnd2[[j]][i]=="value<cut")
  {
    vec3=less2Vec(as.data.frame(dt[,which(feats %in% rl2[[j]])])[,i],as.numeric(cuts[j,][cndsCS[i]]))
    
    if(length(vec4)==0)
    {
      vec4=vec3
    }else{
      vec4=vec3 & vec4
    }
  }
  if(cnd2[[j]][i]=="cut<value<cut")
  {
    vec1=less2Vec(as.data.frame(dt[,which(feats %in% rl2[[j]])])[,i],as.numeric(cuts[j,][cndsCS[i]]))
    vec2=more2Vec(as.data.frame(dt[,which(feats %in% rl2[[j]])])[,i],as.numeric(cuts[j,][cndsCS[i]-1]))
    vec3=vec1==vec2
    
    if(length(vec4)==0)
    {
      vec4=vec3
    }else{
      vec4=vec3 & vec4
    }
  }
}

outLst[[j]]=rownames(dt)[which(vec4)] ##LHS
dt2=dt[which(grepl(dec2[j], dt[,length(dt)])),]
outLst2[[j]]=intersect(rownames(dt2),outLst[[j]])
}
}
objectsPerRuleLHS=unlist(lapply(outLst, function(x) paste(x, collapse = ",")))
objectsPerRuleRHS=unlist(lapply(outLst2, function(x) paste(x, collapse = ",")))
newSupportLHS=unlist(lapply(outLst, function(x) length(x)))
newSupportRHS=unlist(lapply(outLst2, function(x) length(x)))
newAccuracy=newSupportRHS/newSupportLHS

  ## p-value for rules calculation ##
  PVAL <- c()
  RISK_PVAL <- c()
  CONF_INT <- c()
  REL_RISK <- c()
                            
  for(i in 1:length(newSupportLHS)){
    k=round(newSupportLHS[i]*newAccuracy[i])
    
    R1=unname(table(dt[,length(dt)])[names(table(dt[,length(dt)]))== as.character(rules$DECISION[i])])
    N=dim(dt)[1] 
    R2=N-R1
                # the number of decisions/objects/patients
    C1=newSupportLHS[i]   # LHS Support
    #C2=N-C1                  # total drawn
    #R1=dim(dt)[2]            # total hits, number of features
    #R2=N-R1                  # number of features - number of decisions
    PVAL[i]=phyper(k-1, R1, R2, C1, lower.tail = FALSE)  # calculate pvalue from phypergeometric
   
   # risk ratio
   invisible(capture.output(rr<-riskratio(k, C1, R1, N, conf.level=0.95)))
     
   CONF_INT[i] <- paste(as.character(round(rr$conf.int[1:2], digits=3)), collapse =":")
   RISK_PVAL[i] <- rr$p.value
   REL_RISK[i] <- rr$estimate
  }

 
  if(pAdjust){
   PVAL <- p.adjust(PVAL, method=pAdjustMethod)
  }

#decsFinal= unlist(lapply(rules$DECISION, FUN=function(x) (regmatches(x, gregexpr("(?<=\\().*?(?=\\))", x, perl=T))[[1]])))

decsCounts=table(dt[,length(dt)])

numClass=rep(0,length(rules$DECISION))
for(i in 1:length(table(dt[,length(dt)]))){
numClass[which(rules$DECISION==names(table(dt[,length(dt)]))[i])]<-unname(table(dt[,length(dt)]))[i]
}

percSuppLHS=round(newSupportLHS/numClass, digits=3)*100                          
percSuppRHS=round(newSupportRHS/numClass, digits=3)*100                          

 cutsDT=rules[,which(grepl("CUT_", colnames(rules)))]                           
 newDT=data.frame(rules$FEATURES,rules$DECISION,rules$CUTS_COND,rules$DISC_CLASSES,cutsDT,objectsPerRuleLHS,objectsPerRuleRHS,newSupportLHS,newSupportRHS,percSuppLHS,percSuppRHS,newAccuracy,PVAL, RISK_PVAL, REL_RISK, CONF_INT)
 newDT2=newDT[order(newDT$PVAL),]
 colnames(newDT2)<-c("FEATURES","DECISION","CUTS_COND","DISC_CLASSES",colnames(cutsDT),"SUPP_SET_LHS","SUPP_SET_RHS","SUPP_LHS","SUPP_RHS","PERC_SUPP_LHS","PERC_SUPP_RHS","ACC_RHS","PVAL", "RISK_PVAL", "REL_RISK", "CONF_INT")
 rownames(newDT2)<-NULL
 return(newDT2)
}

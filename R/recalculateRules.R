recalculateRules<-function(df, rls, discrete=FALSE, pAdjust=TRUE, pAdjustMethod="BH"){
discretized=discrete
rl2=strsplit(as.character(rls$FEATURES),",",fixed = T)
cnd2=strsplit(as.character(rls$CUTS_COND),",",fixed = T)
dec2=as.character(rls$DECISION)
objs=rownames(df)
feats=colnames(df)
cuts=rls[,grep('CUT_', colnames(rls), value=TRUE)]
less2Vec<-function(x,y){ (x-y)<=0}
more2Vec<-function(x,y){ (x-y)>=0}
eqal2Vec<-function(x,y){ (x-y)==0}

if(discretized)
{
  ####discretized
  outLst=list()
  outLst2=list()

  for(j in 1:dim(rls)[1]){
    cnds=cnd2[[j]]
    #cnds<-as.numeric(cnds)
    cndsLen=length(cnds)
    
    vec4=c()
    for(i in 1:cndsLen){
      
      
      vec3=eqal2Vec(as.data.frame(df[,which(feats %in% rl2[[j]])])[,i], as.numeric(cuts[j,i]))
      
      if(length(vec4)==0)
      {
        vec4=vec3
      }else{
        vec4=vec3 & vec4
      }
      
    }
    
    outLst[[j]]=rownames(df)[which(vec4)] ##LHS
    df2=df[which(grepl(dec2[j], df[,length(df)])),]
    outLst2[[j]]=intersect(rownames(df2),outLst[[j]])
    } 
}else{
outLst=list()
outLst2=list()
for(j in 1:dim(rls)[1]){

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
    vec3=more2Vec(as.data.frame(df[,which(feats %in% rl2[[j]])])[,i],as.numeric(cuts[j,][cndsCS[i]]))
    
    if(length(vec4)==0)
    {
    vec4=vec3
    }else{
    vec4=vec3 & vec4
    }
  }
  if(cnd2[[j]][i]=="value<cut")
  {
    vec3=less2Vec(as.data.frame(df[,which(feats %in% rl2[[j]])])[,i],as.numeric(cuts[j,][cndsCS[i]]))
    
    if(length(vec4)==0)
    {
      vec4=vec3
    }else{
      vec4=vec3 & vec4
    }
  }
  if(cnd2[[j]][i]=="cut<value<cut")
  {
    vec1=less2Vec(as.data.frame(df[,which(feats %in% rl2[[j]])])[,i],as.numeric(cuts[j,][cndsCS[i]]))
    vec2=more2Vec(as.data.frame(df[,which(feats %in% rl2[[j]])])[,i],as.numeric(cuts[j,][cndsCS[i]-1]))
    vec3=vec1==vec2
    
    if(length(vec4)==0)
    {
      vec4=vec3
    }else{
      vec4=vec3 & vec4
    }
  }
}

outLst[[j]]=rownames(df)[which(vec4)] ##LHS
df2=df[which(grepl(dec2[j], df[,length(df)])),]
outLst2[[j]]=intersect(rownames(df2),outLst[[j]])
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
    
    R1=unname(table(df[,length(df)])[names(table(df[,length(df)]))== as.character(rls$DECISION[i])])
    N=dim(df)[1] 
    R2=N-R1
                # the number of decisions/objects/patients
    C1=newSupportLHS[i]   # LHS Support
    #C2=N-C1                  # total drawn
    #R1=dim(df)[2]            # total hits, number of features
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

#decsFinal= unlist(lapply(rls$DECISION, FUN=function(x) (regmatches(x, gregexpr("(?<=\\().*?(?=\\))", x, perl=T))[[1]])))

decsCounts=table(df[,length(df)])

numClass=rep(0,length(rls$DECISION))
for(i in 1:length(table(df[,length(df)]))){
numClass[which(rls$DECISION==names(table(df[,length(df)]))[i])]<-unname(table(df[,length(df)]))[i]
}

percSuppLHS=round(newSupportLHS/numClass, digits=3)*100                          
percSuppRHS=round(newSupportRHS/numClass, digits=3)*100                          

cutsDF=rls[,which(grepl("CUT_", colnames(rls)))]                           
newDF=data.frame(rls$FEATURES,rls$DECISION,rls$CUTS_COND,cutsDF,objectsPerRuleLHS,objectsPerRuleRHS,newSupportLHS,newSupportRHS,percSuppLHS,percSuppRHS,newAccuracy,PVAL, RISK_PVAL, REL_RISK, CONF_INT)
newDF2=newDF[order(newDF$PVAL),]
colnames(newDF2)<-c("FEATURES","DECISION","CUTS_COND",colnames(cutsDF),"SUPP_SET_LHS","SUPP_SET_RHS","SUPP_LHS","SUPP_RHS","PERC_SUPP_LHS","PERC_SUPP_RHS","ACC_RHS","PVAL", "RISK_PVAL", "REL_RISK", "CONF_INT")
rownames(newDF2)<-NULL
return(newDF2)
}

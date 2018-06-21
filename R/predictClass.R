predictClass<-function(df, rls, discrete=F, normalize=T, normalizeMethod="scalar", validate=F, valiDec)
  {

discretized=discrete
dec2=as.character(rls$DECISION)
decs2=unique(dec2)
objs=rownames(df)
feats=colnames(df)
cuts=rls[,grep('CUT_', colnames(rls), value=TRUE)]
less2Vec<-function(x,y){ (x-y)<=0}
more2Vec<-function(x,y){ (x-y)>=0}
eqal2Vec<-function(x,y){ (x-y)==0}

#pb = txtProgressBar(min = 0, max = (dim(df)[1])*(dim(rls)[1]), initial = 0, style=3)
#stepi=0

if(discretized)
{
  
  dfn=df
  outVotes=c()
  

  calcClass<-function(rls3){
    outLst=list()
    outLst2=list()
    rls=rls3
    rl2=strsplit(as.character(rls$FEATURES),",",fixed = T)
    cnd2=strsplit(as.character(rls$CUTS_COND),",",fixed = T)
    
    
    for(k in 1:dim(df)[1]){
      #k=1
      vecObj=dfn[k,]
      for(j in 1:dim(rls)[1]){
        #j=1
        cnds=cnd2[[j]]
        cnds<-as.numeric(cnds)
        cndsLen=length(cnds)
        
        vec4=c()
        
        
        
        for(i in 1:cndsLen){
          
          
         vec3=eqal2Vec(as.data.frame(vecObj[which(feats %in% rl2[[j]])])[,i],as.numeric(cuts[j,i]))
          
          if(length(vec4)==0)
          {
            vec4=vec3
          }else{
            vec4=vec3 & vec4
          }
        #stepi=stepi+1
        #setTxtProgressBar(pb,stepi)
        }
          

        outLst[[j]]=length(which(vec4))


      }
      outVotes[k]=sum(unlist(outLst))
    }

    return(outVotes)
  }
  
}else{

  dfn=df
  outVotes=c()

  calcClass<-function(rls3){
    outLst=list()
    outLst2=list()
    rls=rls3
    rl2=strsplit(as.character(rls$FEATURES),",",fixed = T)
    cnd2=strsplit(as.character(rls$CUTS_COND),",",fixed = T)
    
    
  for(k in 1:dim(df)[1]){
  #k=1
  vecObj=dfn[k,]
  for(j in 1:dim(rls)[1]){
    #j=1
    cnds=cnd2[[j]]
    cnds[cnds == "value>cut"] <- 1
    cnds[cnds == "value<cut"] <- 1
    cnds[cnds == "cut1<value<cut2"] <- 2
    cnds<-as.numeric(cnds)
    cndsLen=length(cnds)
    cndsCS=cumsum(cnds)
    vec4=c()
    

    
    for(i in 1:cndsLen){
      #i=2
      if(cnd2[[j]][i]=="value>cut")
      {
        vec3=more2Vec(vecObj[which(feats %in% rl2[[j]])][,i],as.numeric(cuts[j,][cndsCS[i]]))
        
        if(length(vec4)==0)
        {
          vec4=vec3
        }else{
          vec4=vec3 & vec4
        }
      }
      if(cnd2[[j]][i]=="value<cut")
      {
        vec3=less2Vec(vecObj[which(feats %in% rl2[[j]])][,i],as.numeric(cuts[j,][cndsCS[i]]))
        
        if(length(vec4)==0)
        {
          vec4=vec3
        }else{
          vec4=vec3 & vec4
        }
      }
      if(cnd2[[j]][i]=="cut<value<cut")
      {
        vec1=less2Vec(vecObj[which(feats %in% rl2[[j]])][,i],as.numeric(cuts[j,][cndsCS[i]]))
        vec2=more2Vec(vecObj[which(feats %in% rl2[[j]])][,i],as.numeric(cuts[j,][cndsCS[i]-1]))
        vec3=vec1==vec2
        
        if(length(vec4)==0)
        {
          vec4=vec3
        }else{
          vec4=vec3 & vec4
        }
      }
    #stepi=stepi+1
    #setTxtProgressBar(pb,stepi)

    }


    outLst[[j]]=length(which(vec4))

 }
  outVotes[k]=sum(unlist(outLst))
  }

    return(outVotes)
  } #end of fun
  

}
#common part
outListVotes=data.frame(rownames(df))
for(i in 1:length(decs2)){
  rls3=rls[which(as.character(rls$DECISION)==decs2[i]),]
  
  #print(decs2[i])
  outListVotes2=data.frame(calcClass(rls3))
  
  if(normalize){
  if(normalizeMethod=="median"){
  outListVotes=data.frame(outListVotes,outListVotes2/median(as.numeric(as.matrix(outListVotes2))))
  }
  
  if(normalizeMethod=="mean"){
    outListVotes=data.frame(outListVotes,outListVotes2/mean(as.numeric(as.matrix(outListVotes2))))
  }
  
  if(normalizeMethod=="max"){
    outListVotes=data.frame(outListVotes,outListVotes2/max(as.numeric(as.matrix(outListVotes2))))
  }
  
  if(normalizeMethod=="scalar"){
    outListVotes=data.frame(outListVotes,as.numeric(as.matrix(outListVotes2))/sqrt(sum(as.numeric(as.matrix(outListVotes2))^2)))
  }
  
  if(normalizeMethod=="rulnum"){
    outListVotes=data.frame(outListVotes,(as.numeric(as.matrix(outListVotes2)))/length(which(as.character(rls$DECISION)==decs2[i])))
  }
  }else{
    outListVotes=data.frame(outListVotes,outListVotes2)
  }



}

if(validate){ ### with validation

  newDecs=decs2[apply(outListVotes[,-1], 1, which.max)]
  #newDecs=gsub("[\\(\\)]", "", regmatches(newDecs, gregexpr("\\(.*?\\)", newDecs)))
  outListVotes=data.frame(outListVotes,newDecs,as.character(valiDec))
  colnames(outListVotes)<-c("OBJECT",decs2,"NEW_DEC","OLD_DEC")
  
  acc=c()
  for(i in 1:length(newDecs))
  {
    acc[i]=c(grepl(as.character(valiDec[i]), newDecs[i]) | grepl(newDecs[i], as.character(valiDec[i])))
  }
  
  return(list(out=outListVotes, accuracy=length(which(acc))/length(acc)))
  
}else
{
  
  newDecs=decs2[apply(outListVotes[,-1], 1, which.max)]
  outListVotes=data.frame(outListVotes,newDecs)
  colnames(outListVotes)<-c("OBJECT",decs2,"NEW_DEC")
  return(list(out=outListVotes))
  
}


}

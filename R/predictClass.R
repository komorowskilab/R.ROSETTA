predictClass<-function(dt, rules, discrete=FALSE, normalize=TRUE, normalizeMethod="rss", validate=FALSE, valiDec){

dec2<-as.character(rules$decision)
decs2<-unique(dec2)
objs<-rownames(dt)
feats<-colnames(dt)
cuts<-rules[,grep('cut', colnames(rules), value=TRUE)][,-1]
less2Vec<-function(x,y){ (x-y)<=0}
more2Vec<-function(x,y){ (x-y)>=0}
eqal2Vec<-function(x,y){ (x-y)==0}

if(discrete)
{
  outVotes=c()

  calcClass<-function(x){
  outLst<-outLst2<-list()
  rules<-x
  rl2<-strsplit(as.character(rules$features),",",fixed = T)
  cnd2<-strsplit(as.character(rules$levels),",",fixed = T)
    
    for(k in 1:dim(dt)[1]){

      vecObj=dt[k,]
      for(j in 1:dim(rules)[1]){

        cnds=cnd2[[j]]
        cnds<-as.numeric(cnds)
        cndsLen=length(cnds)
        vec4=c()
        
        for(i in 1:cndsLen){
         vec3=eqal2Vec(as.data.frame(vecObj[which(feats %in% rl2[[j]])])[,i],as.numeric(cuts[j,i]))
         ifelse(length(vec4)==0, vec4<-vec3, vec4<-vec3 & vec4)
        }
        outLst[[j]]=length(which(vec4))
      }
      outVotes[k]=sum(unlist(outLst))
    }

    return(outVotes)
  }
  
}else{

  dtn=dt
  outVotes=c()

  calcClass<-function(x){
    outLst<-outLst2<-list()
    rules<-x
    rl2<-strsplit(as.character(rules$features),",",fixed = T)
    cnd2<-strsplit(as.character(rules$cuts),",",fixed = T)
    
  for(k in 1:dim(dt)[1]){
  vecObj<-dt[k,]
  for(j in 1:dim(rules)[1]){
    cnds<-cnd2[[j]]
    cnds[cnds == "value>cut"] <- 1
    cnds[cnds == "value<cut"] <- 1
    cnds[cnds == "cut<value<cut"] <- 2
    cnds<-as.numeric(cnds)
    cndsLen<-length(cnds)
    cndsCS<-cumsum(cnds)
    vec4<-c()

    for(i in 1:cndsLen){
      if(cnd2[[j]][i]=="value>cut")
      {
        vec3<-more2Vec(vecObj[which(feats %in% rl2[[j]])][,i],as.numeric(cuts[j,][cndsCS[i]]))
        ifelse(length(vec4)==0, vec4<-vec3, vec4<-vec3 & vec4)
      }
      if(cnd2[[j]][i]=="value<cut")
      {
        vec3<-less2Vec(vecObj[which(feats %in% rl2[[j]])][,i],as.numeric(cuts[j,][cndsCS[i]]))
        ifelse(length(vec4)==0, vec4<-vec3, vec4<-vec3 & vec4)
      }
      if(cnd2[[j]][i]=="cut<value<cut")
      {
        vec1<-less2Vec(vecObj[which(feats %in% rl2[[j]])][,i],as.numeric(cuts[j,][cndsCS[i]]))
        vec2<-more2Vec(vecObj[which(feats %in% rl2[[j]])][,i],as.numeric(cuts[j,][cndsCS[i]-1]))
        vec3<-vec1==vec2
        ifelse(length(vec4)==0, vec4<-vec3, vec4<-vec3 & vec4)
      }
    }
    outLst[[j]]<-length(which(vec4))
 }
  outVotes[k]<-sum(unlist(outLst))
  }
    return(outVotes)
  } #end of fun

}
#common part
outListVotes<-data.frame(rownames(dt))
for(i in 1:length(decs2)){
  rules3<-rules[which(as.character(rules$decision)==decs2[i]),]
  outListVotes2<-data.frame(calcClass(rules3))
  
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
  
  if(normalizeMethod=="rss"){ #root sum square
    outListVotes=data.frame(outListVotes,as.numeric(as.matrix(outListVotes2))/sqrt(sum(as.numeric(as.matrix(outListVotes2))^2)))
  }
  
  if(normalizeMethod=="rulnum"){
    outListVotes=data.frame(outListVotes,(as.numeric(as.matrix(outListVotes2)))/length(which(as.character(rules$decision)==decs2[i])))
  }
  }else{
    outListVotes=data.frame(outListVotes,outListVotes2)
  }

}

if(validate){ ### with validation

  newDecs<-decs2[apply(outListVotes[,-1], 1, which.max)]
  outListVotes<-data.frame(outListVotes,as.character(valiDec),newDecs)
  colnames(outListVotes)<-c("object",decs2,"decision","newDecision")
  acc<-c()
  
  for(i in 1:length(newDecs))
  {
    acc[i]<-c(grepl(as.character(valiDec[i]), newDecs[i]) | grepl(newDecs[i], as.character(valiDec[i])))
  }
  
  return(list(out=outListVotes, accuracy=length(which(acc))/length(acc)))
  
}else{
  
  newDecs<-decs2[apply(outListVotes[,-1], 1, which.max)]
  outListVotes<-data.frame(outListVotes,newDecs)
  colnames(outListVotes)<-c("object",decs2,"newDecision")
  return(list(out=outListVotes))
  
}

}
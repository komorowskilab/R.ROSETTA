randRules <- function(nFeatures=150, nObjects=100, nOutcome=2, nRules=300, maxRuleLength=3, nDiscStates=3){

fs=paste0("F_",1:nFeatures)
ds=paste0("D_",1:nOutcome)
ss=1:nDiscStates

new_rules=c()
support=c()
accuracy=c()
features=c()
decision=c()

for(i in 1:nRules){
rule_len=sample(1:maxRuleLength,1)
rule_body=paste0(paste0(sample(fs,rule_len, replace = TRUE),"(",sample(ss,rule_len, replace = TRUE)),")",collapse=" AND ")

dec=sample(ds,1, replace = TRUE)
new_rules[i]<-paste0("IF ",rule_body," THEN ",dec)
support[i]<-sample(nObjects/nDiscStates,1, replace = TRUE)
accuracy[i]<-round(runif(1, 0.5, 1), 2)

##line by line
decision[i]=dec
features[i]=paste(paste0(sample(fs,rule_len, replace = TRUE),"=",sample(ss,rule_len, replace = TRUE)),collapse=",")
}

rule_tab=data.frame(features, decision, support, accuracy)
rule_tab=rule_tab[order(rule_tab$accuracy, decreasing = T),]

return(list(ruleTab=rule_tab, rulesIT=new_rules))
}

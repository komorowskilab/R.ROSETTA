ruleHeatmap <- function(dt, rules, discrete=FALSE, discreteMethod="EF", ind=15, nbins=3, showClust=TRUE){
  
  r=ind
  
  ftrs=unlist(strsplit(as.character(rules$FEATURES)[r], ","))
  dicl=unlist(strsplit(as.character(rules$DISC_CLASSES)[r], ","))
  perc=unlist(strsplit(as.character(rules$PERC_SUPP_RHS)[r], ","))
  decs=unlist(as.character(rules$DECISION))[r]
  pval=unlist(as.numeric(rules$PVAL))[r]
  if(pval<0.05){
    pvalw="*" 
  }
  if(pval<0.01){
    pvalw="**" 
  }
  if(pval<0.001){
    pvalw="***" 
  }
  if(pval>0.05){
    pvalw="ns" 
  }
  
  objs_supp_rule=unlist(strsplit(as.character(rules$SUPP_SET_RHS)[r], ","))
  objs_class=rownames(dt)[which(dt$decision == decs)]
  objs_restclasses=rownames(dt)[which(dt$decision != decs)]
  
  ## selecting TP, FP, TN objects
  objs_tp = which(rownames(dt) %in% objs_supp_rule)# objs supporting rule
  objs_fp = which(rownames(dt) %in% setdiff(objs_class, objs_supp_rule))# objs not supporting the rule
  objs_tn = which(rownames(dt) %in% objs_restclasses)# the rest of objects
  
  ## choosing frame for rules
  dt2=as.matrix(dt[c(objs_tp, objs_fp, objs_tn), which(colnames(dt) %in% ftrs)])
  
  if(discrete==TRUE){
  }else{
  
  if(discreteMethod=="EW"){
  dmeth="interval"
  }
  
  if(discreteMethod=="EB"){
  dmeth="frequency"
  }  
    
  if (nbins >= 3){
    cols=colorRampPalette(c("olivedrab3", "gray90", "indianred1"))(n = nbins)
  }else
  {
    cols=colorRampPalette(c("olivedrab3", "indianred1"))(n = 2)
  }
  
  for(i in 1:length(ftrs)){
    dt2[,i]=discretize(dt2[,i], method=dmeth, breaks = nbins, labels=1:nbins)
  }
  }
  
  if(showClust){
    
    rf1=(length(objs_tp)+1):(length(objs_tp)+length(objs_fp))
    fit_fp <- kmeans(dt2[rf1,], factorial(length(table(as.matrix(dt2[rf1,])))))
    dt2_2=dt2[rf1,][order(fit_fp$cluster),]
    dt2_2<-dt2_2[order(apply(dt2_2, 1, paste, collapse="")),]
    
    rf2=(length(objs_tp)+length(objs_fp)+1):(length(objs_tp)+length(objs_fp)+length(objs_tn))
    fit_tn <- kmeans(dt2[rf2,], factorial(length(table(as.matrix(dt2[rf2,])))))
    dt2_3=dt2[rf2,][order(fit_tn$cluster),]
    dt2_3<-dt2_3[order(apply(dt2_3, 1, paste, collapse="")),]
    
    rf3=1:length(objs_tp)
    dt2_1=dt2[rf3,]
    
    dt2=rbind(dt2_1,dt2_2,dt2_3)
  }
  
  
  heatmap.2(dt2,
            Rowv=F,
            #Colv=FALSE,
            #margins = c(7,10),
            xlab=paste0(pvalw, " IF ",paste(paste0(ftrs,paste("(",dicl,")",sep="")), collapse =" AND ")," THEN ",decs),
            srtCol=0,
            #lwid=c(0.5,4),
            #lhei=c(1,4),
            cexCol=1.2,
            adjCol=c(0.5,0.5),
            cexRow=0.3,
            sepwidth=c(0.0005,0.5),
            sepcol="black", 
            trace="none",
            dendrogram = "none",
            col=cols,
            key=FALSE,
            density.info="none",
            symkey=FALSE,
            labRow= "",
            rowsep=c(length(objs_tp), length(objs_tp)+length(objs_fp)),
            colsep=1:(length(ftrs)-1),
            RowSideColors = c(rep("gold", length(objs_tp)), rep("sandybrown", length(objs_fp)),rep("dodgerblue", length(objs_tn))))
  
  #list('x'=-0.1,'y'=1.2)
  legend(list('x'=0,'y'=1.2),      # location of the legend on the heatmap plot
         legend = c(paste0("Objects supporting the rule"), paste0("Objects not supporting the rule"), "Object for the remaining classes"), # category labels
         col = c("gold", "sandybrown", "dodgerblue"),  # color key
         lty= 1,             # line style
         lwd = 10,
         cex=.7,
         bty = "n",
         xpd=TRUE
  )
  
  
  
}

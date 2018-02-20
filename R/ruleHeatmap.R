ruleHeatmap <- function(df, rls, ind=1, nbins=3){

rls2=rls
r=ind
  
ftrs=unlist(strsplit(as.character(rls2$FEATURES)[r], ","))
perc=unlist(strsplit(as.character(rls2$PERC_SUPP_RHS)[r], ","))
decs=unlist(as.character(rls2$DECISION))[r]

objs_supp_rule=unlist(strsplit(as.character(rls2$SUPP_SET_RHS)[r], ","))
objs_class=rownames(df)[which(df$decision == decs)]
objs_restclasses=rownames(df)[which(df$decision != decs)]

## selecting TP, FP, TN objects
objs_tp = which(rownames(df) %in% objs_supp_rule)# objs supporting rule
objs_fp = which(rownames(df) %in% setdiff(objs_class, objs_supp_rule))# objs not supporting the rule
objs_tn = which(rownames(df) %in% objs_restclasses)# the rest of objects

## choosing frame for rules
df2=as.matrix(df[c(objs_tp, objs_fp, objs_tn), which(colnames(df) %in% ftrs)])

if (nbins >= 3){
cols=colorRampPalette(c("limegreen", "gray96", "tomato2"))(n = nbins)
}else
{
cols=colorRampPalette(c("limegreen", "tomato2"))(n = 2)
}

for(i in 1:length(ftrs)){
df2[,i]=discretize(df2[,i], method="frequency", categories = nbins, labels=1:nbins)
}


heatmap.2(df2,
          Rowv=FALSE,
          Colv=FALSE,
          xlab=paste0("Support set for class: ",decs),
          srtCol=0,
          lwid=c(0.5,4),
          lhei=c(1,4),
          cexCol=1.2,
          adjCol=c(0.5,0.5),
          cexRow=0.3,
          sepwidth=c(0.05,0.5),
          sepcol="black", 
          trace="none",
          dendrogram = "none",
          col=cols,
          key=FALSE,
          density.info="none",
          symkey=FALSE,
          rowsep=c(length(objs_tp), length(objs_tp)+length(objs_fp)),
          RowSideColors = c(rep("gold", length(objs_tp)), rep("firebrick1", length(objs_fp)),rep("dodgerblue", length(objs_tn))))

#par(xpd=TRUE)
#list('x'=-0.1,'y'=1.2)
legend("topleft",      # location of the legend on the heatmap plot
       legend = c(paste0("Objects supporting ",decs), paste0("Objects not supporting ",decs), "Rest of the objects"), # category labels
       col = c("gold", "firebrick1", "dodgerblue"),  # color key
       lty= 1,             # line style
       lwd = 10,
       cex=.7,
       bty = "n",
       xpd=TRUE,
       inset=c(-0.1,-0.15)
       )

  

}

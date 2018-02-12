ruleHeatmap <- function(df, rls, ind=1){

rls2=rls
r=ind
ftrs=unlist(strsplit(as.character(rls2$FEATURES)[r], ","))
objs=unlist(strsplit(as.character(rls2$SUPP_SET_RHS)[r], ","))
perc=unlist(strsplit(as.character(rls2$PERC_SUPP_RHS)[r], ","))
decs=unlist(as.character(rls2$DECISION))[r]

nob=c(which(rownames(df) %in% objs), which(df$decision != decs))
my_group=as.numeric(as.factor(as.character(df$decision)[nob]))
my_col=colorRampPalette(c("darkgoldenrod1", "deepskyblue"))(n = length(unique(my_group)))[my_group]
  
cols=colorRampPalette(c("limegreen", "gray96", "tomato2"))(n = 100)
df2=as.matrix(df[nob, which(colnames(df) %in% ftrs)])
title=paste0("Support set for class: ",decs,", with RHS = ", length(objs)," (",perc,"%)")
heatmap(df2, Colv=NA, col = cols, scale="column", margins=c(5,5),labRow = NA, main=title, cexCol = 0.9, RowSideColors=my_col)
legend("topright",legend=unique(as.character(df$decision)[nob]),
        col=unique(my_col),fill=unique(my_col), cex=0.9)

}
#' @import pheatmap
#' @import tidyverse
#' @import grid
#' @import gridExtra

clusterRules<-function(training_df,recal,support=7,fontsize=7,show_colnames=FALSE,show_rownames=FALSE,y_lab='Samples'){
  if (!require(pheatmap)) install.packages('pheatmap')
  library(pheatmap)
  
  if (!require(tidyverse)) install.packages('tidyverse')
  library(tidyverse)
  
  if (!require(grid)) install.packages('grid')
  library(grid)
  
  if (!require(gridExtra)) install.packages('gridExtra')
  library(gridExtra)
  
  
  #process data for clustering, i.e. making a matrix with binary values 0 and 1
  dataM <- data.frame(matrix(ncol = length(recal$features[1:nrow(recal)]), nrow = length(row.names(training_df))))
  rownames(dataM)<-rownames(training_df)
  colnames(dataM)<-recal$features
  
  #assigning 1 to objects satisfying rules
  for(i in 1:length(recal$features[1:100])){
    list_of_features<-unlist(strsplit(recal$supportSetLHS[i],split=','))
    for(j in 1:length(list_of_features)){
      if(list_of_features[j] %in% rownames(dataM))(dataM[list_of_features[j],i]<-1)
    }
  }
  
  #replacing not satisfying rules as 0
  dataM[is.na(dataM)] <- 0
  
  #decision variable of the dataset
  decision_var<-names(training_df)[ncol(training_df)]
  ann <- data.frame( eval(parse(text=paste("training_df$", decision_var, sep = ""))))
  colnames(ann) <- 'Decision'
  rownames(ann)<- rownames(dataM)
  ann[] <- lapply( ann, factor)
  newdf<-dataM[rowSums(dataM[])>1,colSums(dataM[])>support]
  a <- filter(ann, rownames(ann) %in% rownames(newdf))
  annoCol <- list(category = unique(ann$Decision))
  setHook("grid.newpage", function() pushViewport(viewport(x=1,y=1,width=0.9, height=0.9, name="vp", just=c("right","top"))), action="prepend")
  print(pheatmap(as.matrix(newdf), annotation_row=a ,main='Clustering of Model Rules',fontsize = fontsize, border_color = 'white',annotation_colors = annoCol,cluster_cols = TRUE,show_rownames = show_rownames, show_colnames = show_colnames, cluster_rows = TRUE,color = c('grey88','gray39'),legend_breaks = c(0,1)))
  setHook("grid.newpage", NULL, "replace")
  grid.text("Rules", y=-0.07, gp=gpar(fontsize=15))
  grid.text(y_lab, x=-0.07, rot=90, gp=gpar(fontsize=15))
}

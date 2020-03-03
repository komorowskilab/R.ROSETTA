plotRule <- function(dt, rules, type="heatmap", discrete=FALSE, ind=15, label=c()){
  
  ftrs <- unlist(strsplit(as.character(rules$features)[ind], ","))
  if(length(label)!=0){
    dicl <- label
  }else{
    dicl <- unlist(strsplit(as.character(rules$levels)[ind], ","))
  }
  
  perc <- unlist(strsplit(as.character(rules$supportRatioRHS)[ind], ","))
  decs <- unlist(as.character(rules$decision))[ind]
  pval <- unlist(as.numeric(rules$pValue))[ind]
  cuts <- as.numeric(rules[which(grepl("cut", colnames(rules)))[-1]][ind,])
  
  if(pval < 0.05){
    pvalw <- "*" 
  }
  if(pval < 0.01){
    pvalw <- "**" 
  }
  if(pval < 0.001){
    pvalw="***" 
  }
  if(pval > 0.05){
    pvalw <- "ns" 
  }
  
  objs_supp_rule <- unlist(strsplit(as.character(rules$supportSetRHS)[ind], ","))
  objs_class <- rownames(dt)[which(as.character(dt[,length(dt)]) == decs)]
  objs_restclasses <- rownames(dt)[which(as.character(dt[,length(dt)]) != decs)]
  
  ## selecting TP, FP, TN objects
  objs_tp <- which(rownames(dt) %in% objs_supp_rule)# objs supporting rule
  objs_fp <- which(rownames(dt) %in% setdiff(objs_class, objs_supp_rule))# objs not supporting the rule
  objs_tn <- which(rownames(dt) %in% objs_restclasses)# the rest of objects
  
  ## choosing frame for rules
  dt2 <- as.matrix(dt[c(objs_tp, objs_fp, objs_tn), which(colnames(dt) %in% ftrs)])

  cols <- colorRampPalette(c("#56B4E9","ghostwhite","#E69F00"))(n=50)
  
  rf1 <- (length(objs_tp)+1):(length(objs_tp)+length(objs_fp))
  dt2_2 <- dt2[order(rowMeans(dt2[rf1,])),]
  
  rf2 <- (length(objs_tp)+length(objs_fp)+1):(length(objs_tp)+length(objs_fp)+length(objs_tn))
  dt2_3 <- dt2[order(rowMeans(dt2[rf2,])),]
  
  rf3 <- 1:length(objs_tp)
  dt2_1 <- dt2[order(rowMeans(dt2[rf3,])),]
    
  dt3 <- rbind(dt2_1,dt2_2,dt2_3)

  
  if(type == "heatmap"){
  heatmap.2(dt3,
            Rowv=F,
            #Colv=FALSE,
            #margins = c(7,10),
            xlab=paste0(pvalw, " IF ",paste(paste0(ftrs,paste("(",dicl,")",sep="")), collapse =" AND ")," THEN ",decs),
            keysize = 1.5,
            srtCol=0,
            scale="column",
            #lwid=c(0.5,4),
            lhei=c(0.4,0.8),
            cexCol=1.2,
            adjCol=c(0.5,0.5),
            cexRow=0.3,
            sepwidth=c(0.0005,0.5),
            sepcol="black", 
            trace="none",
            dendrogram = "none",
            col=cols,
            key=T,
            density.info="none",
            symkey=FALSE,
            labRow= "",
            rowsep=c(length(objs_tp), length(objs_tp)+length(objs_fp)),
            colsep=1:(length(ftrs)-1),
            RowSideColors = c(rep("firebrick1", length(objs_tp)), rep("lightpink2", length(objs_fp)),rep("ivory3", length(objs_tn))))
  
  #list('x'=-0.1,'y'=1.2)
  legend(list('x'=0.2,'y'=1.2),      # location of the legend on the heatmap plot
         legend = c(paste0("Objects supporting the rule"), paste0("Objects not supporting the rule"), "Objects for the remaining classes"), # category labels
         col = c("firebrick1", "lightpink2", "ivory3"),  # color key
         lty= 0,             # line style
         lwd = 5,
         pt.cex=1.5,
         cex=.75,
         pch=15,
         bty = "n",
         xpd=TRUE)
  }else{
    
    dt2 <- scale(dt2)
    par(mfrow = c(1, 3), cex.lab=1.5, cex.axis=1.5)
    cols <- c("cornflowerblue", "lightcoral", "darkorchid1", "gold", "chartreuse3", "tan4", "gray60") #new colours
    boxplot(dt2[rf3,], col = cols[1:length(ftrs)], ylim = c(min(as.numeric(as.matrix(dt2))), max(as.numeric(as.matrix(dt2)))), main="Objects supporting the rule")
    boxplot(dt2[rf1,], col = cols[1:length(ftrs)], ylim = c(min(as.numeric(as.matrix(dt2))), max(as.numeric(as.matrix(dt2)))), main="Objects not supporting the rule")
    boxplot(dt2[rf2,], col = cols[1:length(ftrs)], ylim = c(min(as.numeric(as.matrix(dt2))), max(as.numeric(as.matrix(dt2)))), main="Objects for the remaining classes")
    mtext(paste0(pvalw, " IF ",paste(paste0(ftrs, paste("=",dicl,sep="")), collapse =" AND "), " THEN ", decs), line = -2, side = 1, outer = TRUE, cex = 1.2)
    par(mfrow = c(1, 1), cex.lab=1.5, cex.axis=1.5)
  }
  
  
  
}

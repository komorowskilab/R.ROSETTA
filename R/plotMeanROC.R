plotMeanROC<- function(out, col="orangered3", backCol="snow2")
{
  
  ROCstats <- out$ROCstats
  
  if(is.null(ROCstats)){
    ROCstats <- out$ROC.stats 
  }
  
  ROCstats<-ROCstats[ROCstats$CVNumber<10,]
  
  unstacked_Sens  <- unstack(ROCstats, form = Sensitivity ~ CVNumber)
  unstacked_OMSpec <- unstack(ROCstats, form = OneMinusSpecificity ~ CVNumber)
  
  interpolate_to_253 <- function(x) {
    approx(1:length(x), x, n = 253)$y
  }
  
  # 3. Apply the interpolation to all folds
  Sens_matrix  <- do.call(cbind, lapply(unstacked_Sens, interpolate_to_253))
  OMSpec_matrix <- do.call(cbind, lapply(unstacked_OMSpec, interpolate_to_253))
  
  # 4. Now rowMeans will work perfectly
  Sens  <- rowMeans(Sens_matrix)
  OMSpec <- rowMeans(OMSpec_matrix)
  
  # OMSpec <- rowMeans(unstack(ROCstats, form = OneMinusSpecificity ~ CVNumber))
  #
  # Sens <- rowMeans(unstack(ROCstats, form = Sensitivity ~ CVNumber))
  
  ## plotting with new colours
  plot(OMSpec, Sens, type = "l", lwd=3, col=col, xlab="1 - specificity (FPR)", ylab="sensitivity (TPR)", axes=F, cex.lab=1.5)
  polygon(c(1,OMSpec), c(0,Sens), lwd=0.01, col=backCol)
  text(0.75, 0.1, col=col, paste0("mean AUC = ",round(ros$quality$ROC.AUC.MEAN, digits = 2)),cex = 1.5)
  axis(side=1, at=seq(0,1,0.1))
  axis(side=2, at=seq(0,1,0.1))
  
}

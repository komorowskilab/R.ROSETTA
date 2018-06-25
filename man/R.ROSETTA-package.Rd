\name{R.ROSETTA-package}
\alias{R.ROSETTA-package}
\alias{R.ROSETTA}
\docType{package}
\title{
\packageTitle{R.ROSETTA}
}
\description{
\packageDescription{R.ROSETTA}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{R.ROSETTA}
\packageIndices{R.ROSETTA}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
\packageAuthor{R.ROSETTA}

Maintainer: \packageMaintainer{R.ROSETTA}
}
\references{
A. Ohrn, J. Komorowski (1997), ROSETTA: A Rough Set Toolkit for Analysis of Data, Proc. Third International Joint Conference on Information Sciences, Fifth International Workshop on Rough Sets and Soft Computing (RSSC'97), Durham, NC, USA, March 1-5, Vol. 3, pp. 403-407.
}
\keyword{package}
\seealso{
\link[ROSETTA]{bioinf.icm.uu.se/rosetta}
}
\examples{
  ## Load a sample dataset
  ## you can find a sample dataset under "autcon"
  ## create a model on the default parameters
  resultsRos<-rosetta(autcon)
  
  ## print the accuracy of your model
  print(resultsRos$quality$Accuracy.Mean)
  
  ## assign the rule table for p-value < 0.05
  rules<-resultsRos$main[which(resultsRos$main$PVAL<0.05),]
  
  ## print the features distribution
  print(getFeatures(autcon, rules))
  
  ## print the rule heatmap for the most significant autism rule
  rules2=recalculateRules(autcon, rules)
  ruleHeatmap(autcon, rules2, ind=5, nbins=3, showClust=TRUE)
  
  ## print the support set for the rules
  print(rules2$SUPP_SET_RHS)
  
  ## save the rules to generate networks
  saveLineByLine(rules, "rules.txt")
  
  ## predict decision classes
  decision<-as.character(autcon[,length(autcon)])
  prediction<-predictClass(autcon[,-length(autcon)], rules, discrete=F, normalize=T, normalizeMethod="max", validate=T, decision)
  
  ## print the accuracy of validated model
  print(prediction$accuracy)
}

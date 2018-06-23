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
  ## LOAD sample dataset
  ## you can find a sample dataset under "autcon"
  ## run rosetta on default parameters
  resultsRos<-rosetta(autcon)
  
  ##print the accuracy of your model
  print(resultsRos$quality$Accuracy.Mean)
  
  ## assign the rule table for p-value < 0.05
  rules<-resultsRos$main[which(resultsRos$main$PVAL<0.05),]
  
  ## print the features distribution
  print(getFeatures(autcon,rules))
  
  ## print rule heatmap for the most significant autism rule
  rules2=recalculateRules(autcon, rules)
  ruleHeatmap(autcon, rules2, ind=5, nbins=3, showClust=TRUE)
  
  ## print support set for the rules
  print(rules2$SUPP_SET_RHS)
  
  ## save rules to generate the networks
  saveLineByLine(rules,"rules.txt")
}

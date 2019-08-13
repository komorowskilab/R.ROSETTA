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

  ## The sample dataset exists as autcon variable
  resultsRos<-rosetta(autcon)
  
  ## print the accuracy of your model
  print(resultsRos$quality$accuracyMean)
  
  ## save the rule table for rules with p-value < 0.05
  rules <- resultsRos$main[which(resultsRos$main$pValue < 0.05),]
  
  ## print rules in IF-THEN format
viewRules(rules, setDiscLabels = FALSE, newDiscLabels=c("down", "nochange",  "up"))

}
